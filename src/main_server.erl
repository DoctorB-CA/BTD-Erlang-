-module(main_server).
-behaviour(gen_server).
-include("dbr.hrl").  % Include database records
-export([start_link/1, add_monkey/3, add_bloon/1, generate_level1/0, game_over/0, restart_game/0, check_and_revive_regions/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, 200).
-define(balloon_cooldown, 1000).

% Monkey costs in bananas
-define(GROUND_MONKEY_COST, 100).
-define(WATER_MONKEY_COST, 150).
-define(FIRE_MONKEY_COST, 200).
-define(AIR_MONKEY_COST, 250).
-define(AVATAR_MONKEY_COST, 500).

% The state will now hold the actual PIDs of the remote regions.
-record(state, { region_pids = [], game_over = false, bananas = 1000 }).

start_link(AllNodes) -> 
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [AllNodes], []) of
        {ok, Pid} ->
            % Register globally so any node can find main_server
            io:format("*DEBUG* Registering main_server globally~n"),
            global:register_name(main_server, Pid),
            {ok, Pid};
        Error ->
            Error
    end.
add_monkey(Type, Pos, Range) -> gen_server:cast(?MODULE, {add_monkey, Type, Pos, Range}).
add_bloon(Health) -> gen_server:cast(?MODULE, {add_bloon,Health}).
game_over() -> gen_server:cast(?MODULE, game_over).
restart_game() -> gen_server:cast(?MODULE, restart_game).

init([AllNodes]) ->
    io:format("Main Server started. Waiting for all regions to report in...~n"),
    
    % Initialize bananas and update GUI
    InitialBananas = 1000,
    gui:change_bananas(InitialBananas),
    
    RegionNameNodes = lists:zipwith(
        fun(Id, Node) -> {list_to_atom("region_" ++ integer_to_list(Id)), Node} end,
        lists:seq(0, ?NUM_REGIONS - 1),
        AllNodes
    ),

    % Use our new helper function to reliably get the PIDs, retrying a few times.
    RegionPids = [get_remote_pid(NameNode, 10) || NameNode <- RegionNameNodes],

    io:format("Main Server: All regions are up and running with PIDs: ~p~n", [RegionPids]),
    
    % Start timer for GUI updates - 30 FPS for smooth visuals without overload
    timer:send_interval(33, self(), update_gui_balloons),
    
    {ok, #state{region_pids = RegionPids, bananas = InitialBananas}}.


handle_cast({game_over, BloonId}, State = #state{game_over = GameOver}) ->
    io:format("*DEBUG* main_server RECEIVED game_over message for balloon ~p~n", [BloonId]),
    case GameOver of
        false ->
            io:format("-------------------main----------------------~n"),
            io:format("*DEBUG* GAME OVER! Balloon ~p reached the end~n", [BloonId]),
            io:format("*DEBUG* Calling gui:lose_game()...~n"),
            io:format("-------------------main----------------------~n"),
            try
                gui:lose_game(),
                io:format("*DEBUG* gui:lose_game() called successfully~n")
            catch
                Error:Reason ->
                    io:format("*ERROR* gui:lose_game() failed: ~p:~p~n", [Error, Reason])
            end,
            {noreply, State#state{game_over = true}};
        true ->
            io:format("*DEBUG* Game already over, ignoring additional balloon ~p end~n", [BloonId]),
            {noreply, State}
    end;

handle_cast(game_over, State = #state{game_over = GameOver}) ->
    case GameOver of
        false ->
            io:format("*DEBUG* GAME OVER! First balloon reached the end~n"),
            gui:lose_game(),
            {noreply, State#state{game_over = true}};
        true ->
            io:format("*DEBUG* Game already over, ignoring additional balloon end~n"),
            {noreply, State}
    end;

handle_cast(restart_game, State = #state{region_pids = RegionPids}) ->
    io:format("*DEBUG* ========== AGGRESSIVE GAME RESTART ==========~n"),
    
    % Phase 1: Immediately kill ALL game processes on the main node
    io:format("*DEBUG* Phase 1: Killing ALL game processes on main node~n"),
    AllPids = erlang:processes(),
    MainNodeKills = lists:foldl(fun(Pid, Count) ->
        try
            case is_process_alive(Pid) of
                true ->
                    case process_info(Pid, initial_call) of
                        {initial_call, {Module, _, _}} when Module =:= bloon; Module =:= monkey ->
                            io:format("*DEBUG* Main: Killing ~p process ~p~n", [Module, Pid]),
                            exit(Pid, kill),
                            Count + 1;
                        _ -> Count
                    end;
                false -> Count
            end
        catch
            _:_ -> Count
        end
    end, 0, AllPids),
    
    % Phase 2: Clear ALL global registrations immediately
    io:format("*DEBUG* Phase 2: Clearing ALL global registrations~n"),
    GlobalNames = global:registered_names(),
    GlobalClears = lists:foldl(fun(Name, Count) ->
        try
            case global:whereis_name(Name) of
                Pid when is_pid(Pid) ->
                    io:format("*DEBUG* Main: Killing and unregistering ~p (~p)~n", [Name, Pid]),
                    exit(Pid, kill),
                    global:unregister_name(Name),
                    Count + 1;
                undefined -> 
                    global:unregister_name(Name), % Clean up stale registrations
                    Count + 1
            end
        catch
            _:_ -> Count
        end
    end, 0, GlobalNames),
    
    % Phase 3: Clear database AFTER killing processes
    io:format("*DEBUG* Phase 3: Clearing database~n"),
    db:db_clear(),
    
    % Phase 4: Notify all region servers to do aggressive cleanup
    io:format("*DEBUG* Phase 4: Notifying regions for aggressive cleanup~n"),
    lists:foreach(fun(RegionPid) ->
        case is_pid(RegionPid) of
            true ->
                gen_server:cast(RegionPid, restart_cleanup),
                io:format("*DEBUG* Sent aggressive cleanup to region ~p~n", [RegionPid]);
            false ->
                io:format("*ERROR* Invalid region PID: ~p~n", [RegionPid])
        end
    end, RegionPids),
    
    % Phase 5: Wait for cleanup and final verification
    timer:sleep(500), % Give more time for thorough cleanup
    
    % Phase 6: Final verification - check for any remaining game processes
    io:format("*DEBUG* Phase 6: Final verification~n"),
    RemainingPids = erlang:processes(),
    RemainingGameProcesses = lists:filter(fun(Pid) ->
        try
            case is_process_alive(Pid) of
                true ->
                    case process_info(Pid, initial_call) of
                        {initial_call, {Module, _, _}} when Module =:= bloon; Module =:= monkey ->
                            io:format("*WARNING* Found remaining game process: ~p (~p)~n", [Module, Pid]),
                            true;
                        _ -> false
                    end;
                false -> false
            end
        catch
            _:_ -> false
        end
    end, RemainingPids),
    
    % Reset bananas and game state
    NewBananas = 1000,
    gui:change_bananas(NewBananas),
    gui:clear_board(), % Immediately clear GUI to show restart
    
    io:format("*DEBUG* ========== RESTART COMPLETE ==========~n"),
    io:format("*DEBUG* Killed ~p main processes, cleared ~p globals, ~p remaining game processes~n", 
             [MainNodeKills, GlobalClears, length(RemainingGameProcesses)]),
    io:format("*DEBUG* Game ready for new balloons and monkeys~n"),
    
    {noreply, State#state{game_over = false, bananas = NewBananas}};

handle_cast({balloon_destroyed, BloonId, OriginalHealth}, State = #state{bananas = CurrentBananas}) ->
    % Award bananas based on balloon's original health
    % Basic reward formula: 5 + (Health * 2) bananas
    BananaReward = 5 + (OriginalHealth * 2),
    NewBananas = CurrentBananas + BananaReward,
    gui:change_bananas(NewBananas),
    io:format("*DEBUG* Balloon ~p destroyed! Health: ~p, +~p bananas (Total: ~p)~n", [BloonId, OriginalHealth, BananaReward, NewBananas]),
    {noreply, State#state{bananas = NewBananas}};

handle_cast({balloon_destroyed, BloonId}, State = #state{bananas = CurrentBananas}) ->
    % Fallback for old format without health info - assume default health of 1
    BananaReward = 5 + (1 * 2), % 7 bananas for default
    NewBananas = CurrentBananas + BananaReward,
    gui:change_bananas(NewBananas),
    io:format("*DEBUG* Balloon ~p destroyed (legacy)! +~p bananas (Total: ~p)~n", [BloonId, BananaReward, NewBananas]),
    {noreply, State#state{bananas = NewBananas}};

handle_cast({add_monkey, Type, Pos = {X, Y}, Range}, State = #state{region_pids = Pids, bananas = CurrentBananas}) ->
    % Check if we have enough bananas for this monkey type
    Cost = get_monkey_cost(Type),
    case CurrentBananas >= Cost of
        true ->
            io:format("*DEBUG* Adding ~p (Cost: ~p, Remaining: ~p)~n", 
                     [Type, Cost, CurrentBananas - Cost]),
            
            RegionIndex = trunc(X / ?REGION_WIDTH),
            RegionPid = lists:nth(RegionIndex + 1, Pids),
            io:format("~p: Routing 'add_monkey' to region PID ~p~n", [node(), RegionPid]),
            case is_pid(RegionPid) of
                true -> 
                    gen_server:cast(RegionPid, {spawn_monkey, Type, Pos, Range}),
                    gui:add_monkey(Type,X,Y,erlang:make_ref());
                false -> 
                    io:format("~p: ERROR - Invalid PID for region ~p~n", [node(), RegionIndex])
            end,
            
            % Deduct bananas and update GUI
            NewBananas = CurrentBananas - Cost,
            gui:change_bananas(NewBananas),
            {noreply, State#state{bananas = NewBananas}};
        false ->
            io:format("*ERROR* Not enough bananas! Need ~p, have ~p for ~p monkey~n", 
                     [Cost, CurrentBananas, Type]),
            {noreply, State}
    end;


handle_cast({place_item,{MT,X,Y}}, State = #state{region_pids = Pids, bananas = CurrentBananas}) ->
    % Check if we have enough bananas for this monkey type
    Cost = get_monkey_cost(MT),
    case CurrentBananas >= Cost of
        true ->
            % We have enough bananas - proceed with placing monkey
            io:format("*DEBUG* Placing ~p (Cost: ~p, Current: ~p, Remaining: ~p)~n", 
                     [MT, Cost, CurrentBananas, CurrentBananas - Cost]),
            
            % Generate a unique ID for the monkey
            I = erlang:make_ref(),
            
            % Add monkey to the GUI first
            gui:add_monkey(MT,X,Y,I),
            
            % Route to the appropriate region to create the monkey FSM
            RegionIndex = trunc(X / ?REGION_WIDTH),
            RegionPid = lists:nth(RegionIndex + 1, Pids),
            io:format("~p: Routing 'place_item' to region PID ~p~n", [node(), RegionPid]),
            case is_pid(RegionPid) of
                true -> 
                    gen_server:cast(RegionPid, {spawn_monkey, MT, {X,Y}, 200});
                false -> 
                    io:format("~p: ERROR - Invalid PID for region ~p~n", [node(), RegionIndex])
            end,
            
            % Deduct bananas and update GUI
            NewBananas = CurrentBananas - Cost,
            gui:change_bananas(NewBananas),
            {noreply, State#state{bananas = NewBananas}};
        false ->
            % Not enough bananas - reject the placement
            io:format("Not enough bananas! Need ~p, have ~p for ~p monkey~n", 
                     [Cost, CurrentBananas, MT]),
            {noreply, State}
    end;


handle_cast({add_bloon, Health}, State = #state{region_pids = Pids}) ->
    X = 0,
    RegionIndex = trunc(X / ?REGION_WIDTH),
    RegionPid = lists:nth(RegionIndex + 1, Pids),
    % This is now much simpler and correct. We already have the PIDs.
    gen_server:cast(RegionPid, {spawn_bloon, Health, Pids, RegionIndex}),
    {noreply, State};

handle_cast({update_region_pid, RegionId, NewPid}, State = #state{region_pids = RegionPids}) ->
    % Update the PID for a revived region
    io:format("*DEBUG* Updating region ~p PID to ~p~n", [RegionId, NewPid]),
    NewRegionPids = update_list_at_index(RegionPids, RegionId + 1, NewPid),
    io:format("*DEBUG* Updated region PIDs: ~p~n", [NewRegionPids]),
    {noreply, State#state{region_pids = NewRegionPids}}.

handle_call(get_region_info, _From, State = #state{region_pids = RegionPids}) ->
    % Return region pids and node info for health checking
    MainNode = node(),
    WorkerNodes = [node(Pid) || Pid <- RegionPids],
    AllNodes = [MainNode | WorkerNodes],
    {reply, {ok, {RegionPids, AllNodes}}, State};

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_info(update_gui_balloons, State) ->
    % SIMPLE APPROACH: Just grab everything and update everything
    % This is the most efficient way for high-frequency updates
    AllBloons = db:get_all_bloons(),  % 1 database query for balloons
    AllDarts = db:get_all_darts(),    % 1 database query for darts
    update_gui_with_objects(AllBloons, AllDarts),  % 1 message to GUI
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%% --- HELPER FUNCTION ---
update_gui_with_objects(AllBloons, AllDarts) ->
    % SIMPLE: Convert all balloons to GUI format in one shot
    BalloonMap = maps:from_list([
        {Id, {red, Pos}} || #bloon{id=Id, pos=Pos} <- AllBloons
    ]),
    % SIMPLE: Convert all darts to GUI format in one shot
    DartMap = maps:from_list([
        {Id, {Type, Pos}} || #dart{id=Id, type=Type, pos=Pos} <- AllDarts
    ]),
    % SIMPLE: Send everything to GUI in separate messages
    gui:update_balloons(BalloonMap),
    gui:update_darts(DartMap).

get_remote_pid(_NameNode, 0) ->
    erlang:error({could_not_find_remote_pid, _NameNode});
get_remote_pid({Name, Node} = NameNode, Retries) ->
    case rpc:call(Node, erlang, whereis, [Name]) of
        undefined ->
            io:format("Region ~p on node ~p not up yet, waiting...~n", [Name, Node]),
            timer:sleep(500),
            get_remote_pid(NameNode, Retries - 1);
        Pid when is_pid(Pid) ->
            Pid
    end.


%% --- genertes levels  ----
generate_level1() ->
    %% Spawn balloons with proper timing to avoid GUI overload
    spawn(fun() ->
        lists:foreach(fun(N) ->
            main_server:add_bloon(1),
            io:format("Spawned balloon ~p/5~n", [N]),
            timer:sleep(700)  % 700ms between balloons
        end, lists:seq(1, 20))
    end).

generate_level2() ->
    %% Spawn balloons with proper timing to avoid GUI overload
    spawn(fun() ->
        lists:foreach(fun(N) ->
            main_server:add_bloon(1),
            io:format("Spawned balloon ~p/5~n", [N]),
            timer:sleep(500)  % 500ms between balloons
        end, lists:seq(1, 40))
    end).

generate_levelinf() ->
    %% Spawn balloons with proper timing to avoid GUI overload
    spawn(fun() ->
        lists:foreach(fun(N) ->
            main_server:add_bloon(1),
            io:format("Spawned balloon ~p/5~n", [N]),
            timer:sleep(500)  % 500ms between balloons
        end, lists:seq(1, 1))
    end).

%% --- Function to check and revive dead regions ---
check_and_revive_regions() ->
    case gen_server:call(?MODULE, get_region_info) of
        {ok, {RegionPids, AllNodes}} ->
            MainNode = node(),
            WorkerNodes = AllNodes -- [MainNode],
            io:format("*DEBUG* Checking health of regions: ~p~n", [RegionPids]),
            io:format("*DEBUG* Worker nodes: ~p~n", [WorkerNodes]),
            
            % Check each region and revive if dead
            RegionData = lists:zip3(RegionPids, lists:seq(0, ?NUM_REGIONS - 1), WorkerNodes),
            lists:foreach(fun({RegionPid, RegionId, Node}) ->
                % First check if the node itself is reachable
                case net_adm:ping(Node) of
                    pong ->
                        % Node is reachable, check if process is alive
                        case is_process_alive(RegionPid) of
                            true ->
                                io:format("*DEBUG* Region ~p (~p) is alive~n", [RegionId, Node]);
                            false ->
                                io:format("*WARNING* Region ~p (~p) process is DEAD but node is reachable - attempting revival~n", [RegionId, Node]),
                                revive_dead_region(RegionId, [Node])
                        end;
                    pang ->
                        io:format("*ERROR* Region ~p (~p) is UNREACHABLE - network disconnected or node down~n", [RegionId, Node]),
                        io:format("*INFO* Cannot revive region ~p until network connectivity is restored~n", [RegionId])
                end
            end, RegionData);
        Error ->
            io:format("*ERROR* Could not get region info: ~p~n", [Error])
    end.

%% Helper function to revive a dead region
revive_dead_region(RegionId, WorkerNodes) ->
    try
        % Attempt to revive the database connection for this region
        case db:revive_region(RegionId, WorkerNodes) of
            ok ->
                io:format("Successfully revived database for region ~p~n", [RegionId]),
                % Try to restart the region supervisor
                try
                    gen_statem:start({global, {region_server, RegionId}}, region_server, [RegionId], []),
                    io:format("Successfully revived region ~p~n", [RegionId])
                catch
                    StartError:StartReason ->
                        io:format("*ERROR* Failed to restart region supervisor for region ~p: ~p:~p~n", [RegionId, StartError, StartReason])
                end;
            {error, DbReason} ->
                io:format("*ERROR* Failed to revive database for region ~p: ~p~n", [RegionId, DbReason])
        end
    catch
        Error:Reason ->
            io:format("*ERROR* Unexpected error reviving region ~p: ~p:~p~n", [RegionId, Error, Reason])
    end.

%% --- Helper function to get monkey costs ---
get_monkey_cost(ground_monkey) -> ?GROUND_MONKEY_COST;
get_monkey_cost(water_monkey) -> ?WATER_MONKEY_COST;
get_monkey_cost(fire_monkey) -> ?FIRE_MONKEY_COST;
get_monkey_cost(air_monkey) -> ?AIR_MONKEY_COST;
get_monkey_cost(avatar_monkey) -> ?AVATAR_MONKEY_COST;
get_monkey_cost(_) -> 0.  % Unknown monkey type costs nothing

%% --- Helper function to update list at specific index ---
update_list_at_index(List, Index, NewValue) ->
    {Before, [_|After]} = lists:split(Index - 1, List),
    Before ++ [NewValue] ++ After.

