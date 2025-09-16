-module(region_server).
-behaviour(gen_server).

-include("dbr.hrl").

-export([start_link/3, init/1, handle_call/3, handle_cast/2]).

% The state no longer needs to track bloons. It only needs its own ID.
-record(region_state, {id, total_regions}).

start_link(Id, TotalRegions, EndX) ->
    RegionName = list_to_atom("region_" ++ integer_to_list(Id)),
    gen_server:start_link({local, RegionName}, ?MODULE, [Id, TotalRegions, EndX], []).

init([Id, TotalRegions, _EndX]) ->
    io:format("*DEBUG* Region Server ~p started on node ~p.~n", [Id, node()]),
    {ok, #region_state{id = Id, total_regions = TotalRegions}}.

handle_call({find_bloon, MonkeyPos, Range}, _From, State = #region_state{id = MyId, total_regions = Total}) ->
    % Determine which regions to scan (self, previous, and next)
    RegionsToScan = lists:usort([
        MyId,
        if MyId > 0 -> MyId - 1; true -> MyId end,
        if MyId < Total - 1 -> MyId + 1; true -> MyId end
    ]),
    
    % Fetch bloons from the database for all relevant regions
    BloonRecords = db:get_bloons_in_regions(RegionsToScan),
    
    % Find the closest bloon from the fetched records
    Closest = find_closest_bloon(MonkeyPos, Range, BloonRecords, none),
    
    Reply = case Closest of
        none -> {error, not_found};
        {_Dist, BloonId} -> {ok, BloonId}
    end,
    {reply, Reply, State};

handle_call(ping, _From, State) -> {reply, self(), State}.

handle_cast({spawn_monkey,MT, Pos, Range}, State = #region_state{id = RegionId}) ->
    io:format("~p*DEBUG* ~p: Spawning monkey.~n", [MT,node()]),
    monkey:start_link(MT,Pos, Range, self(), RegionId),
    {noreply, State};

handle_cast({spawn_bloon, Health, AllRegionPids, _RegionId}, State) ->
    io:format("*DEBUG* ~p: Spawning bloon with health ~p.~n", [node(), Health]),
    bloon:start_link(Health, self(), AllRegionPids),
    {noreply, State};

handle_cast({spawn_bloon_migration, Health, Index, Pos, AllRegionPids, _RegionId, OriginalBloonId}, State) ->
    % Check if balloon already exists on this node to avoid duplication
    case global:whereis_name(OriginalBloonId) of
        undefined ->
            % Safe to spawn new balloon
            io:format("*DEBUG* Region spawning migrated balloon ~p at pos ~p~n", [OriginalBloonId, Pos]),
            bloon:start_link_migration(Health, Index, Pos, self(), AllRegionPids, OriginalBloonId);
        ExistingPid ->
            % Balloon already exists, check if it's on a different node
            case node(ExistingPid) =:= node() of
                true ->
                    io:format("*DEBUG* Balloon ~p already exists on this node, skipping spawn~n", [OriginalBloonId]);
                false ->
                    io:format("*DEBUG* Balloon ~p exists on different node ~p, waiting for migration~n", [OriginalBloonId, node(ExistingPid)]),
                    % Wait a bit and try again
                    timer:apply_after(100, gen_server, cast, [self(), {spawn_bloon_migration, Health, Index, Pos, AllRegionPids, _RegionId, OriginalBloonId}])
            end
    end,
    {noreply, State};

handle_cast({balloon_reached_end, BloonId}, State = #region_state{id = RegionId}) ->
    io:format("-------------------region----------------------~n"),
    io:format("*DEBUG* Region ~p: Balloon ~p reached end! Notifying main_server~n", [RegionId, BloonId]),
    io:format("*DEBUG* Current node: ~p~n", [node()]),
    
    % Use global registration to find main_server
    case global:whereis_name(main_server) of
        undefined ->
            io:format("*ERROR* main_server not found in global registry!~n");
        MainServerPid ->
            io:format("*DEBUG* Found main_server globally: ~p on node ~p~n", [MainServerPid, node(MainServerPid)]),
            try
                gen_server:cast(MainServerPid, {game_over, BloonId}),
                io:format("*DEBUG* main_server notified about game over~n")
            catch
                Error:Reason ->
                    io:format("*ERROR* Failed to notify main_server: ~p:~p~n", [Error, Reason])
            end
    end,
    io:format("-------------------region----------------------~n"),
    {noreply, State};

handle_cast({balloon_destroyed, BloonId, OriginalHealth}, State = #region_state{id = RegionId}) ->
    io:format("*DEBUG* Region ~p: Balloon ~p destroyed! Original health: ~p~n", [RegionId, BloonId, OriginalHealth]),
    
    % Use global registration to find main_server
    case global:whereis_name(main_server) of
        undefined ->
            io:format("*ERROR* main_server not found in global registry!~n");
        MainServerPid ->
            io:format("*DEBUG* Found main_server globally: ~p on node ~p~n", [MainServerPid, node(MainServerPid)]),
            try
                gen_server:cast(MainServerPid, {balloon_destroyed, BloonId, OriginalHealth}),
                io:format("*DEBUG* main_server notified about balloon destruction~n")
            catch
                Error:Reason ->
                    io:format("*ERROR* Failed to notify main_server about balloon destruction: ~p:~p~n", [Error, Reason])
            end
    end,
    {noreply, State};

handle_cast(restart_cleanup, State = #region_state{id = RegionId}) ->
    io:format("*DEBUG* Region ~p: Received restart_cleanup - terminating all processes~n", [RegionId]),
    
    try
        % Kill all globally registered balloons from ALL nodes
        io:format("*DEBUG* Region ~p: Cleaning up globally registered balloons~n", [RegionId]),
        GlobalNames = global:registered_names(),
        GlobalKillCount = lists:foldl(fun(Name, Count) ->
            case global:whereis_name(Name) of
                Pid when is_pid(Pid) ->
                    % Check if this looks like a balloon process by its registration name
                    case is_reference(Name) of  % Balloon IDs are references
                        true ->
                            try
                                io:format("*DEBUG* Region ~p: Killing global balloon ~p on node ~p~n", 
                                         [RegionId, Name, node(Pid)]),
                                global:unregister_name(Name),
                                exit(Pid, restart_cleanup),
                                Count + 1
                            catch
                                _:_ -> Count % Ignore errors if process already dead
                            end;
                        false -> Count
                    end;
                undefined -> Count
            end
        end, 0, GlobalNames),
        
        % Kill all locally registered processes (monkeys, darts, etc.)
        io:format("*DEBUG* Region ~p: Killing locally registered processes~n", [RegionId]),
        LocalNames = registered(),
        LocalPids = lists:filtermap(fun(Name) ->
            case whereis(Name) of
                Pid when is_pid(Pid) ->
                    % Skip essential system processes
                    case lists:member(Name, [init, error_logger, application_controller, 
                                           kernel_sup, code_server, file_server_2, 
                                           standard_error, user, region_server, 
                                           worker_supervisor, gui]) of
                        false -> {true, Pid};
                        true -> false
                    end;
                undefined -> false
            end
        end, LocalNames),
        
        LocalKillCount = lists:foldl(fun(Pid, Count) ->
            try
                io:format("*DEBUG* Region ~p: Killing local process ~p~n", [RegionId, Pid]),
                exit(Pid, kill),
                Count + 1
            catch
                _:_ -> Count % Ignore errors if process already dead
            end
        end, 0, LocalPids),
        
        % Additional selective cleanup: find all processes running bloon or monkey modules
        io:format("*DEBUG* Region ~p: Performing selective cleanup of bloon/monkey processes~n", [RegionId]),
        AllPids = erlang:processes(),
        
        lists:foreach(fun(Pid) ->
            try
                case node(Pid) =:= node() andalso Pid =/= self() of
                    true ->
                        case process_info(Pid, initial_call) of
                            {initial_call, {Module, _, _}} when Module =:= bloon; Module =:= monkey; Module =:= arrow ->
                                io:format("*DEBUG* Region ~p: Killing game process ~p (module: ~p)~n", [RegionId, Pid, Module]),
                                exit(Pid, kill);
                            {initial_call, {erlang, apply, _}} ->
                                % This might be a dart process or other game-related spawned process
                                % Check if it has game-related information in its process dictionary
                                case process_info(Pid, dictionary) of
                                    {dictionary, Dict} ->
                                        HasGameInfo = lists:any(fun({Key, Val}) ->
                                            case {Key, Val} of
                                                {'$initial_call', {dart, _, _}} -> true;
                                                {'$initial_call', {arrow, _, _}} -> true;
                                                {'$initial_call', {bloon, _, _}} -> true;
                                                {'$initial_call', {monkey, _, _}} -> true;
                                                _ -> false
                                            end
                                        end, Dict),
                                        case HasGameInfo of
                                            true ->
                                                io:format("*DEBUG* Region ~p: Killing game-related process ~p~n", [RegionId, Pid]),
                                                exit(Pid, kill);
                                            false -> ok
                                        end;
                                    undefined -> ok
                                end;
                            _ -> ok
                        end;
                    false -> ok
                end
            catch
                _:_ -> ok
            end
        end, AllPids),
        
        io:format("*DEBUG* Region ~p: Cleanup completed - killed ~p global and ~p local processes~n", 
                 [RegionId, GlobalKillCount, LocalKillCount])
    catch
        Error:Reason ->
            io:format("*ERROR* Region ~p: Error during cleanup: ~p:~p~n", [RegionId, Error, Reason])
    end,
    {noreply, State}.

% Helper functions
distance({X1, Y1}, {X2, Y2}) -> math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

find_closest_bloon(_, _, [], Closest) -> Closest;
find_closest_bloon(MonkeyPos, Range, [Bloon | Rest], Closest) ->
    #bloon{id=BloonId, health=_Health, index=_Idx, pos=BloonPos, region_id=_RegionId} = Bloon,
    Dist = distance(MonkeyPos, BloonPos),
    NewClosest = if
        Dist =< Range ->
            case Closest of
                none -> {Dist, BloonId};
                {ClosestDist, _} when Dist < ClosestDist -> {Dist, BloonId};
                _ -> Closest
            end;
        true ->
            Closest
    end,
    find_closest_bloon(MonkeyPos, Range, Rest, NewClosest).