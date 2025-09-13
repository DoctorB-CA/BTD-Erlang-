-module(main_server).
-behaviour(gen_server).
-include("dbr.hrl").  % Include database records
-export([start_link/1, add_monkey/3, add_bloon/1, generate_level1/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, 200).

% The state will now hold the actual PIDs of the remote regions.
-record(state, { region_pids = [] }).

start_link(AllNodes) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [AllNodes], []).
add_monkey(Type, Pos, Range) -> gen_server:cast(?MODULE, {add_monkey, Type, Pos, Range}).
add_bloon(Health) -> gen_server:cast(?MODULE, {add_bloon,Health}).

init([AllNodes]) ->
    io:format("Main Server started. Waiting for all regions to report in...~n"),
    
    RegionNameNodes = lists:zipwith(
        fun(Id, Node) -> {list_to_atom("region_" ++ integer_to_list(Id)), Node} end,
        lists:seq(0, ?NUM_REGIONS - 1),
        AllNodes
    ),

    % Use our new helper function to reliably get the PIDs, retrying a few times.
    RegionPids = [get_remote_pid(NameNode, 10) || NameNode <- RegionNameNodes],

    io:format("Main Server: All regions are up and running with PIDs: ~p~n", [RegionPids]),
    
    % Start timer for GUI updates - balanced for performance and smoothness
    timer:send_interval(150, self(), update_gui_balloons),
    
    {ok, #state{region_pids = RegionPids}}.


handle_cast({add_monkey, Type, Pos = {X, Y}, Range}, State = #state{region_pids = Pids}) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    RegionPid = lists:nth(RegionIndex + 1, Pids),
    io:format("~p: Routing 'add_monkey' to region PID ~p~n", [node(), RegionPid]),
    case is_pid(RegionPid) of
        true -> 
            gen_server:cast(RegionPid, {spawn_monkey, Type, Pos, Range}),
            gui:add_monkey(Type,X,Y,erlang:make_ref());
        false -> io:format("~p: ERROR - Invalid PID for region ~p~n", [node(), RegionIndex])
    end,
    {noreply, State};


handle_cast({place_item,{MT,X,Y}}, State = #state{region_pids = Pids}) ->
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
            gen_server:cast(RegionPid, {spawn_monkey, MT, {X,Y}, 80});
        false -> 
            io:format("~p: ERROR - Invalid PID for region ~p~n", [node(), RegionIndex])
    end,
    {noreply, State};


handle_cast({add_bloon, Health}, State = #state{region_pids = Pids}) ->
    X = 0,
    RegionIndex = trunc(X / ?REGION_WIDTH),
    RegionPid = lists:nth(RegionIndex + 1, Pids),
    % This is now much simpler and correct. We already have the PIDs.
    gen_server:cast(RegionPid, {spawn_bloon, Health, Pids, RegionIndex}),
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_info(update_gui_balloons, State) ->
    % Get all bloons from database and update GUI
    AllBloons = db:get_bloons_in_regions([0, 1, 2, 3]), % Get from all regions
    
    % Convert bloon records to GUI format
    BalloonMap = lists:foldl(
    fun(BloonRecord, Acc) ->
        #bloon{id=Id, pos=Pos} = BloonRecord,
        maps:put(Id, {red, Pos}, Acc)
    end,
    #{},
    AllBloons
),
    
    % Send batch update to GUI
    
    gui:update_balloons(BalloonMap),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.


%% --- HELPER FUNCTION ---
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
    %% Step 8: Test the System by adding a bloon
    main_server:add_bloon(5),
    timer:sleep(500), % cooldown
    main_server:add_bloon(5),
    timer:sleep(500), % cooldown
    main_server:add_bloon(5),
    timer:sleep(500), % cooldown
    main_server:add_bloon(5),
    timer:sleep(500), % cooldown
    main_server:add_bloon(5).

