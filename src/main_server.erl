-module(main_server).
-behaviour(gen_server).
-export([start_link/1, add_monkey/2, add_bloon/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, 50).

-record(state, {
    num_regions :: integer(),
    all_workers :: list(),
    active_workers :: list(),
    region_map :: map()
}).

start_link(WorkerNodes) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [WorkerNodes], []).
add_monkey(Pos, Range) -> gen_server:cast(?MODULE, {add_monkey, Pos, Range}).
add_bloon(Path, Health) -> gen_server:cast(?MODULE, {add_bloon, Path, Health}).

init([WorkerNodes]) ->
    net_kernel:monitor_nodes(true),
    io:format("Main Server started. Monitoring nodes: ~p~n", [WorkerNodes]),

    lists:foreach(
        fun({Node, RegionId}) ->
            io:format("Starting worker_supervisor on ~p for region ~p~n", [Node, RegionId]),
            rpc:call(Node, worker_supervisor, start_as_root, [RegionId, ?NUM_REGIONS])
        end,
        lists:zip(WorkerNodes, lists:seq(0, ?NUM_REGIONS - 1))
    ),
    timer:sleep(1000),

    RegionMap = build_region_map(WorkerNodes),
    io:format("Main Server: Initial region map created: ~p~n", [RegionMap]),

    {ok, #state{
        num_regions = ?NUM_REGIONS,
        all_workers = WorkerNodes,
        active_workers = nodes(),
        region_map = RegionMap
    }}.

handle_cast({add_monkey, Pos = {X, _Y}, Range}, State = #state{region_map = Map}) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    case maps:get(RegionIndex, Map, not_found) of
        {_Node, Pid} ->
            gen_server:cast(Pid, {spawn_monkey, Pos, Range});
        not_found ->
            io:format("~p: ERROR - Could not find PID for region ~p~n", [node(), RegionIndex])
    end,
    {noreply, State};

handle_cast({add_bloon, Path = [{X, _Y} | _], Health}, State = #state{region_map = Map}) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    case maps:get(RegionIndex, Map, not_found) of
        {_Node, Pid} ->
            AllPids = get_ordered_pids(Map),
            gen_server:cast(Pid, {spawn_bloon, Path, Health, AllPids, RegionIndex});
        not_found ->
            io:format("~p: ERROR - Could not find PID for region ~p~n", [node(), RegionIndex])
    end,
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_info({nodedown, DeadNode}, State = #state{active_workers = Active, region_map = Map, num_regions = NumRegions}) ->
    io:format("!!! Node down: ~p~n", [DeadNode]),
    NewActive = lists:delete(DeadNode, Active),

    case find_region_for_node(DeadNode, Map) of
        {ok, FailedRegionId} ->
            io:format("Region ~p was on the failed node. Attempting recovery.~n", [FailedRegionId]),
            recover_region(FailedRegionId, NewActive, Map, NumRegions, State);
        not_found ->
            io:format("Node ~p went down, but it wasn't hosting a region.~n", [DeadNode]),
            {noreply, State#state{active_workers = NewActive}}
    end;

handle_info({nodeup, NewNode}, State = #state{active_workers = Active}) ->
    io:format("Node up: ~p~n", [NewNode]),
    NewActive = [NewNode | Active],
    {noreply, State#state{active_workers = NewActive}};

handle_info(Info, State) ->
    io:format("Main server received unhandled info: ~p~n", [Info]),
    {noreply, State}.

%% --- Helper Functions ---

recover_region(FailedRegionId, [], _Map, _NumRegions, State) ->
    io:format("!!! No active workers left! Cannot recover region ~p~n", [FailedRegionId]),
    {noreply, State};
recover_region(FailedRegionId, [NewHostNode | _], Map, NumRegions, State) ->
    io:format("Moving region ~p to new host ~p~n", [FailedRegionId, NewHostNode]),
    
    % This assumes worker_supervisor can start a region on its own.
    % We may need to create a new function in worker_supervisor for this.
    rpc:call(NewHostNode, worker_supervisor, start_as_root, [FailedRegionId, NumRegions]),
    
    RegionName = list_to_atom("region_" ++ integer_to_list(FailedRegionId)),
    case get_remote_pid({RegionName, NewHostNode}, 10) of
        {ok, NewPid} ->
            io:format("Successfully started new region server for ~p on ~p with pid ~p~n", [FailedRegionId, NewHostNode, NewPid]),
            NewMap = Map#{FailedRegionId => {NewHostNode, NewPid}},
            broadcast_pids(NewMap),
            NewState = State#state{region_map = NewMap, active_workers = [NewHostNode | State#state.active_workers]},
            {noreply, NewState};
        {error, Reason} ->
            io:format("!!! Failed to start new region server for ~p: ~p~n", [FailedRegionId, Reason]),
            {noreply, State}
    end.

build_region_map(WorkerNodes) ->
    lists:foldl(
        fun({Node, RegionId}, AccMap) ->
            RegionName = list_to_atom("region_" ++ integer_to_list(RegionId)),
            case get_remote_pid({RegionName, Node}, 10) of
                {ok, Pid} -> AccMap#{RegionId => {Node, Pid}};
                {error, _} ->
                    io:format("!!! Could not find region ~p on node ~p during init~n", [RegionName, Node]),
                    AccMap
            end
        end,
        #{},
        lists:zip(WorkerNodes, lists:seq(0, ?NUM_REGIONS - 1))
    ).

get_remote_pid(_NameNode, 0) ->
    {error, timeout};
get_remote_pid({Name, Node} = NameNode, Retries) ->
    case rpc:call(Node, erlang, whereis, [Name]) of
        undefined ->
            timer:sleep(500),
            get_remote_pid(NameNode, Retries - 1);
        Pid when is_pid(Pid) ->
            {ok, Pid}
    end.

find_region_for_node(Node, Map) ->
    Found = maps:filter(fun(_, {N, _}) -> N == Node end, Map),
    case maps:to_list(Found) of
        [{RegionId, _}] -> {ok, RegionId};
        [] -> not_found
    end.

get_ordered_pids(RegionMap) ->
    Sorted = lists:sort(maps:to_list(RegionMap)),
    [Pid || {_, {_, Pid}} <- Sorted].

broadcast_pids(RegionMap) ->
    Pids = get_ordered_pids(RegionMap),
    RunningPids = [Pid_ || {_, {_, Pid_}} <- maps:to_list(RegionMap)],
    io:format("Broadcasting new PID list: ~p~n", [Pids]),
    lists:foreach(
        fun(RunningPid) ->
            gen_server:cast(RunningPid, {update_region_pids, Pids})
        end,
        RunningPids
    ).