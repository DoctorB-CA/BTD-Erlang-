-module(main_server).
-behaviour(gen_server).
-export([start_link/1, add_monkey/2, add_bloon/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, 50).

% The state will now hold the actual PIDs of the remote regions.
-record(state, { region_pids = [] }).

start_link(AllNodes) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [AllNodes], []).
add_monkey(Pos, Range) -> gen_server:cast(?MODULE, {add_monkey, Pos, Range}).
add_bloon(Path, Health) -> gen_server:cast(?MODULE, {add_bloon, Path, Health}).

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
    {ok, #state{region_pids = RegionPids}}.


handle_cast({add_monkey, Pos = {X, Y}, Range}, State = #state{region_pids = Pids}) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    RegionPid = lists:nth(RegionIndex + 1, Pids),
    io:format("~p: Routing 'add_monkey' to region PID ~p~n", [node(), RegionPid]),
    case is_pid(RegionPid) of
        true -> 
            gen_server:cast(RegionPid, {spawn_monkey, Pos, Range}),
            gui:add_monkey(ground_monkeyT,X,Y,bob);
        false -> io:format("~p: ERROR - Invalid PID for region ~p~n", [node(), RegionIndex])
    end,
    {noreply, State};

handle_cast({add_bloon, Path = [{X, _Y} | _], Health}, State = #state{region_pids = Pids}) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    RegionPid = lists:nth(RegionIndex + 1, Pids),
    % This is now much simpler and correct. We already have the PIDs.
    gen_server:cast(RegionPid, {spawn_bloon, Path, Health, Pids, RegionIndex}),
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.


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