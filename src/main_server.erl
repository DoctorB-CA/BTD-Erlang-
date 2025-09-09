-module(main_server).
-behaviour(gen_server).

-export([start_link/1, add_monkey/3, add_bloon/2, get_game_state/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("dbr.hrl").
-include_lib("wx/include/wx.hrl").

-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, 200).

% The state will now hold the actual PIDs of the remote regions.
-record(state, { region_pids = [] }).

start_link(AllNodes) -> gen_server:start_link({local, ?MODULE}, ?MODULE, [AllNodes], []).
add_monkey(Pos, Range, Type) -> gen_server:cast(?MODULE, {add_monkey, Pos, Range, Type}).
add_bloon(Path, Type) -> gen_server:cast(?MODULE, {add_bloon, Path, Type}).
get_game_state() -> gen_server:call(?MODULE, get_game_state).

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


handle_cast({add_monkey, Pos = {X, _Y}, Range, Type}, State = #state{region_pids = Pids}) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    RegionPid = lists:nth(RegionIndex + 1, Pids),
    io:format("~p: Routing 'add_monkey' to region PID ~p~n", [node(), RegionPid]),
    case is_pid(RegionPid) of
        true -> gen_server:cast(RegionPid, {spawn_monkey, Pos, Range, Type});
        false -> io:format("~p: ERROR - Invalid PID for region ~p~n", [node(), RegionIndex])
    end,
    {noreply, State};

handle_cast({add_bloon, Path = [{X, _Y} | _], Type}, State = #state{region_pids = Pids}) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    RegionPid = lists:nth(RegionIndex + 1, Pids),
    Health = get_health_from_type(Type),
    gen_server:cast(RegionPid, {spawn_bloon, Path, Type, Health, Pids, RegionIndex}),
    {noreply, State};
handle_cast(_Request, State) -> {noreply, State}.

handle_call(get_game_state, _From, State) ->
    {ok, Monkeys} = db:get_all_monkeys(),
    {ok, Bloons} = db:get_all_bloons(),
    Reply = #{monkeys => Monkeys, bloons => Bloons},
    {reply, Reply, State};
handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_info(_Info, State) -> {noreply, State}.


%% --- HELPER FUNCTIONS ---

get_health_from_type(black_bloon) -> 4;
get_health_from_type(green_bloon) -> 3;
get_health_from_type(blue_bloon) -> 2;
get_health_from_type(red_bloon) -> 1;
get_health_from_type(_) -> 1.

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