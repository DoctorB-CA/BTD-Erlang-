-module(main_server).
-behaviour(gen_server).

-export([start_link/1, add_monkey/2, add_bloon/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(MAP_WIDTH, 200).
-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, ?MAP_WIDTH / ?NUM_REGIONS).

-record(state, { regions }). % regions is a list of {Node, Pid}

start_link(RegionNodes) when is_list(RegionNodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [RegionNodes], []).

add_monkey(Pos, Range) ->
    gen_server:cast(?MODULE, {add_monkey, Pos, Range}).

add_bloon(Path, Health) ->
    gen_server:cast(?MODULE, {add_bloon, Path, Health}).

init([RegionNodes]) ->
    io:format("Main Server is starting...~n"),
    ets:new(game_data, [set, public, named_table]),
    RegionArgs = lists:map(fun(I) ->
        {I, I * ?REGION_WIDTH, I * ?REGION_WIDTH + ?REGION_WIDTH - 1}
    end, lists:seq(0, ?NUM_REGIONS - 1)),
    NodeArgs = lists:zip(RegionNodes, RegionArgs),
    RegionInfo = lists:map(fun({Node, {Id, StartX, EndX}}) ->
        io:format("Main Server: Requesting node ~p to start region ~p...~n", [Node, Id]),
        case rpc:call(Node, region_server, start_remotely, [Id, StartX, EndX]) of
            {ok, Pid} -> {Node, Pid};
            Error -> erlang:error({failed_to_start_remote_node, {Node, Error}})
        end
    end, NodeArgs),
    io:format("Main Server: All start requests sent.~n"),
    {ok, #state{regions = RegionInfo}}.

handle_call(_Request, _From, State) -> {reply, ok, State}.

handle_cast({add_monkey, Pos = {X, _Y}, Range}, State) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    {Node, RegionPid} = lists:nth(RegionIndex + 1, State#state.regions),
    io:format("Main Server: Requesting node ~p to start a monkey...~n", [Node]),
    rpc:call(Node, monkey, start_remotely, [Pos, Range, RegionPid]),
    {noreply, State};

handle_cast({add_bloon, Path = [{X, _Y} | _], Health}, State) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    % FIX: Get the Node for the bloon's starting region
    {Node, RegionPid} = lists:nth(RegionIndex + 1, State#state.regions),
    % Get a list of all region PIDs for the bloon to know about
    AllRegionPids = [Pid || {_N, Pid} <- State#state.regions],
    % FIX: Use rpc to start the bloon on the correct remote node
    io:format("Main Server: Requesting node ~p to start a bloon...~n", [Node]),
    rpc:call(Node, bloon, start_remotely, [Path, Health, RegionPid, AllRegionPids]),
    {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.