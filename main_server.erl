-module(main_server).
-behaviour(gen_server).

-export([start_link/0, add_monkey/2, add_bloon/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(MAP_WIDTH, 200).
-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, ?MAP_WIDTH / ?NUM_REGIONS).

-record(state, {
    regions % List of region server PIDs
}).

%% API Functions
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% FIX: Simplified the function head to remove the unused variable warning.
% The pattern matching is only needed in the handle_cast clause.
add_monkey(Pos, Range) ->
    gen_server:cast(?MODULE, {add_monkey, Pos, Range}).

add_bloon(Path, Health) ->
    gen_server:cast(?MODULE, {add_bloon, Path, Health}).

%% gen_server Callbacks
init([]) ->
    io:format("Main Server is starting...~n"),
    ets:new(game_data, [set, public, named_table]),

    RegionPids = lists:map(fun(I) ->
        StartX = I * ?REGION_WIDTH,
        EndX = StartX + ?REGION_WIDTH - 1,
        {ok, Pid} = region_server:start_link(I, StartX, EndX),
        Pid
    end, lists:seq(0, ?NUM_REGIONS - 1)),

    io:format("All region servers are up.~n"),
    {ok, #state{regions = RegionPids}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% FIX: Grouped all handle_cast/2 clauses together.
handle_cast({add_monkey, Pos = {X, _Y}, Range}, State) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    RegionPid = lists:nth(RegionIndex + 1, State#state.regions),
    monkey:start_link(Pos, Range, RegionPid),
    {noreply, State};

handle_cast({add_bloon, Path = [{X, _Y} | _], Health}, State) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    RegionPid = lists:nth(RegionIndex + 1, State#state.regions),
    bloon:start_link(Path, Health, RegionPid, State#state.regions),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.