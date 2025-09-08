-module(main_server).
-behaviour(gen_server).

-export([start_link/0, start_game/0]). % Updated exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, 50).

-record(state, {
    num_regions :: integer(),
    active_workers :: list(),
    region_pids :: list() % List of PIDs for region_servers
}).

% The supervisor calls this function
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% The user calls this from the shell
start_game() ->
    gen_server:cast(?MODULE, start_game).

init([]) ->
    io:format("Main Server started. Waiting for worker nodes to connect...~n"),
    net_kernel:monitor_nodes(true),
    {ok, #state{
        num_regions = ?NUM_REGIONS,
        active_workers = [],
        region_pids = []
    }}.

handle_cast(start_game, State = #state{active_workers = Workers}) ->
    io:format("Main server: start_game command received.~n"),
    if
        length(Workers) < 1 ->
            io:format("Cannot start game: No worker nodes have connected yet.~n");
        true ->
            spawn_initial_bloons(State)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({nodeup, Node}, State = #state{active_workers = Workers}) ->
    io:format("Node up: ~p~n", [Node]),
    % Tell the DB process to add the new node to the Mnesia schema
    db:add_node_to_schema(Node),
    % Add the new node to our list of active workers
    {noreply, State#state{active_workers = [Node | Workers]}};

handle_info({nodedown, Node}, State = #state{active_workers = Workers}) ->
    io:format("!!! Node down: ~p~n", [Node]),
    % The recovery logic is now handled by region_servers restarting controllers.
    % The main server only needs to update its list of active workers.
    db:remove_node_from_schema(Node),
    {noreply, State#state{active_workers = lists:delete(Node, Workers)}};

handle_info(_Info, State) ->
    {noreply, State}.


%% ===================================================================
%% Internal Functions
%% ===================================================================

spawn_initial_bloons(State = #state{active_workers = Workers}) ->
    io:format("Spawning initial wave of bloons across ~p workers...~n", [length(Workers)]),
    % Define a simple path for the bloons
    Path = [{0, 100}, {50, 100}, {100, 100}, {150, 100}, {200, 100}],
    Health = 10,

    % Create 10 bloons and distribute them
    lists:foreach(
        fun(I) ->
            BloonId = erlang:unique_integer(),
            % Distribute bloons across workers in a round-robin fashion
            WorkerNode = lists:nth(((I-1) rem length(Workers)) + 1, Workers),
            io:format("Creating bloon ~p on node ~p~n", [BloonId, WorkerNode]),
            % Create the bloon record directly in the database via an RPC call
            rpc:call(WorkerNode, game, create_bloon_on_node, [BloonId, Path, Health])
        end,
        lists:seq(1, 10)
    ),
    io:format("Initial bloon wave spawned.~n").