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

handle_cast(start_game, State = #state{active_workers = Workers, num_regions = NumRegions}) ->
    io:format("Main server: start_game command received.~n"),
    if
        length(Workers) < 1 ->
            io:format("Cannot start game: No worker nodes have connected yet.~n");
        true ->
            assign_regions_to_workers(Workers, NumRegions),
            spawn_initial_bloons(State)
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_info({nodeup, Node}, State = #state{active_workers = Workers}) ->
    io:format("Node up: ~p~n", [Node]),
    case db:add_node_to_schema(Node) of
        ok ->
            io:format("Successfully added node ~p to schema. It is now an active worker.~n", [Node]),
            {noreply, State#state{active_workers = [Node | Workers]}};
        {error, Reason} ->
            io:format("Error adding node ~p to schema: ~p. It will not be used as a worker.~n", [Node, Reason]),
            {noreply, State}
    end;

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

assign_regions_to_workers(Workers, NumRegions) ->
    io:format("Assigning ~p regions to ~p workers...~n", [NumRegions, length(Workers)]),
    lists:foreach(
        fun(RegionId) ->
            WorkerNode = lists:nth((RegionId rem length(Workers)) + 1, Workers),
            io:format("Starting region ~p on worker ~p~n", [RegionId, WorkerNode]),
            rpc:call(WorkerNode, worker_root_supervisor, start_worker_supervisor, [])
        end,
        lists:seq(0, NumRegions - 1)
    ).

spawn_initial_bloons(#state{active_workers = Workers}) ->
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