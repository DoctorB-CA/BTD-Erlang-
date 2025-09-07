-module(db).
-export([init/1]).
-export([write_bloon/1, delete_bloon/1, write_monkey/1, delete_monkey/1]).
-export([get_bloons_in_regions/1]).

-include("dbr.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Fetches all bloon records for a given list of region IDs.
get_bloons_in_regions(RegionIds) ->
    F = fun() ->
        lists:flatmap(
            fun(Id) ->
                Pattern = #bloon{region_id = Id, _ = '_'},
                mnesia:match_object(bloon, Pattern, read)
            end,
            RegionIds
        )
    end,
    case mnesia:transaction(F) of
        {atomic, Result} -> Result;
        {aborted, Reason} ->
            io:format("*ERROR* DB transaction aborted in get_bloons_in_regions: ~p~n", [Reason]),
            []
    end.

%% @doc Persists a bloon record to the database.
write_bloon(BloonRecord = #bloon{}) ->
    F = fun() -> mnesia:write(bloon, BloonRecord, write) end,
    mnesia:transaction(F).

%% @doc Deletes a bloon record from the database.
delete_bloon(BloonId) ->
    Oid = {bloon, BloonId},
    F = fun() -> mnesia:delete(Oid) end,
    mnesia:transaction(F).

%% @doc Persists a monkey record to the database.
write_monkey(MonkeyRecord = #monkey{}) ->
    F = fun() -> mnesia:write(monkey, MonkeyRecord, write) end,
    mnesia:transaction(F).

%% @doc Deletes a monkey record from the database.
delete_monkey(MonkeyId) ->
    Oid = {monkey, MonkeyId},
    F = fun() -> mnesia:delete(Oid) end,
    mnesia:transaction(F).


%% ===================================================================
%% Setup Functions
%% ===================================================================

%% @doc
%% This is called from the main node's shell to initialize the entire distributed DB.
%% It ensures a clean start, creates the schema, starts Mnesia on all nodes,
%% creates the tables, and waits for them to be ready everywhere.
init(AllNodes) ->
    MainNode = node(),
    WorkerNodes = AllNodes -- [MainNode],

    io:format("DB: Stopping Mnesia on all nodes to ensure a clean start.~n"),
    rpc:multicall(AllNodes, mnesia, stop, []),
    timer:sleep(1000), % Give it a moment to stop

    io:format("DB: Deleting old schema on main node.~n"),
    mnesia:delete_schema([MainNode]), % Only delete on the disc node
    timer:sleep(1000),

    io:format("DB: Creating Mnesia schema on main node: ~p~n", [MainNode]),
    case mnesia:create_schema([MainNode]) of
        ok -> ok;
        {error, {MainNode, {already_exists, MainNode}}} ->
            io:format("DB: Schema already exists on ~p, which is okay.~n", [MainNode]),
            ok;
        {error, CreateReason} ->
            io:format("DB: Error creating schema: ~p~n", [CreateReason]),
            exit({schema_creation_failed, CreateReason})
    end,

    io:format("DB: Starting Mnesia on main node...~n"),
    ok = mnesia:start(),

    io:format("DB: Starting Mnesia on worker nodes...~n"),
    rpc:multicall(WorkerNodes, mnesia, start, []),
    timer:sleep(2000), % Give workers time to start up their empty mnesia

    io:format("DB: Main node connecting to worker nodes: ~p~n", [WorkerNodes]),
    case mnesia:change_config(extra_db_nodes, WorkerNodes) of
        {ok, ConnectedNodes} ->
            io:format("DB: Successfully connected to nodes: ~p~n", [ConnectedNodes]);
        {error, ConnectReason} ->
            io:format("DB: Error connecting to worker nodes: ~p~n", [ConnectReason])
    end,
    timer:sleep(1000), % Give time for schema to propagate

    io:format("DB: Creating tables...~n"),
    create_tables(AllNodes),

    io:format("DB: Waiting for tables to be ready on all worker nodes...~n"),
    Results = rpc:multicall(WorkerNodes, mnesia, wait_for_tables, [[bloon, monkey], 20000]),
    io:format("DB: Worker wait_for_tables results: ~p~n", [Results]),

    case lists:all(fun(ok) -> true; (_) -> false end, Results) of
        true ->
            io:format("DB: All workers ready. Main node setup complete.~n"),
            ok;
        false ->
            io:format("*ERROR* Some workers failed to get tables ready. Aborting.~n"),
            {error, {worker_tables_not_ready, Results}}
    end.

%% @private
%% Defines and creates all Mnesia tables.
create_tables(AllNodes) ->
    MainNode = node(),
    WorkerNodes = AllNodes -- [MainNode],

    % Define the table properties.
    BloonProps = [
        {disc_copies, [MainNode]},
        {ram_copies, WorkerNodes},
        {attributes, record_info(fields, bloon)}
    ],
    MonkeyProps = [
        {disc_copies, [MainNode]},
        {ram_copies, WorkerNodes},
        {attributes, record_info(fields, monkey)}
    ],

    % Create bloon table. This is already a transaction.
    case mnesia:create_table(bloon, BloonProps) of
        {atomic, ok} ->
            io:format("DB: Table 'bloon' created successfully.~n");
        {aborted, {already_exists, bloon}} ->
            io:format("DB: Table 'bloon' already exists.~n");
        {aborted, Reason1} ->
            io:format("DB: Error creating 'bloon' table: ~p~n", [Reason1])
    end,

    % Create monkey table. This is already a transaction.
    case mnesia:create_table(monkey, MonkeyProps) of
        {atomic, ok} ->
            io:format("DB: Table 'monkey' created successfully.~n");
        {aborted, {already_exists, monkey}} ->
            io:format("DB: Table 'monkey' already exists.~n");
        {aborted, Reason2} ->
            io:format("DB: Error creating 'monkey' table: ~p~n", [Reason2])
    end,

    io:format("DB: Waiting for tables to be loaded on main node...~n"),
    mnesia:wait_for_tables([bloon, monkey], 20000).
