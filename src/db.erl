-module(db).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([write_bloon/1, delete_bloon/1, write_monkey/1, delete_monkey/1]).
-export([get_bloons_in_regions/1, wait_for_local_tables/0]).
-export([add_node_to_schema/1, remove_node_from_schema/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("db.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
            io:format("DB Error: transaction aborted in get_bloons_in_regions: ~p~n", [Reason]),
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

%% @doc Adds a new node to the Mnesia schema, making it part of the cluster.
%% This function is robust against race conditions where the node has just
%% connected but is not yet fully recognized by Mnesia.
add_node_to_schema(Node) ->
    add_node_to_schema_retry(Node, 5). % 5 retries, 1 second apart

add_node_to_schema_retry(_Node, 0) ->
    io:format("DB Error: Failed to add node after multiple retries.~n"),
    {error, max_retries_exceeded};
add_node_to_schema_retry(Node, Retries) ->
    io:format("DB: Attempting to add node ~p to schema. Retries left: ~p~n", [Node, Retries-1]),
    % The key is to first make the new node part of the schema management.
    % This operation itself can fail with {no_exists, Node} if Mnesia hasn't
    % yet processed the net_kernel notification about the new node.
    case mnesia:add_table_copy(schema, Node, disc_copies) of
        {atomic, ok} ->
            io:format("DB: Node ~p is now part of the schema. Adding application tables.~n", [Node]),
            % Now that the node is aware of schema changes, we can add our tables.
            % These operations should now find the node.
            mnesia:add_table_copy(bloon, Node, ram_copies),
            mnesia:add_table_copy(monkey, Node, ram_copies),
            ok;
        {aborted, {no_exists, Node}} ->
            io:format("DB Warn: Mnesia doesn't see node ~p yet. Retrying...~n", [Node]),
            timer:sleep(1000),
            add_node_to_schema_retry(Node, Retries - 1);
        {aborted, Reason} ->
            io:format("DB Error: Could not add node ~p to schema: ~p~n", [Node, Reason]),
            {error, Reason}
    end.

%% @doc Blocks until the core application tables are loaded on the local node.
wait_for_local_tables() ->
    wait_for_local_tables(30000). % 30 seconds timeout

wait_for_local_tables(Timeout) ->
    Tables = [bloon, monkey],
    io:format("DB: Waiting for local tables ~p to be loaded...~n", [Tables]),
    case mnesia:wait_for_tables(Tables, Timeout) of
        ok ->
            io:format("DB: All local tables are ready.~n"),
            ok;
        {timeout, _} ->
            io:format("DB Error: timed out waiting for local tables.~n"),
            {error, timeout};
        {error, Reason} ->
            io:format("DB Error: Mnesia error while waiting for tables: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Removes a node from the Mnesia schema.
remove_node_from_schema(Node) ->
    io:format("DB: Removing node ~p from Mnesia schema.~n", [Node]),
    % This will remove the table replicas from the downed node.
    mnesia:del_table_copy(bloon, Node),
    mnesia:del_table_copy(monkey, Node),
    ok.

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init([]) ->
    io:format("DB process starting on main node.~n"),
    % This process only runs on the main node.
    % It's responsible for setting up the Mnesia schema for the whole cluster.
    MainNode = node(),
    mnesia:delete_schema([MainNode]),
    timer:sleep(500),
    case mnesia:create_schema([MainNode]) of
        ok -> ok;
        {error, {MainNode, {already_exists, MainNode}}} -> ok;
        {error, Reason} -> exit({schema_creation_failed, Reason})
    end,
    mnesia:start(),
    create_tables(),
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% This is where we handle new nodes joining the cluster.
handle_info({nodeup, Node, _NodeType}, State) ->
    io:format("DB: Node ~p came up. Adding to Mnesia schema.~n", [Node]),
    add_node_to_schema(Node),
    {noreply, State};

handle_info({nodedown, Node}, State) ->
    io:format("DB: Node ~p went down. It will be removed from the schema if it reconnects.~n", [Node]),
    remove_node_from_schema(Node),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% ===================================================================
%% Internal Setup Functions
%% ===================================================================

create_tables() ->
    MainNode = node(),
    BloonProps = [
        {disc_copies, [MainNode]},
        {attributes, record_info(fields, bloon)}
    ],
    MonkeyProps = [
        {disc_copies, [MainNode]},
        {attributes, record_info(fields, monkey)}
    ],

    case mnesia:create_table(bloon, BloonProps) of
        {atomic, ok} -> io:format("DB: Table 'bloon' created.~n");
        {aborted, {already_exists, bloon}} -> io:format("DB: Table 'bloon' already exists.~n");
        {aborted, R1} -> io:format("DB Error: creating 'bloon' table: ~p~n", [R1])
    end,

    case mnesia:create_table(monkey, MonkeyProps) of
        {atomic, ok} -> io:format("DB: Table 'monkey' created.~n");
        {aborted, {already_exists, monkey}} -> io:format("DB: Table 'monkey' already exists.~n");
        {aborted, R2} -> io:format("DB Error: creating 'monkey' table: ~p~n", [R2])
    end,

    mnesia:wait_for_tables([bloon, monkey], 20000).
