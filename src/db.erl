-module(db).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-export([write_bloon/1, delete_bloon/1, write_monkey/1, delete_monkey/1]).
-export([get_bloons_in_regions/1]).
-export([add_node_to_schema/1, remove_node_from_schema/1]).

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

%% @doc Adds a new node to the Mnesia schema and sets up table replicas.
add_node_to_schema(Node) ->
    io:format("DB: Adding node ~p to Mnesia schema.~n", [Node]),

    % Add ram_copies of the tables to the new node.
    % Mnesia will handle adding the node to the schema implicitly.
    mnesia:add_table_copy(bloon, Node, ram_copies),
    mnesia:add_table_copy(monkey, Node, ram_copies),

    % Wait for the tables to be created and ready on the remote node.
    % This is a blocking call, which is what we want here.
    Tables = [bloon, monkey],
    io:format("DB: Waiting for tables ~p to be ready on node ~p...~n", [Tables, Node]),
    case mnesia:wait_for_tables(Tables, 20000) of
        ok ->
            io:format("DB: Tables are ready on node ~p.~n", [Node]),
            ok;
        {timeout, _} ->
            io:format("DB Error: timed out waiting for tables on node ~p.~n", [Node]),
            {error, timeout};
        {error, Reason} ->
            io:format("DB Error: waiting for tables on node ~p: ~p~n", [Node, Reason]),
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
