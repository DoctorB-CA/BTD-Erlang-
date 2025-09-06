-module(db).
-export([init/1, create_schema_on_workers/1]).
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
%% This is called by the main_supervisor on the MAIN node.
%% It creates the Mnesia schema and the initial tables.
init(AllNodes) ->
    io:format("DB: Creating Mnesia schema on all nodes: ~p~n", [AllNodes]),
    mnesia:create_schema(AllNodes),
    
    io:format("DB: Starting Mnesia on main node...~n"),
    ok = mnesia:start(),

    io:format("DB: Creating tables...~n"),
    create_tables(),

    io:format("DB: Telling worker nodes to start Mnesia and wait for tables...~n"),
    rpc:multicall(AllNodes, ?MODULE, create_schema_on_workers, [self()]),
    
    io:format("DB: Main node setup complete.~n"),
    ok.

%% @doc
%% This is called via RPC on all the WORKER nodes.
create_schema_on_workers(MainNode) ->
    io:format("DB (~p): Starting Mnesia...~n", [node()]),
    ok = mnesia:start(),
    io:format("DB (~p): Waiting for tables from ~p...~n", [node(), MainNode]),
    mnesia:wait_for_tables([bloon, monkey], 20000),
    io:format("DB (~p): Tables are ready.~n", [node()]),
    ok.

%% @private
%% Defines and creates all Mnesia tables.
create_tables() ->
    MainNode = node(),
    WorkerNodes = nodes() -- [MainNode],

    % Define the table properties.
    % The master copy is on the main node's disk.
    % All other nodes get a fast in-memory copy.
    TableProps = [
        {disc_copies, [MainNode]},
        {ram_copies, WorkerNodes}
    ],

    % Create the 'bloon' table.
    case mnesia:create_table(bloon, TableProps ++ [{attributes, record_info(fields, bloon)}]) of
        {atomic, ok} -> io:format("DB: Table 'bloon' created.~n");
        {aborted, {already_exists, bloon}} -> io:format("DB: Table 'bloon' already exists.~n")
    end,

    % Create the 'monkey' table.
    case mnesia:create_table(monkey, TableProps ++ [{attributes, record_info(fields, monkey)}]) of
        {atomic, ok} -> io:format("DB: Table 'monkey' created.~n");
        {aborted, {already_exists, monkey}} -> io:format("DB: Table 'monkey' already exists.~n")
    end,
    ok.
