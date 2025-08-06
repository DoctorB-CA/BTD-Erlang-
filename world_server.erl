-module(world_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([register_entity/3, deregister_entity/1, handle_balloon_move/4, get_entities_for_zone/1, get_balloon_pos/1]).

-export([init/1, handle_call/3, handle_cast/2]).

-define(WORLD_WIDTH, 400).
-define(WORLD_HEIGHT, 400).
-define(ETS_TABLE, entity_locations).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_entity(Id, Pid, Pos) ->
    gen_server:call(?MODULE, {register, Id, Pid, Pos}).

deregister_entity(Id) ->
    gen_server:cast(?MODULE, {deregister, Id}).

handle_balloon_move(WorldPid, Id, EntityPid, NewPos) ->
    gen_server:call(WorldPid, {moved_zone, Id, EntityPid, NewPos}).

get_entities_for_zone(ZoneId) ->
    gen_server:call(?MODULE, {get_entities, ZoneId}).

get_balloon_pos(TargetPid) ->
    gen_server:call(?MODULE, {get_pos, TargetPid}).

init([]) ->
    io:format("World Server starting...~n"),
    EtsTable = ets:new(?ETS_TABLE, [set, public, named_table, {read_concurrency, true}]),
    ZoneDefs = [
        {zone1, {0, 0, ?WORLD_WIDTH/2, ?WORLD_HEIGHT/2}},
        {zone2, {?WORLD_WIDTH/2, 0, ?WORLD_WIDTH, ?WORLD_HEIGHT/2}},
        {zone3, {0, ?WORLD_HEIGHT/2, ?WORLD_WIDTH/2, ?WORLD_HEIGHT}},
        {zone4, {?WORLD_WIDTH/2, ?WORLD_HEIGHT/2, ?WORLD_WIDTH, ?WORLD_HEIGHT}}
    ],
    Zones = lists:map(fun({Id, Bounds}) ->
        {ok, Pid} = zone_server:start_link(Id, Bounds, self()),
        {Id, Pid, Bounds}
    end, ZoneDefs),
    State = #{
        zones => Zones,
        ets_table => EtsTable
    },
    {ok, State}.

handle_call({register, Id, Pid, Pos}, _From, State = #{zones := Zones}) ->
    case get_zone_for_pos(Pos, Zones) of
        {ok, {ZoneId, ZonePid, _Bounds}} ->
            zone_server:add_entity(ZonePid, Id, Pid, Pos),
            ets:insert(?ETS_TABLE, {Id, Pid, Pos, ZoneId}),
            {reply, {ok, ZonePid}, State};
        not_found ->
            {reply, {error, out_of_bounds}, State}
    end;

handle_call({moved_zone, Id, Pid, NewPos}, _From, State = #{zones := Zones}) ->
    case get_zone_for_pos(NewPos, Zones) of
        {ok, {NewZoneId, NewZonePid, _Bounds}} ->
            zone_server:add_entity(NewZonePid, Id, Pid, NewPos),
            ets:insert(?ETS_TABLE, {Id, Pid, NewPos, NewZoneId}),
            {reply, {ok, NewZonePid}, State};
        not_found ->
            ets:delete(?ETS_TABLE, Id),
            {reply, {error, out_of_bounds}, State}
    end;

handle_call({get_entities, ZoneId}, _From, State) ->
    % THE FIX IS HERE: Using ets:match_object which is simpler and safer.
    % The pattern {'_', '_', '_', ZoneId} finds all objects where the 4th element is our ZoneId.
    MatchingObjects = ets:match_object(?ETS_TABLE, {'_', '_', '_', ZoneId}),
    % We then transform the result into the map format the zone_server expects.
    Entities = lists:map(fun({Id, Pid, Pos, _Zone}) -> {Id, {Pid, Pos}} end, MatchingObjects),
    {reply, {ok, maps:from_list(Entities)}, State};

handle_call({get_pos, TargetPidOrId}, _From, State) ->
    % Find by Pid or Id for flexibility
    Pattern = case is_pid(TargetPidOrId) of
        true -> {'_', TargetPidOrId, '$1', '_'};
        false -> {TargetPidOrId, '_', '$1', '_'}
    end,
    case ets:match_object(?ETS_TABLE, Pattern) of
        [{_, _, Pos, _}] -> {reply, {ok, Pos}, State};
        [] -> {reply, not_found, State}
    end.

handle_cast({deregister, Id}, State) ->
    ets:delete(?ETS_TABLE, Id),
    {noreply, State}.

get_zone_for_pos(_, []) -> not_found;
get_zone_for_pos({X, Y}, [ {Id, Pid, {X1, Y1, X2, Y2}} | Rest ]) ->
    if X >= X1, X < X2, Y >= Y1, Y < Y2 -> {ok, {Id, Pid, {X1, Y1, X2, Y2}}};
       true -> get_zone_for_pos({X, Y}, Rest)
    end.
