-module(world_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([register_entity/3, get_balloon_pos/1, handle_balloon_move/4]).

-export([init/1, handle_call/3]).

-define(WORLD_WIDTH, 400).
-define(WORLD_HEIGHT, 400).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

register_entity(Id, Pid, Pos) ->
    gen_server:call(?MODULE, {register, Id, Pid, Pos}).

get_balloon_pos(BalloonId) ->
    gen_server:call(?MODULE, {get_pos, BalloonId}).

handle_balloon_move(WorldPid, Id, EntityPid, NewPos) ->
    gen_server:call(WorldPid, {moved_zone, Id, EntityPid, NewPos}).

init([]) ->
    io:format("World Server starting...~n"),
    Host = atom_to_list(node()),
    [_Name, Hostname] = string:split(Host, "@"),

    ZoneNodeDefs = [
        {zone1, list_to_atom("zone1@" ++ Hostname), {0, 0, ?WORLD_WIDTH/2, ?WORLD_HEIGHT/2}},
        {zone2, list_to_atom("zone2@" ++ Hostname), {?WORLD_WIDTH/2, 0, ?WORLD_WIDTH, ?WORLD_HEIGHT/2}},
        {zone3, list_to_atom("zone3@" ++ Hostname), {0, ?WORLD_HEIGHT/2, ?WORLD_WIDTH/2, ?WORLD_HEIGHT}},
        {zone4, list_to_atom("zone4@" ++ Hostname), {?WORLD_WIDTH/2, ?WORLD_HEIGHT/2, ?WORLD_WIDTH, ?WORLD_HEIGHT}}
    ],
    Zones = lists:map(fun({Id, Node, Bounds}) ->
        io:format("Attempting to start ~p on node ~p...~n", [Id, Node]),
        {ok, Pid} = rpc:call(Node, zone_server, start_link, [Id, Bounds, self()]),
        {Id, Pid, Node, Bounds}
    end, ZoneNodeDefs),
    State = #{
        zones => Zones,
        balloon_locations => maps:new()
    },
    {ok, State}.

handle_call({register, Id, Pid, Pos}, _From, State = #{zones := Zones, balloon_locations := Locs}) ->
    case get_zone_for_pos(Pos, Zones) of
        {ok, {ZoneId, ZonePid, Node, _Bounds}} ->
            gen_server:cast(ZonePid, {add, Id, Pid, Pos}),
            NewLocs = case is_balloon(Id) of
                true -> maps:put(Id, {ZoneId, Node}, Locs);
                false -> Locs
            end,
            {reply, {ok, {ZoneId, Node}}, State#{balloon_locations => NewLocs}};
        not_found ->
            {reply, {error, out_of_bounds}, State}
    end;
handle_call({get_pos, BalloonId}, _From, State = #{balloon_locations := Locs}) ->
    case maps:find(BalloonId, Locs) of
        {ok, ZoneRef} ->
            Reply = zone_server:get_balloon_pos(ZoneRef, BalloonId),
            {reply, Reply, State};
        error ->
            {reply, not_found, State}
    end;
handle_call({moved_zone, Id, Pid, NewPos}, _From, State = #{zones := Zones, balloon_locations := Locs}) ->
    case get_zone_for_pos(NewPos, Zones) of
        {ok, {NewZoneId, NewZonePid, NewNode, _Bounds}} ->
            gen_server:cast(NewZonePid, {add, Id, Pid, NewPos}),
            NewLocs = maps:put(Id, {NewZoneId, NewNode}, Locs),
            {reply, {ok, {NewZoneId, NewNode}}, State#{balloon_locations => NewLocs}};
        not_found ->
            NewLocs = maps:remove(Id, Locs),
            {reply, ok, State#{balloon_locations => NewLocs}}
    end.

get_zone_for_pos(_, []) ->
    not_found;
get_zone_for_pos({X, Y}, [ {Id, Pid, Node, {X1, Y1, X2, Y2}} | Rest ]) ->
    if
        X >= X1, X < X2, Y >= Y1, Y < Y2 -> {ok, {Id, Pid, Node, {X1, Y1, X2, Y2}}};
        true -> get_zone_for_pos({X, Y}, Rest)
    end.

is_balloon(AtomId) ->
    case lists:prefix("b", atom_to_list(AtomId)) of
        true -> true;
        false -> false
    end.
