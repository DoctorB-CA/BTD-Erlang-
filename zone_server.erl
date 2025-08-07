-module(zone_server).
-behaviour(gen_server).

-export([start_link/3]).
-export([add_entity/4, get_closest_balloon/3, get_balloon_pos/2, update_balloon_pos/3]).

-export([init/1, handle_call/3, handle_cast/2]).

start_link(Id, Bounds, WorldServerPid) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id, Bounds, WorldServerPid], []).

add_entity(ServerRef, Id, EntityPid, Pos) ->
    gen_server:cast(ServerRef, {add, Id, EntityPid, Pos}).

get_closest_balloon(ServerRef, MonkeyPos, Range) ->
    gen_server:call(ServerRef, {get_closest, MonkeyPos, Range}).

get_balloon_pos(ServerRef, BalloonId) ->
    gen_server:call(ServerRef, {get_pos, BalloonId}).

update_balloon_pos(ServerRef, Id, NewPos) ->
    gen_server:cast(ServerRef, {update_pos, Id, NewPos}).

init([_Id, Bounds, WorldServerPid]) ->
    group_leader(whereis(user), self()),
    io:format("~p: Zone Server process started for bounds ~p.~n", [node(), Bounds]),
    State = #{
        bounds => Bounds,
        world_server => WorldServerPid,
        entities => maps:new()
    },
    {ok, State}.

handle_cast({add, Id, EntityPid, Pos}, State = #{entities := Entities}) ->
    NewEntities = maps:put(Id, {EntityPid, Pos}, Entities),
    {noreply, State#{entities => NewEntities}};
handle_cast({update_pos, Id, NewPos}, State = #{bounds := Bounds, world_server := World, entities := Entities}) ->
    case maps:find(Id, Entities) of
        {ok, {EntityPid, _OldPos}} ->
            case is_in_bounds(NewPos, Bounds) of
                true ->
                    NewEntities = maps:update(Id, {EntityPid, NewPos}, Entities),
                    {noreply, State#{entities => NewEntities}};
                false ->
                    {ok, NewZoneRef} = world_server:handle_balloon_move(World, Id, EntityPid, NewPos),
                    b:update_zone(EntityPid, NewZoneRef),
                    NewEntities = maps:remove(Id, Entities),
                    {noreply, State#{entities => NewEntities}}
            end;
        error ->
            {noreply, State}
    end.

handle_call({get_closest, MonkeyPos, Range}, _From, State = #{entities := Entities}) ->
    Balloons = maps:filter(fun(Id, _) -> is_balloon(Id) end, Entities),
    BalloonsInRange = maps:filter(fun(_, {_Pid, Pos}) ->
        distance(MonkeyPos, Pos) =< Range
    end, Balloons),
    case maps:to_list(BalloonsInRange) of
        [] -> {reply, none, State};
        List ->
            SortFun = fun({_IdA, {_PidA, PosA}}, {_IdB, {_PidB, PosB}}) ->
                          distance(MonkeyPos, PosA) =< distance(MonkeyPos, PosB)
                      end,
            [ClosestBalloon | _] = lists:sort(SortFun, List),
            {Id, {Pid, _Pos}} = ClosestBalloon,
            {reply, {ok, Id, Pid}, State}
    end;
handle_call({get_pos, BalloonId}, _From, State = #{entities := Entities}) ->
    case maps:find(BalloonId, Entities) of
        {ok, {_Pid, Pos}} -> {reply, {ok, Pos}, State};
        error -> {reply, not_found, State}
    end.

is_in_bounds({X, Y}, {X1, Y1, X2, Y2}) ->
    X >= X1 andalso X < X2 andalso Y >= Y1 andalso Y < Y2.

is_balloon(AtomId) ->
    case lists:prefix("b", atom_to_list(AtomId)) of
        true -> true;
        false -> false
    end.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).