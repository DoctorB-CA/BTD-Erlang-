-module(b).
-behaviour(gen_statem).

-export([start_link/3, stop/1]).
-export([take_damage/2, update_zone/2]).

-export([init/1, callback_mode/0, moving/3, terminate/3]).

-define(MOVE_INTERVAL, 1000).

-record(state, {
    id,
    hp = 1,
    pos,
    path :: list(),
    zone_ref :: {atom(), atom()}
}).

start_link(Id, Hp, Path) ->
    gen_statem:start_link({local, Id}, ?MODULE, [Id, Hp, Path], []).

take_damage(BalloonPid, DartSpec) ->
    gen_statem:cast(BalloonPid, {take_damage, DartSpec}).

update_zone(BalloonPid, NewZoneRef) ->
    gen_statem:cast(BalloonPid, {new_zone, NewZoneRef}).

stop(Id) ->
    gen_statem:stop(Id).

init([Id, Hp, [StartPos | RestOfPath]]) ->
    {ok, ZoneRef} = world_server:register_entity(Id, self(), StartPos),
    io:format("Balloon ~p assigned to zone ~p.~n", [Id, ZoneRef]),
    StateData = #state{
        id = Id,
        hp = Hp,
        pos = StartPos,
        path = RestOfPath,
        zone_ref = ZoneRef
    },
    {ok, moving, StateData, {state_timeout, 0, move}}.

callback_mode() ->
    state_functions.

moving(state_timeout, move, State = #state{id = Id, path = Path, zone_ref = ZoneRef}) ->
    case Path of
        [NextPos | RestOfPath] ->
            zone_server:update_balloon_pos(ZoneRef, Id, NextPos),
            NewState = State#state{pos = NextPos, path = RestOfPath},
            {keep_state, NewState, {state_timeout, ?MOVE_INTERVAL, move}};
        [] ->
            {stop, normal, State}
    end;
moving(cast, {take_damage, DartSpec}, State = #state{id = Id, hp = Hp}) ->
    Damage = maps:get(damage, DartSpec),
    DartType = maps:get(type, DartSpec),
    NewHp = Hp - Damage,
    io:format("Balloon ~p was hit by a ~p dart for ~p damage! HP is now ~p~n", [Id, DartType, Damage, NewHp]),
    if
        NewHp =< 0 ->
            {stop, normal, State#state{hp = NewHp}};
        true ->
            {keep_state, State#state{hp = NewHp}}
    end;
moving(cast, {new_zone, NewZoneRef}, State = #state{id = Id}) ->
    io:format("Balloon ~p has been handed off to new zone ~p~n", [Id, NewZoneRef]),
    {keep_state, State#state{zone_ref = NewZoneRef}}.

terminate(_Reason, _StateName, _StateData = #state{id = Id}) ->
    io:format("Balloon ~p process is terminating.~n", [Id]),
    ok.
