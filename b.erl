-module(b).
-behaviour(gen_statem).

-export([start_link/3, stop/1]).
-export([take_damage/2, update_zone/2]). % Added update_zone

-export([init/1, callback_mode/0, moving/3, terminate/3]).

-define(MOVE_INTERVAL, 1000).

-record(state, {
    id,
    hp = 1,
    pos,
    path :: list(),
    zone_pid :: pid()
}).

start_link(Id, Hp, Path) ->
    gen_statem:start_link({local, Id}, ?MODULE, [Id, Hp, Path], []).

take_damage(BalloonPid, DartSpec) ->
    gen_statem:cast(BalloonPid, {take_damage, DartSpec}).

% NEW: Public function for a zone_server to call
update_zone(BalloonPid, NewZonePid) ->
    gen_statem:cast(BalloonPid, {new_zone, NewZonePid}).

stop(Id) ->
    gen_statem:stop(Id).

init([Id, Hp, [StartPos | RestOfPath]]) ->
    {ok, ZonePid} = world_server:register_entity(Id, self(), StartPos),
    io:format("Balloon ~p assigned to zone ~p.~n", [Id, ZonePid]),
    StateData = #state{
        id = Id,
        hp = Hp,
        pos = StartPos,
        path = RestOfPath,
        zone_pid = ZonePid
    },
    {ok, moving, StateData, {state_timeout, 0, move}}.

callback_mode() ->
    state_functions.

moving(state_timeout, move, State = #state{id = Id, path = Path, zone_pid = ZonePid}) ->
    case Path of
        [NextPos | RestOfPath] ->
            zone_server:update_balloon_pos(ZonePid, Id, NextPos),
            NewState = State#state{pos = NextPos, path = RestOfPath},
            {keep_state, NewState, {state_timeout, ?MOVE_INTERVAL, move}};
        [] ->
            {stop, normal, State}
    end;
moving(cast, {take_damage, DartSpec}, State = #state{id = Id, hp = Hp}) ->
    Damage = maps:get(damage, DartSpec),
    DartType = maps:get(type, DartSpec),
    NewHp = Hp - Damage,
    % Now this line will work because DartType has a value
    io:format("Balloon ~p was hit by a ~p dart for ~p damage! HP is now ~p~n", [Id, DartType, Damage, NewHp]),
    if
        NewHp =< 0 ->
            {stop, normal, State#state{hp = NewHp}};
        true ->
            {keep_state, State#state{hp = NewHp}}
    end;
% NEW: Handler to update the balloon's zone_pid
moving(cast, {new_zone, NewZonePid}, State = #state{id = Id}) ->
    io:format("Balloon ~p has been handed off to new zone ~p~n", [Id, NewZonePid]),
    {keep_state, State#state{zone_pid = NewZonePid}}.

terminate(_Reason, _StateName, _StateData = #state{id = Id}) ->
    io:format("Balloon ~p process is terminating.~n", [Id]),
    % Tell the world to remove us from the ETS table
    world_server:deregister_entity(Id),
    ok.
