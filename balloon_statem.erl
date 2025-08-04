-module(balloon_statem).
-behaviour(gen_statem).

-export([start_link/3, stop/1]).
-export([take_damage/2]).

-export([init/1, callback_mode/0, moving/3, terminate/3]).

-define(MOVE_INTERVAL, 1000).

-record(state, {
    id,
    hp = 1,
    pos,
    path :: list()
}).

%==================================================================
% Public API Functions
%==================================================================

start_link(Id, Hp, Path) ->
    gen_statem:start_link({local, Id}, ?MODULE, [Id, Hp, Path], []).

take_damage(BalloonPid, DartSpec) ->
    gen_statem:cast(BalloonPid, {take_damage, DartSpec}).

stop(Id) ->
    gen_statem:stop(Id).

%==================================================================
% gen_statem Callbacks
%==================================================================

init([Id, Hp, [StartPos | RestOfPath]]) ->
    StateData = #state{id = Id, hp = Hp, pos = StartPos, path = RestOfPath},
    game_state_server:add_balloon(Id, self(), StartPos),
    {ok, moving, StateData, {state_timeout, 0, move}}.

callback_mode() ->
    state_functions.

moving(state_timeout, move, State = #state{id = Id, pos = CurrentPos, path = Path}) ->
    case Path of
        [NextPos | RestOfPath] ->
            io:format("Balloon ~p moving to ~p~n", [Id, NextPos]),
            NewState = State#state{pos = NextPos, path = RestOfPath},
            game_state_server:add_balloon(Id, self(), NextPos),
            {keep_state, NewState, {state_timeout, ?MOVE_INTERVAL, move}};
        [] ->
            io:format("Balloon ~p reached the end.~n", [Id]),
            {stop, normal, State}
    end;
moving(cast, {take_damage, DartSpec}, State = #state{id = Id, hp = Hp}) ->
    Damage = maps:get(damage, DartSpec),
    DartType = maps:get(type, DartSpec),
    NewHp = Hp - Damage,
    io:format("Balloon ~p was hit by a ~p dart for ~p damage! HP is now ~p~n", [Id, DartType, Damage, NewHp]),
    if
        NewHp =< 0 ->
            io:format("Balloon ~p POPPED!~n", [Id]),
            {stop, normal, State#state{hp = NewHp}};
        true ->
            NewState = State#state{hp = NewHp},
            {keep_state, NewState}
    end.

terminate(_Reason, _State, #state{id = Id}) ->
    io:format("Balloon ~p is being removed from the game.~n", [Id]),
    game_state_server:remove_balloon(Id).