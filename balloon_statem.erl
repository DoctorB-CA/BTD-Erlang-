-module(balloon_statem).
-behaviour(gen_statem).

-export([start_link/3, stop/1]).
-export([take_damage/2]). % Public function for monkeys to call

-export([init/1, callback_mode/0, moving/3, terminate/3]).

-define(MOVE_INTERVAL, 1000). % Move every 1 second

-record(state, {
    id,
    hp = 1,
    pos,
    path :: list() % A list of {X,Y} coordinates to follow
}).

% --- Public API ---

% Path is a list of coordinates, e.g., [{10,0}, {10,100}, {50,100}]
start_link(Id, Hp, Path) ->
    gen_statem:start_link({local, Id}, ?MODULE, [Id, Hp, Path], []).

take_damage(BalloonPid, Damage) ->
    gen_statem:cast(BalloonPid, {take_damage, Damage}).

stop(Id) ->
    gen_statem:stop(Id).

% --- gen_statem Callbacks ---

init([Id, Hp, [StartPos | RestOfPath]]) ->
    StateData = #state{id = Id, hp = Hp, pos = StartPos, path = RestOfPath},
    % Tell the game server we exist!
    game_state_server:add_balloon(Id, self(), StartPos),
    % Start moving immediately
    {ok, moving, StateData, {state_timeout, 0, move}}.

callback_mode() ->
    state_functions.

% The balloon is in its 'moving' state
moving(state_timeout, move, State = #state{id = Id, pos = CurrentPos, path = Path}) ->
    case Path of
        [NextPos | RestOfPath] ->
            io:format("Balloon ~p moving to ~p~n", [Id, NextPos]),
            % In a real game, you would smoothly animate this. Here, we just jump.
            NewState = State#state{pos = NextPos, path = RestOfPath},
            % IMPORTANT: Update the game server with our new position
            game_state_server:add_balloon(Id, self(), NextPos),
            {keep_state, NewState, {state_timeout, ?MOVE_INTERVAL, move}};
        [] ->
            io:format("Balloon ~p reached the end.~n", [Id]),
            {stop, normal, State} % Stop the process if the path is finished
    end;

% Handle taking damage from a monkey
moving(cast, {take_damage, Damage}, State = #state{id = Id, hp = Hp}) ->
    NewHp = Hp - Damage,
    io:format("Balloon ~p took ~p damage, HP is now ~p~n", [Id, Damage, NewHp]),
    if
        NewHp =< 0 ->
            io:format("Balloon ~p POPPED!~n", [Id]),
            {stop, normal, State#state{hp = NewHp}}; % Pop! Stop the process.
        true ->
            NewState = State#state{hp = NewHp},
            {keep_state, NewState}
    end.

% Cleanup function, called automatically when the process stops for any reason
terminate(_Reason, _State, #state{id = Id}) ->
    io:format("Balloon ~p is being removed from the game.~n", [Id]),
    % IMPORTANT: Tell the game server we are gone. This prevents "ghost" balloons.
    game_state_server:remove_balloon(Id).