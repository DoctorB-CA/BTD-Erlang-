-module(monkey_statem).
-behaviour(gen_statem).

-export([start_link/2, stop/1]).

% New public function to get balloons. This would be part of a game state module.
-export([get_closest_balloon/1]). 

% Callback functions for gen_statem
-export([init/1, callback_mode/0, idle/3, throwing/3]).

-record(state, {
    monkey_id,
    range = 100,
    target_balloon = none,
    damage = 1,
    throw_interval = 1000 % in milliseconds
}).

% --- Initial and Utility Functions ---

start_link(MonkeyId, Range) ->
    gen_statem:start_link({local, MonkeyId}, ?MODULE, [MonkeyId, Range], []).

init([MonkeyId, Range]) ->
    io:format("Monkey (~p) is starting up.~n", [MonkeyId]),
    StateData = #state{monkey_id = MonkeyId, range = Range},
    
    % Start in idle and set a periodic timer to check for targets.
    CheckInterval = 2000, % Check every 2 seconds
    {ok, idle, StateData, {state_timeout, CheckInterval, check_target}}.

callback_mode() ->
    state_functions.

% --- The States ---

idle(state_timeout, check_target, StateData) ->
    MonkeyId = StateData#state.monkey_id,
    Range = StateData#state.range,
    io:format("Monkey (~p) is idling and checking for balloons...~n", [MonkeyId]),
    
    % This is where the magic happens. Call a function to find a target.
    % We'll simulate this with a fake function.
    case get_closest_balloon(Range) of
        {ok, BalloonId} ->
            io:format("Monkey (~p) found balloon (~p)! Switching to 'throwing' state.~n", [MonkeyId, BalloonId]),
            NewStateData = StateData#state{target_balloon = BalloonId},
            
            % Stop the check_target timer and start throwing immediately.
            {next_state, throwing, NewStateData, {state_timeout, 0, throw}};
            
        none ->
            io:format("Monkey (~p) found no balloons. Staying in 'idle'.~n", [MonkeyId]),
            CheckInterval = 2000,
            % Stay in idle and set the check_target timer again.
            {keep_state, StateData, {state_timeout, CheckInterval, check_target}}
    end;

% Add a catch-all for any other messages to stay in idle
idle(_, _, StateData) ->
    {keep_state, StateData}.

throwing(state_timeout, throw, StateData) ->
    MonkeyId = StateData#state.monkey_id,
    TargetBalloon = StateData#state.target_balloon,
    
    % Check if the target is still there
    case is_balloon_alive(TargetBalloon) of
        true ->
            Damage = StateData#state.damage,
            io:format("Monkey (~p) is throwing a dart at balloon (~p)! (Damage: ~p)~n", [MonkeyId, TargetBalloon, Damage]),
            % Simulate sending a message to the balloon process
            % balloon_server:take_damage(TargetBalloon, Damage).
            
            % Set the next throw timer
            ThrowInterval = StateData#state.throw_interval,
            {keep_state, StateData, {state_timeout, ThrowInterval, throw}};
        
        false ->
            io:format("Balloon (~p) is gone! Monkey (~p) returning to 'idle'.~n", [TargetBalloon, MonkeyId]),
            NewStateData = StateData#state{target_balloon = none},
            
            % Stop the throwing timer and start the idle check_target timer again.
            CheckInterval = 2000,
            {next_state, idle, NewStateData, {state_timeout, CheckInterval, check_target}}
    end.

% --- Mock Functions for Demonstration ---

get_closest_balloon(_Range) ->
    % In a real game, this function would query the game state for balloons.
    % We'll simulate a random chance of finding one.
    case random:uniform(10) > 5 of
        true -> {ok, random:uniform(100)}; % Found a random balloon ID
        false -> none
    end.

is_balloon_alive(_BalloonId) ->
    % In a real game, this would check if the process for this balloon is still running.
    % We'll simulate a random chance of it being gone.
    case random:uniform(10) > 2 of
        true -> true;
        false -> false
    end.

stop(MonkeyId) ->
    gen_statem:stop(MonkeyId).