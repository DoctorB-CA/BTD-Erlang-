-module(monkey_statem).
-behaviour(gen_statem).

-export([start_link/3, stop/1]).

% Callback functions for gen_statem
-export([init/1, callback_mode/0, idle/3, throwing/3]).

-define(CHECK_INTERVAL, 1000). % Check for targets every 1 second

-record(state, {
    monkey_id,
    pos,                 % The {X, Y} position of the monkey
    range = 100,
    target_id = none,    % The ID of the target balloon
    target_pid = none,   % The process ID (Pid) of the target balloon
    damage = 1,
    throw_interval = 1000 % Attack speed in milliseconds
}).

% --- Initial and Utility Functions ---

start_link(MonkeyId, Pos, Range) ->
    gen_statem:start_link({local, MonkeyId}, ?MODULE, [MonkeyId, Pos, Range], []).

stop(MonkeyId) ->
    gen_statem:stop(MonkeyId).

init([MonkeyId, Pos, Range]) ->
    io:format("Monkey (~p) at ~p is starting up.~n", [MonkeyId, Pos]),
    StateData = #state{monkey_id = MonkeyId, pos = Pos, range = Range},
    
    % Start in idle and set a periodic timer to check for targets.
    {ok, idle, StateData, {state_timeout, ?CHECK_INTERVAL, check_target}}.

callback_mode() ->
    state_functions.

% --- The States ---

%% @doc In the idle state, the monkey periodically scans for a target.
idle(state_timeout, check_target, StateData = #state{monkey_id = MonkeyId, pos = Pos, range = Range}) ->
    io:format("Monkey (~p) is idling and checking for balloons...~n", [MonkeyId]),
    
    % Call the game state server to find a target in range
    case game_state_server:get_closest_balloon(Pos, Range) of
        {ok, BalloonId, BalloonPid} ->
            io:format("Monkey (~p) found balloon (~p)! Switching to 'throwing' state.~n", [MonkeyId, BalloonId]),
            NewStateData = StateData#state{target_id = BalloonId, target_pid = BalloonPid},
            
            % Switch to throwing state and throw a dart immediately
            {next_state, throwing, NewStateData, {state_timeout, 0, throw}};
            
        none ->
            % No balloon found, stay in idle and check again later
            {keep_state, StateData, {state_timeout, ?CHECK_INTERVAL, check_target}}
    end;

% Catch-all for any other messages to stay in idle
idle(_, _, StateData) ->
    
    {keep_state, StateData}.

throwing(state_timeout, throw, StateData = #state{monkey_id = MonkeyId, target_id = TargetId, target_pid = TargetPid}) ->
    % Use a 'case' statement, which is not restricted to guards.
    case erlang:is_process_alive(TargetPid) of
        true ->
            % The balloon process is still alive, so attack.
            Damage = StateData#state.damage,
            io:format("Monkey (~p) is throwing a dart at balloon (~p)! (Damage: ~p)~n", [MonkeyId, TargetId, Damage]),
            balloon_statem:take_damage(TargetPid, Damage),
            
            % Set the timer for the next throw.
            ThrowInterval = StateData#state.throw_interval,
            {keep_state, StateData, {state_timeout, ThrowInterval, throw}};
        
        false ->
            % The balloon process is gone, so return to idle.
            io:format("Balloon (~p) is gone! Monkey (~p) returning to 'idle'.~n", [TargetId, MonkeyId]),
            NewStateData = StateData#state{target_id = none, target_pid = none},
            
            % Switch back to idle and start checking for targets again.
            {next_state, idle, NewStateData, {state_timeout, ?CHECK_INTERVAL, check_target}}
    end;

% Catch-all for any other messages to stay in the throwing state
throwing(_, _, StateData) ->
    {keep_state, StateData}.