-module(m).
-behaviour(gen_statem).

-export([start_link/4, stop/1]).
-export([init/1, callback_mode/0, idle/3, throwing/3]).

-define(CHECK_INTERVAL, 1000).

-record(state, {
    monkey_id,   % id of monkey
    pos,          %postion of monkey in the map
    range = 100,   %how far the monkey can shoot (+- 100 in X and Y)
    target_id = none,  %what ballon are we hiting now
    target_pid = none, %what position are we hitiing now
    dart_spec :: map() %my weapon staus (stright, speed adn etc)
}).

%==================================================================
% Public API Functions
%==================================================================

start_link(MonkeyId, Pos, Range, DartSpec) ->    %getting id, postion,range. and weapon status
    gen_statem:start_link({local, MonkeyId}, ?MODULE, [MonkeyId, Pos, Range, DartSpec], []).   %creating a fsm of moinkey in the name of the id

stop(MonkeyId) ->
    gen_statem:stop(MonkeyId).   %killing monkey  <-----------need to get some banan back

%==================================================================
% gen_statem Callbacks
%==================================================================

init([MonkeyId, Pos, Range, DartSpec]) ->
    io:format("Monkey (~p) at ~p with ~p darts is starting up.~n", [MonkeyId, Pos, DartSpec]),
    StateData = #state{monkey_id = MonkeyId, pos = Pos, range = Range, dart_spec = DartSpec},
    {ok, idle, StateData, {state_timeout, ?CHECK_INTERVAL, check_target}}.

callback_mode() ->
    state_functions.

% --- States ---

idle(state_timeout, check_target, StateData = #state{monkey_id = MonkeyId, pos = Pos, range = Range}) ->
    io:format("Monkey (~p) is idling and checking for balloons...~n", [MonkeyId]),
    % FIXED: Changed 'game_state_server' to 's'
    case s:get_closest_balloon(Pos, Range) of
        {ok, BalloonId, BalloonPid} ->
            io:format("Monkey (~p) found balloon (~p)! Switching to 'throwing' state.~n", [MonkeyId, BalloonId]),
            NewStateData = StateData#state{target_id = BalloonId, target_pid = BalloonPid},
            {next_state, throwing, NewStateData, {state_timeout, 0, throw}};
        none ->
            {keep_state, StateData, {state_timeout, ?CHECK_INTERVAL, check_target}}
    end;
idle(_, _, StateData) ->
    {keep_state, StateData}.


throwing(state_timeout, throw, StateData) ->
    #state{
        monkey_id = MonkeyId,
        pos = MonkeyPos,
        target_id = TargetId,
        target_pid = TargetPid,
        dart_spec = DartSpec
    } = StateData,

    case erlang:is_process_alive(TargetPid) of
        true ->
            io:format("Monkey (~p) is firing a homing dart at balloon (~p)!~n", [MonkeyId, TargetId]),
            % FIXED: Changed 'dart_statem' to whatever you named your dart file (e.g., 'd')
            d:start_link(DartSpec, MonkeyPos, TargetId, TargetPid),
            ThrowInterval = maps:get(throw_interval, DartSpec, 1000),
            {keep_state, StateData, {state_timeout, ThrowInterval, throw}};
        false ->
            io:format("Target (~p) is gone! Monkey (~p) returning to 'idle'.~n", [TargetId, MonkeyId]),
            NewStateData = StateData#state{target_id = none, target_pid = none},
            {next_state, idle, NewStateData, {state_timeout, ?CHECK_INTERVAL, check_target}}
    end;
throwing(_, _, StateData) ->
    {keep_state, StateData}.
