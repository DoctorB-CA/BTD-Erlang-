-module(d).
-behaviour(gen_statem).

-export([start_link/4]).
-export([init/1, callback_mode/0, moving/3]).

-define(DART_SPEED, 8).
-define(DART_TICK_INTERVAL, 50).
-define(HIT_RADIUS, 5.0).

-record(state, {
    current_pos,
    target_id,
    target_pid,
    dart_spec
}).

start_link(DartSpec, StartPos, TargetId, TargetPid) ->
    gen_statem:start_link(?MODULE, [DartSpec, StartPos, TargetId, TargetPid], []).

init([DartSpec, StartPos, TargetId, TargetPid]) ->
    StateData = #state{
        current_pos = StartPos,
        target_id = TargetId,
        target_pid = TargetPid,
        dart_spec = DartSpec
    },
    {ok, moving, StateData, {state_timeout, 0, move}}.

callback_mode() ->
    state_functions.

moving(state_timeout, move, State = #state{current_pos = CurrentPos, target_id = TargetId, target_pid = TargetPid}) ->
    case world_server:get_balloon_pos(TargetId) of
        not_found ->
            {stop, normal, State};
        {ok, TargetPos} ->
            case distance(CurrentPos, TargetPos) < ?HIT_RADIUS of
                true ->
                    b:take_damage(TargetPid, State#state.dart_spec),
                    {stop, normal, State};
                false ->
                    NewPos = calculate_next_step(CurrentPos, TargetPos, ?DART_SPEED),
                    NewState = State#state{current_pos = NewPos},
                    {keep_state, NewState, {state_timeout, ?DART_TICK_INTERVAL, move}}
            end
    end.

calculate_next_step({X1, Y1}, {X2, Y2}, Speed) ->
    Dist = distance({X1, Y1}, {X2, Y2}),
    if
        Dist == 0 -> {X1, Y1};
        true ->
            Ratio = Speed / Dist,
            StepX = (X2 - X1) * Ratio,
            StepY = (Y2 - Y1) * Ratio,
            {X1 + StepX, Y1 + StepY}
    end.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).