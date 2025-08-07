-module(m).
-behaviour(gen_statem).

-export([start_link/4, stop/1]).
-export([init/1, callback_mode/0, idle/3, throwing/3]).

-define(CHECK_INTERVAL, 1000).

-record(state, {
    monkey_id,
    pos,
    range = 100,
    target_id = none,
    target_pid = none,
    dart_spec :: map(),
    zone_ref :: {atom(), atom()}
}).

start_link(MonkeyId, Pos, Range, DartSpec) ->
    gen_statem:start_link({local, MonkeyId}, ?MODULE, [MonkeyId, Pos, Range, DartSpec], []).

stop(MonkeyId) ->
    gen_statem:stop(MonkeyId).

init([MonkeyId, Pos, Range, DartSpec]) ->
    io:format("Monkey (~p) at ~p is starting up.~n", [MonkeyId, Pos]),
    {ok, ZoneRef} = world_server:register_entity(MonkeyId, self(), Pos),
    io:format("Monkey (~p) assigned to zone ~p.~n", [MonkeyId, ZoneRef]),
    StateData = #state{
        monkey_id = MonkeyId,
        pos = Pos,
        range = Range,
        dart_spec = DartSpec,
        zone_ref = ZoneRef
    },
    {ok, idle, StateData, {state_timeout, ?CHECK_INTERVAL, check_target}}.

callback_mode() ->
    state_functions.

idle(state_timeout, check_target, StateData = #state{monkey_id = MonkeyId, pos = Pos, range = Range, zone_ref = ZoneRef}) ->
    io:format("Monkey (~p) is asking zone ~p for balloons...~n", [MonkeyId, ZoneRef]),
    case zone_server:get_closest_balloon(ZoneRef, Pos, Range) of
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