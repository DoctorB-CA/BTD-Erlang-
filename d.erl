-module(d).
-behaviour(gen_statem).

-export([start_link/4]).
-export([init/1, callback_mode/0, homing/3, terminate/3]).

-define(MOVE_INTERVAL, 200). % Darts move faster than balloons

-record(state, {
    id,
    target_pid :: pid(),
    target_mref :: reference(), % To store the monitor reference
    spec,
    pos
}).

start_link(Id, Pos, TargetPid, Spec) ->
    gen_statem:start_link(?MODULE, [Id, Pos, TargetPid, Spec], []).

init([Id, Pos, TargetPid, Spec]) ->
    % THIS IS THE KEY: The dart monitors the balloon's process.
    MRef = erlang:monitor(process, TargetPid),
    io:format("Dart ~p created, monitoring balloon ~p~n", [Id, TargetPid]),
    StateData = #state{
        id = Id,
        target_pid = TargetPid,
        target_mref = MRef,
        spec = Spec,
        pos = Pos
    },
    % Start moving immediately
    {ok, homing, StateData, {state_timeout, 0, move}}.

callback_mode() ->
    state_functions.

% This is the handler for the one-way link.
% If the balloon dies, this function is triggered.
homing(info, {'DOWN', MRef, process, _Pid, _Reason}, State = #state{target_mref = MRef, id = Id}) ->
    io:format("Dart ~p terminating because its target is down.~n", [Id]),
    {stop, normal, State};

homing(state_timeout, move, State = #state{id = Id, pos = MyPos, target_pid = TargetPid, spec = Spec}) ->
    % Ask the world/zone for the balloon's current position
    case world_server:get_balloon_pos(TargetPid) of
        {ok, TargetPos} ->
            % Simple logic: move towards the target. A real implementation would be more complex.
            {DX, DY} = {element(1, TargetPos) - element(1, MyPos), element(2, TargetPos) - element(2, MyPos)},
            Dist = math:sqrt(DX*DX + DY*DY),
            if
                Dist < 10 -> % We are close enough to hit
                    io:format("Dart ~p hit balloon ~p!~n", [Id, TargetPid]),
                    b:take_damage(TargetPid, Spec),
                    {stop, normal, State};
                true ->
                    % Move a fraction of the way towards the target
                    NewX = element(1, MyPos) + (DX / Dist) * 20,
                    NewY = element(2, MyPos) + (DY / Dist) * 20,
                    {keep_state, State#state{pos = {NewX, NewY}}, {state_timeout, ?MOVE_INTERVAL, move}}
            end;
        not_found ->
            % The balloon is no longer registered, so we should stop.
            io:format("Dart ~p terminating because its target was not found.~n", [Id]),
            {stop, normal, State}
    end.

terminate(_Reason, _StateName, _StateData) ->
    ok.
