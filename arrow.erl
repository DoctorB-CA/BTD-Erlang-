-module(arrow).
-export([fire/2]).

-define(SPEED, 10).
-define(TICK_RATE, 50).
-define(MAX_RANGE, 300).
-define(HIT_THRESHOLD, 8).

fire(StartPos, TargetPid) ->
    spawn(fun() -> init(StartPos, TargetPid) end).

init(StartPos, TargetPid) ->
    link(TargetPid),
    case bloon:get_pos(TargetPid) of
        {ok, TargetPos} ->
            Vector = calculate_vector(StartPos, TargetPos),
            MaxSteps = round(?MAX_RANGE / ?SPEED),
            loop(StartPos, TargetPid, Vector, MaxSteps);
        {error, _Reason} ->
            exit(normal)
    end.

loop(CurrentPos, TargetPid, Vector, StepsLeft) ->
    if
            %%bar put here the arrow moves
        StepsLeft =< 0 ->
            exit(normal);
        true ->
            timer:sleep(?TICK_RATE),
            NewPos = move(CurrentPos, Vector),
            case bloon:get_pos(TargetPid) of
                {ok, TargetPos} ->
                    Dist = distance(NewPos, TargetPos),
                    if
                        Dist < ?HIT_THRESHOLD ->
                            TargetPid ! {hit, 1},
                            exit(normal);
                        true ->
                            loop(NewPos, TargetPid, Vector, StepsLeft - 1)
                    end;
                {error, _Reason} ->
                    exit(normal)
            end
    end.

calculate_vector({X1, Y1}, {X2, Y2}) ->
    DX = X2 - X1,
    DY = Y2 - Y1,
    Dist = math:sqrt(DX*DX + DY*DY),
    if
        Dist == 0 -> {0, 0};
        true -> {DX / Dist * ?SPEED, DY / Dist * ?SPEED}
    end.

move({X, Y}, {VX, VY}) ->
    {X + VX, Y + VY}.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).