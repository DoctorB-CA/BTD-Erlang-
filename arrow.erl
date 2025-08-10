-module(arrow).
-export([fire/2]).

-define(SPEED, 10).      
-define(TICK_RATE, 50).   
-define(MAX_RANGE, 300). 
-define(HIT_THRESHOLD, 8).

fire(StartPos, TargetPid) ->
    % We use spawn (not spawn_link) to keep the arrow independent from the monkey.
    spawn(fun() -> init(StartPos, TargetPid) end).

init(StartPos, TargetPid) ->
    % FIX: Link this arrow process to its target bloon.
    % If the bloon process dies, this arrow process will now terminate automatically.
    link(TargetPid),

    % We still need the initial position to aim. 
    % If the bloon is already dead, the 'link' call above would have already
    % crashed this process, which is the desired behavior.
    case bloon:get_pos(TargetPid) of
        {ok, TargetPos} ->
            Vector = calculate_vector(StartPos, TargetPos),
            MaxSteps = round(?MAX_RANGE / ?SPEED),
            loop(StartPos, TargetPid, Vector, MaxSteps);
        {error, _Reason} ->
            % This case is now very unlikely, but we leave it for safety.
            exit(normal)
    end.

% The loop is simpler now. It no longer needs a try...catch block because
% the link will terminate this process if the bloon dies.
loop(CurrentPos, TargetPid, Vector, StepsLeft) ->
    if
        StepsLeft =< 0 ->
            exit(normal); % Arrow disappears if it misses and goes too far.
        true ->
            timer:sleep(?TICK_RATE),
            NewPos = move(CurrentPos, Vector),
            % We still check for hits in case the arrow reaches the bloon.
            case bloon:get_pos(TargetPid) of
                {ok, TargetPos} ->
                    Dist = distance(NewPos, TargetPos),
                    if
                        Dist < ?HIT_THRESHOLD ->
                            TargetPid ! {hit, 1},
                            exit(normal); % Arrow hits and disappears.
                        true ->
                            loop(NewPos, TargetPid, Vector, StepsLeft - 1)
                    end;
                {error, _Reason} ->
                    % The bloon died in the tiny window since the last check.
                    % The link's exit signal will terminate us anyway.
                    exit(normal)
            end
    end.

%% Helper Functions
calculate_vector({X1, Y1}, {X2, Y2}) ->
    DX = X2 - X1,
    DY = Y2 - Y1,
    Dist = math:sqrt(DX*DX + DY*DY),
    if 
        Dist == 0 -> {0, 0};
        true ->
            {DX / Dist * ?SPEED, DY / Dist * ?SPEED}
    end.

move({X, Y}, {VX, VY}) ->
    {X + VX, Y + VY}.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).