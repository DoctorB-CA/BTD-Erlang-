-module(game_logic).
-export([calculate_dart_path/3]).

%% This is the main function to calculate a dart's path to a balloon.
%% DartPos: The {X,Y} starting position of the dart (i.e., the monkey's position).
%% Balloon: The balloon map, which contains its current path_index.
%% Path: The list of all {X,Y} coordinates for the balloon path.
calculate_dart_path(DartPos, Balloon, Path) ->
    % The dart moves twice as fast as the balloon. Since the balloon moves
    % 1 step on the path per tick, the dart moves 2 steps.
    DartSpeed = 2,

    % Get the balloon's current and next positions from the main path.
    CurrentIndex = maps:get(path_index, Balloon),
    CurrentPos = lists:nth(CurrentIndex, Path),
    NextPos = lists:nth(CurrentIndex + 1, Path),

    % Calculate the balloon's velocity vector (how much it moves in one tick).
    {Xb, Yb} = CurrentPos,
    {NextXb, NextYb} = NextPos,
    VelX = NextXb - Xb,
    VelY = NextYb - Yb,

    % Predict where the balloon will be in the future to find an intercept point.
    % This is a simple prediction; more advanced games use more complex math.
    {Xd, Yd} = DartPos,
    Dist = distance(DartPos, CurrentPos),
    TimeToHit = Dist / DartSpeed, % A rough estimate of how many ticks it will take.
    
    InterceptX = round(Xb + VelX * TimeToHit),
    InterceptY = round(Yb + VelY * TimeToHit),
    InterceptPos = {InterceptX, InterceptY},

    % Now, generate a straight-line path for the dart from its start to the intercept point.
    generate_line(DartPos, InterceptPos, DartSpeed).


%% Helper function to calculate the distance between two points.
distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

%% Helper function to generate a list of points for the dart's path.
generate_line(StartPos, EndPos, Speed) ->
    {X1, Y1} = StartPos,
    {X2, Y2} = EndPos,
    Dist = distance(StartPos, EndPos),
    NumSteps = round(Dist / Speed),
    if
        NumSteps > 0 ->
            StepX = (X2 - X1) / NumSteps,
            StepY = (Y2 - Y1) / NumSteps,
            [{round(X1 + I * StepX), round(Y1 + I * StepY)} || I <- lists:seq(1, NumSteps)];
        true ->
            [] % If it's too close, the path is empty.
    end.
