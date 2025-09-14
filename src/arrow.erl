-module(arrow).
-behaviour(gen_statem).
-include("dbr.hrl").

-export([start_link/4]).
-export([init/1, callback_mode/0, flying/3]).

-define(SPEED, 10).
-define(TICK_RATE, 50).
-define(MAX_RANGE, 300).
-define(HIT_THRESHOLD, 20).

-record(data, {id, type, pos, target_id, region_id, steps_left}).

start_link(DartType, StartPos, TargetId, RegionId) ->
    gen_statem:start_link(?MODULE, [DartType, StartPos, TargetId, RegionId], []).

callback_mode() -> state_functions.

init([DartType, StartPos, TargetId, RegionId]) ->
    io:format("Arrow ~p starting at ~p targeting ~p~n", [DartType, StartPos, TargetId]),
    
    % Create unique ID and store in database
    DartId = erlang:make_ref(),
    DartRecord = #dart{id=DartId, type=DartType, pos=StartPos, target_id=TargetId, region_id=RegionId},
    db:write_dart(DartRecord),
    
    MaxSteps = round(?MAX_RANGE / ?SPEED),
    Data = #data{id=DartId, type=DartType, pos=StartPos, target_id=TargetId, region_id=RegionId, steps_left=MaxSteps},
    
    {ok, flying, Data, {state_timeout, 0, move}}.

flying(state_timeout, move, Data = #data{id=DartId, pos=CurrentPos, target_id=TargetId, steps_left=StepsLeft}) ->
    if
        StepsLeft =< 0 ->
            % Out of range, remove from database
            db:delete_dart(DartId),
            {stop, normal, Data};
        true ->
            % Get target position from database
            case get_target_position(TargetId) of
                {ok, TargetPos} ->
                    % Calculate new position
                    Vector = calculate_vector(CurrentPos, TargetPos),
                    NewPos = move_towards(CurrentPos, Vector),
                    
                    % Update position in database
                    UpdatedRecord = #dart{id=DartId, type=Data#data.type, pos=NewPos, 
                                        target_id=TargetId, region_id=Data#data.region_id},
                    db:write_dart(UpdatedRecord),
                    
                    % Check if hit target
                    Dist = distance(NewPos, TargetPos),
                    if
                        Dist < ?HIT_THRESHOLD ->
                            io:format("Dart ~p hit target ~p!~n", [DartId, TargetId]),
                            % Hit! Remove dart and damage target
                            db:delete_dart(DartId),
                            damage_target(TargetId),
                            {stop, normal, Data};
                        true ->
                            % Continue flying
                            NewData = Data#data{pos=NewPos, steps_left=StepsLeft-1},
                            {keep_state, NewData, {state_timeout, ?TICK_RATE, move}}
                    end;
                {error, _} ->
                    % Target not found, remove dart
                    db:delete_dart(DartId),
                    {stop, normal, Data}
            end
    end.

%% Helper functions
get_target_position(TargetId) ->
    case db:get_all_bloons() of
        [] -> {error, no_bloons};
        Bloons ->
            case lists:keyfind(TargetId, #bloon.id, Bloons) of
                false -> {error, target_not_found};
                #bloon{pos=Pos} -> {ok, Pos}
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

move_towards({X, Y}, {VX, VY}) ->
    {X + VX, Y + VY}.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

damage_target(TargetId) ->
    % Send damage message directly to the balloon process
    try
        gen_statem:cast({global, TargetId}, {hit, 1}),
        io:format("*DEBUG* Sent damage to balloon ~p~n", [TargetId])
    catch
        _:_ ->
            % Balloon process might be dead/migrated, remove from database
            io:format("*DEBUG* Failed to damage balloon ~p, removing from DB~n", [TargetId]),
            db:delete_bloon(TargetId)
    end.