-module(arrow).
-behaviour(gen_statem).

-include("db.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-export([fire/2]).

%% ===================================================================
%% gen_statem Callbacks
%% ===================================================================
-export([init/1, callback_mode/0, terminate/3, flying/3]).

-define(DAMAGE, 1).
-define(SPEED, 20).
-define(TICK_RATE, 50). % ms
-define(HIT_THRESHOLD, 10).
-define(REGION_WIDTH, 50).

-record(data, {
    current_pos,
    target_pos
}).

%% ===================================================================
%% Public API Implementation
%% ===================================================================

%% Fire an arrow from StartPos towards a BloonId
fire(StartPos, TargetId) ->
    % We don't track the bloon. We get its current position and fire at that spot.
    F = fun() -> mnesia:read({bloon, TargetId}) end,
    case mnesia:transaction(F) of
        {atomic, [B]} ->
            TargetPos = lists:nth(B#bloon.path_index, B#bloon.path),
            % Start a temporary FSM to control the arrow's flight.
            gen_statem:start_link(?MODULE, [StartPos, TargetPos], []);
        _ ->
            % Bloon might have died between scan and fire, that's ok.
            ok
    end.

%% ===================================================================
%% gen_statem Callback Implementation
%% ===================================================================

callback_mode() -> state_functions.

init([StartPos, TargetPos]) ->
    Data = #data{current_pos = StartPos, target_pos = TargetPos},
    % Start in the 'flying' state and trigger the first move immediately.
    {ok, flying, Data, [{state_timeout, 0, move}]}.

%% ===================================================================
%% State: flying
%% The arrow is moving towards its target position.
%% ===================================================================
flying({state_timeout, _, move}, _From, Data = #data{current_pos = CurrentPos, target_pos = TargetPos}) ->
    Dist = distance(CurrentPos, TargetPos),
    if
        Dist < ?HIT_THRESHOLD ->
            % We've reached the destination. Apply damage and stop.
            apply_damage(TargetPos),
            {stop, normal, Data};
        true ->
            % Move closer and schedule the next move.
            Vector = calculate_vector(CurrentPos, TargetPos),
            NewPos = move(CurrentPos, Vector),
            NewData = Data#data{current_pos = NewPos},
            {keep_state, NewData, [{state_timeout, ?TICK_RATE, move}]}
    end.

%% ===================================================================
%% Helper Functions
%% ===================================================================

%% Find all bloons near the impact site and damage them.
apply_damage(ImpactPos) ->
    RegionId = trunc(element(1, ImpactPos) / ?REGION_WIDTH),
    RegionName = list_to_atom("region_" ++ integer_to_list(RegionId)),
    % Ask the region server to find a bloon at the impact site.
    case gen_server:call(RegionName, {find_bloon, ImpactPos, ?HIT_THRESHOLD}) of
        {ok, BloonId} ->
            % If found, tell that bloon's FSM to take damage.
            bloon:take_damage(BloonId, ?DAMAGE);
        {error, not_found} ->
            ok
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

%% ===================================================================
%% Termination
%% ===================================================================
terminate(_Reason, _State, _Data) ->
    ok.
