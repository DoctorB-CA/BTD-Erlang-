-module(bloon).
-behaviour(gen_statem).

-include("db.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-export([start_link/1, take_damage/2, freeze/2]).

%% ===================================================================
%% gen_statem Callbacks
%% ===================================================================
-export([init/1, callback_mode/0, terminate/3]).
-export([moving/3, frozen/3]). % State handler functions

-define(MOVE_INTERVAL, 1000). % ms
-define(REGION_WIDTH, 50).

%% This is the data the process holds. The "state" is the function name.
-record(data, {
    b,                  % The #bloon{} record from mnesia
    all_region_pids = [] :: list()
}).

%% ===================================================================
%% Public API Implementation
%% ===================================================================

% Start the FSM that CONTROLS a bloon.
start_link(BloonId) ->
    % Use {n, g, ...} for a GLOBAL unique name across all nodes.
    % This prevents the "split-brain" bug where two processes think they control the same bloon.
    gen_statem:start_link({via, gproc, {n, g, {bloon, BloonId}}}, ?MODULE, [BloonId], []).

% A dart damages a bloon. This can happen in any state.
take_damage(BloonId, Damage) ->
    % Find the globally registered process and cast to it.
    gen_statem:cast(gproc:where({n, g, {bloon, BloonId}}), {take_damage, Damage}).

% An ice monkey freezes a bloon. This causes a state transition.
freeze(BloonId, Duration) ->
    % Find the globally registered process and cast to it.
    gen_statem:cast(gproc:where({n, g, {bloon, BloonId}}), {freeze, Duration}).

%% ===================================================================
%% gen_statem Callback Implementation
%% ===================================================================

callback_mode() -> state_functions.

init([BloonId]) ->
    io:format("Bloon FSM ~p starting for ID ~p on node ~p~n", [self(), BloonId, node()]),
    F = fun() -> mnesia:read({bloon, BloonId}) end,
    case mnesia:transaction(F) of
        {atomic, [B]} ->
            % Start in the 'moving' state, and set a timer for the first move.
            Data = #data{b = B, all_region_pids = []},
            Actions = [{state_timeout, ?MOVE_INTERVAL, move}],
            {ok, moving, Data, Actions};
        {atomic, []} ->
            {stop, not_found};
        {aborted, Reason} ->
            {stop, {db_error, Reason}}
    end.

%% ===================================================================
%% State: moving
%% The bloon is actively moving along its path.
%% ===================================================================
moving({state_timeout, _, move}, _From, Data = #data{b = B, all_region_pids = Pids}) ->
    NextIdx = B#bloon.path_index + 1,
    Path = B#bloon.path,
    if
        NextIdx > length(Path) ->
            % Reached end of path. Delete from DB and stop.
            FDel = fun() -> mnesia:delete({bloon, B#bloon.id}) end,
            mnesia:transaction(FDel),
            {stop, normal, Data};
        true ->
            NewPos = lists:nth(NextIdx, Path),
            NewRegionId = trunc(element(1, NewPos) / ?REGION_WIDTH),
            UpdatedB = B#bloon{path_index = NextIdx, region_id = NewRegionId},
            FWrite = fun() -> mnesia:write(UpdatedB) end,
            mnesia:transaction(FWrite),
            NewData = Data#data{b = UpdatedB},
            if
                UpdatedB#bloon.region_id /= B#bloon.region_id andalso Pids /= [] ->
                    % Crossed into a new region. Tell new region server to take over.
                    NewRegionPid = lists:nth(NewRegionId + 1, Pids),
                    gen_statem:cast(NewRegionPid, {take_control, B#bloon.id}),
                    {stop, normal, NewData};
                true ->
                    % Continue moving. Reset the move timer.
                    {keep_state, NewData, [{state_timeout, ?MOVE_INTERVAL, move}]}
            end
    end;

moving({cast, {freeze, Duration}}, _From, Data) ->
    io:format("Bloon ~p is frozen for ~p ms~n", [Data#data.b#bloon.id, Duration]),
    % Transition to the 'frozen' state. Set a timer to unfreeze.
    {next_state, frozen, Data, [{state_timeout, Duration, unfreeze}]};

moving({cast, {take_damage, Damage}}, _From, Data) ->
    handle_damage(Damage, Data);

moving({cast, {update_region_pids, NewPids}}, _From, Data) ->
    {keep_state, Data#data{all_region_pids = NewPids}}.

%% ===================================================================
%% State: frozen
%% The bloon is stuck and cannot move.
%% ===================================================================
frozen({state_timeout, _, unfreeze}, _From, Data) ->
    io:format("Bloon ~p un-froze~n", [Data#data.b#bloon.id]),
    % Timer is up. Go back to the 'moving' state and start the move timer again.
    {next_state, moving, Data, [{state_timeout, ?MOVE_INTERVAL, move}]};

frozen({state_timeout, _, move}, _From, Data) ->
    % A 'move' tick fired while we were frozen. Ignore it, but keep the state.
    {keep_state, Data};

frozen({cast, {take_damage, Damage}}, _From, Data) ->
    % We can still take damage while frozen.
    handle_damage(Damage, Data);

frozen({cast, {update_region_pids, NewPids}}, _From, Data) ->
    {keep_state, Data#data{all_region_pids = NewPids}}.


%% ===================================================================
%% Common Helper Functions
%% ===================================================================

% Handles damage calculation, shared by all states.
handle_damage(Damage, Data = #data{b = B}) ->
    NewHealth = B#bloon.health - Damage,
    if
        NewHealth =< 0 ->
            FDel = fun() -> mnesia:delete({bloon, B#bloon.id}) end,
            mnesia:transaction(FDel),
            {stop, normal, Data};
        true ->
            NewB = B#bloon{health = NewHealth},
            FWrite = fun() -> mnesia:write(NewB) end,
            mnesia:transaction(FWrite),
            {keep_state, Data#data{b = NewB}}
    end.

%% ===================================================================
%% Termination
%% ===================================================================
terminate(Reason, _State, Data) ->
    Id = if
        is_record(Data, data) andalso is_record(Data#data.b, bloon) -> Data#data.b#bloon.id;
        true -> "unknown"
    end,
    io:format("Bloon FSM for ~p terminating. Reason: ~p~n", [Id, Reason]),
    ok.
