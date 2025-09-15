-module(bloon).
-behaviour(gen_statem).

-include("dbr.hrl").

-export([start_link/3, start_link_migration/6, get_pos/1]).
-export([init/1, callback_mode/0, terminate/3, moving/3]).

-define(MOVE_INTERVAL, 50). % Move every 50ms - fast movement for 60 FPS GUI
-define(REGION_WIDTH, 200).

-record(state, {id, index, health, pos, current_region_pid, region_pids, region_id}).

get_pos(Pid) -> gen_statem:call(Pid, get_pos).

start_link(Health, RPid, AllRPids) ->
    BloonId = erlang:make_ref(),
    gen_statem:start_link({global, BloonId}, ?MODULE, [{start, Health, BloonId}, RPid, AllRPids], []).

start_link_migration(Health, Index, Pos, RPid, AllRPids, OriginalBloonId) ->
    gen_statem:start_link({global, OriginalBloonId}, ?MODULE, [{migrate, Health, Index, Pos, OriginalBloonId}, RPid, AllRPids], []).

callback_mode() -> state_functions.

init([{start, Health, BloonId}, RPid, AllRPids]) ->
    Path = get_path(),
    [StartPos | _] = Path,
    BloonRecord = #bloon{id=BloonId, health=Health, index=1, pos=StartPos, region_id=0},
    db:write_bloon(BloonRecord),
    Data = #state{id=BloonId, health=Health, pos=StartPos, index=1,
                  current_region_pid=RPid, region_pids=AllRPids, region_id=0},
    {ok, moving, Data, {state_timeout, ?MOVE_INTERVAL, move}};

init([{migrate, Health, Index, Pos, BloonId}, RPid, AllRPids]) ->
    NewX = element(1, Pos),
    NewRegionId = trunc(NewX / ?REGION_WIDTH),
    % Update the existing balloon record (don't create a new one)
    BloonRecord = #bloon{id=BloonId, health=Health, index=Index, pos=Pos, region_id=NewRegionId},
    db:write_bloon(BloonRecord),
    Data = #state{id=BloonId, health=Health, pos=Pos, index=Index,
                  current_region_pid=RPid, region_pids=AllRPids, region_id=NewRegionId},
    io:format("*DEBUG* Migrated balloon ~p to node ~p at position ~p~n", [BloonId, node(), Pos]),
    {ok, moving, Data, {state_timeout, ?MOVE_INTERVAL, move}}.

terminate(normal, moving, #state{id = BloonId, current_region_pid = _RPid, index = Index}) ->
    % Check if this was a balloon that reached the end (high index) vs migration
    Path = get_path(),
    if 
        Index >= length(Path) ->
            % Balloon reached the end of the path - DELETE from DB
            io:format("*DEBUG* Balloon ~p reached end of path, deleting from DB~n", [BloonId]),
            db:delete_bloon(BloonId);
        true ->
            % This is a normal migration - DON'T delete from DB
            io:format("*DEBUG* Balloon ~p process terminating due to migration (normal)~n", [BloonId])
    end,
    ok;
terminate(_Reason, moving, #state{id = BloonId, current_region_pid = _RPid}) ->
    % This is a real death (health ≤ 0 or error) - DELETE from DB
    io:format("*DEBUG* Balloon ~p dying due to ~p, deleting from DB~n", [BloonId, _Reason]),
    db:delete_bloon(BloonId),
    ok.

moving({call, From}, get_pos, Data) ->
    {keep_state, Data, {reply, From, {ok, Data#state.pos}}};
moving(cast, {hit, Dmg}, Data=#state{id=BloonId, health=H, index=PI, region_id=RId, pos=Pos}) ->
    NewHealth = H - Dmg,
    io:format("*DEBUG* Balloon ~p hit for ~p damage! Health: ~p -> ~p~n", [BloonId, Dmg, H, NewHealth]),
    db:write_bloon(#bloon{id=BloonId, health=NewHealth, index=PI, pos=Pos, region_id=RId}),
    if
        NewHealth =< 0 ->
            io:format("*DEBUG* Bloon ~p died at position: ~p~n", [BloonId, Pos]),
            db:delete_bloon(BloonId),
            {stop, normal, Data#state{health=NewHealth}};
        true ->
            {keep_state, Data#state{health=NewHealth}}
    end;
moving(info, {hit, Dmg}, Data=#state{id=BloonId, health=H, index=PI, region_id=RId, pos=Pos}) ->
    % Keep old info handler for compatibility
    NewHealth = H - Dmg,
    db:write_bloon(#bloon{id=BloonId, health=NewHealth, index=PI, pos=Pos, region_id=RId}),
    if
        NewHealth =< 0 ->
            io:format("*DEBUG* Bloon died at position: ~p~n", [Pos]),
            db:delete_bloon(BloonId),
            {stop, normal, Data#state{health=NewHealth}};
        true ->
            {keep_state, Data#state{health=NewHealth}}
    end;
moving(state_timeout, move, Data=#state{id=BloonId, health=H, index=PI, region_id=RId}) ->
    NextIdx = PI + 1,
    NewPos = get_location(NextIdx),
    if
        NewPos =:= undefined ->
            % Balloon reached the end - player loses!
            gui:lose_game(),
            {stop, normal, Data};
        true ->
            db:write_bloon(#bloon{id=BloonId, health=H, index=NextIdx, pos=NewPos, region_id=RId}),
            NewData = Data#state{index=NextIdx, pos=NewPos},
            Action = handle_region_crossing(NewData),
            case Action of
                {move_to_new_node, _} ->
                    {stop, normal, NewData};
                {stop_and_migrate, _} ->
                    io:format("*DEBUG* Balloon ~p stopping for migration~n", [BloonId]),
                    {stop, normal, NewData};
                {stay, NewRPid} ->
                    % Update region PID in state if it changed
                    UpdatedData = NewData#state{current_region_pid=NewRPid},
                    {keep_state, UpdatedData, {state_timeout, ?MOVE_INTERVAL, move}}
            end
    end.

handle_region_crossing(#state{id=BloonId, pos = {NewX, _} = CurrentPos, index = Idx, health = H, region_pids = AllPids, current_region_pid = OldRPid, region_id = OldRegionId}) ->
    NewRIdx = trunc(NewX / ?REGION_WIDTH),
    if 
        NewRIdx < 0 -> 
            io:format("*DEBUG* Balloon ~p trying to go to negative region ~p, keeping in region 0~n", [BloonId, NewRIdx]),
            {stay, OldRPid};
        NewRIdx >= length(AllPids) ->
            % Balloon is going beyond the last region - let it continue to the end
            % Don't try to migrate, just stay in the current region until it reaches the end
            {stay, OldRPid};
        true ->
            NewRPid = lists:nth(NewRIdx + 1, AllPids),
            OldNode = node(OldRPid),
            NewNode = node(NewRPid),
            if
                OldNode /= NewNode ->
                    io:format("*DEBUG* --- Bloon ~p MIGRATING from node ~p to ~p ---~n", [BloonId, OldNode, NewNode]),
                    NewRegionId = NewRIdx,
                    % First spawn on new node, then stop this process
                    gen_server:cast(NewRPid, {spawn_bloon_migration, H, Idx, CurrentPos, AllPids, NewRegionId, BloonId}),
                    % Use a small delay to ensure new process starts before old one stops
                    timer:sleep(50),
                    {stop_and_migrate, NewRPid};
                OldRPid /= NewRPid ->
                    % Same node, different region - just update region (no debug print)
                    NewRegionId = NewRIdx,
                    db:write_bloon(#bloon{id=BloonId, health=H, pos=CurrentPos, index=Idx, region_id=NewRegionId}),
                    {stay, NewRPid};  % Switch to new region PID
                true ->
                    % Same region, just update position if needed
                    NewRegionId = NewRIdx,
                    if
                        NewRegionId /= OldRegionId ->
                            db:write_bloon(#bloon{id=BloonId, health=H, pos=CurrentPos, index=Idx, region_id=NewRegionId});
                        true -> ok
                    end,
                    {stay, OldRPid}
            end
    end.


%%% ---------- paths -----------
get_path() ->
     %% Path: {0,200} -> {200,200} -> {200,400} -> {500,400} -> {500,200} -> {300,200} -> {300,600} -> {799,600}
    Start = {0,200},
    Path1 = right(Start, 180),                % {0,200} -> {180,200}
    Path2 = up(lists:last(Path1), 200),       % {180,200} -> {200,400}
    Path3 = right(lists:last(Path2), 300),    % {200,400} -> {500,400}
    Path4 = down(lists:last(Path3), 200),     % {500,400} -> {500,200}
    Path5 = left(lists:last(Path4), 200),     % {500,200} -> {300,200}
    Path6 = up(lists:last(Path5), 400),       % {300,200} -> {300,600}
    Path7 = right(lists:last(Path6), 499),    % {300,600} -> {799,600}
    Path1 ++ Path2 ++ Path3 ++ Path4 ++ Path5 ++ Path6 ++ Path7.

%% Moves right by N pixels from Pos
right({X,Y}, N) ->
    lists:map(fun(D) -> {X+D, Y} end, lists:seq(1, N)).

%% Moves left by N pixels from Pos
left({X,Y}, N) ->
    lists:map(fun(D) -> {X-D, Y} end, lists:seq(1, N)).

%% Moves up by N pixels from Pos
up({X,Y}, N) ->
    lists:map(fun(D) -> {X, Y+D} end, lists:seq(1, N)).

%% Moves down by N pixels from Pos
down({X,Y}, N) ->
    lists:map(fun(D) -> {X, Y-D} end, lists:seq(1, N)).

%% Returns the location on the path for a given index
get_location(Index) ->
    Path = get_path(),
    case (Index > 0) andalso (Index =< length(Path)) of
        true -> lists:nth(Index, Path);
        false -> undefined
    end.