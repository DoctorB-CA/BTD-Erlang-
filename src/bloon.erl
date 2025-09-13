-module(bloon).
-behaviour(gen_statem).

-include("dbr.hrl").

-export([start_link/3, get_pos/1]).
-export([init/1, callback_mode/0, terminate/3, moving/3]).

-define(MOVE_INTERVAL, 200).  % Move every 200ms instead of 2000ms
-define(REGION_WIDTH, 200).

-record(state, {id, index, health, pos, current_region_pid, region_pids, region_id}).

get_pos(Pid) -> gen_statem:call(Pid, get_pos).

start_link(Health, RPid, AllRPids) ->
    BloonId = erlang:make_ref(),
    gen_statem:start_link({global, BloonId}, ?MODULE, [{start, Health, BloonId}, RPid, AllRPids], []).

callback_mode() -> state_functions.

init([{start, Health, BloonId}, RPid, AllRPids]) ->
    Path = get_path(),
    [StartPos | _] = Path,
    BloonRecord = #bloon{id=BloonId, health=Health, index=1, pos=StartPos, region_id=0},
    db:write_bloon(BloonRecord),
    Data = #state{id=BloonId, health=Health, pos=StartPos, index=1,
                  current_region_pid=RPid, region_pids=AllRPids, region_id=0},
    {ok, moving, Data, {state_timeout, ?MOVE_INTERVAL, move}}.

terminate(_Reason, moving, #state{id = BloonId, current_region_pid = _RPid}) ->
    db:delete_bloon(BloonId),
    ok.

moving({call, From}, get_pos, Data) ->
    {keep_state, Data, {reply, From, {ok, Data#state.pos}}};
moving(info, {hit, Dmg}, Data=#state{id=BloonId, health=H, index=PI, region_id=RId, pos=Pos}) ->
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
            {stop, normal, Data};
        true ->
            db:write_bloon(#bloon{id=BloonId, health=H, index=NextIdx, pos=NewPos, region_id=RId}),
            NewData = Data#state{index=NextIdx, pos=NewPos},
            Action = handle_region_crossing(NewData),
            case Action of
                {move_to_new_node, _} ->
                    {stop, normal, NewData};
                {stay, _OldRPid} ->
                    {keep_state, NewData, {state_timeout, ?MOVE_INTERVAL, move}}
            end
    end.

handle_region_crossing(#state{pos = {NewX, _}, index = Idx, health = H, region_pids = AllPids, current_region_pid = OldRPid, region_id = OldRegionId}) ->
    NewRIdx = trunc(NewX / ?REGION_WIDTH),
    NewRPid = lists:nth(NewRIdx + 1, AllPids),
    OldNode = node(OldRPid),
    NewNode = node(NewRPid),
    if
        OldNode /= NewNode ->
            io:format("*DEBUG* --- Bloon ~p MIGRATING from node ~p to ~p ---~n", [self(), OldNode, NewNode]),
            Path = get_path(),
            RestOfPath = lists:nthtail(Idx - 1, Path),
            NewRegionId = NewRIdx,
            gen_server:cast(NewRPid, {spawn_bloon, RestOfPath, H, AllPids, NewRegionId}),
            {move_to_new_node, NewRPid};
        true ->
            NewRegionId = NewRIdx,
            if
                NewRegionId /= OldRegionId ->
                    db:write_bloon(#bloon{id=self(), health=H, pos=lists:nth(Idx, get_path()), index=Idx, region_id=NewRegionId});
                true -> ok
            end,
            {stay, OldRPid}
    end.


%%% ---------- paths -----------
get_path() ->
     %% Path: {0,200} -> {200,200} -> {200,400} -> {500,400} -> {500,200} -> {300,200} -> {300,600} -> {800,600}
    Start = {0,200},
    Path1 = right(Start, 200),                % {0,200} -> {200,200}
    Path2 = up(lists:last(Path1), 200),       % {200,200} -> {200,400}
    Path3 = right(lists:last(Path2), 300),    % {200,400} -> {500,400}
    Path4 = down(lists:last(Path3), 200),     % {500,400} -> {500,200}
    Path5 = left(lists:last(Path4), 200),     % {500,200} -> {300,200}
    Path6 = up(lists:last(Path5), 400),       % {300,200} -> {300,600}
    Path7 = right(lists:last(Path6), 500),    % {300,600} -> {800,600}
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