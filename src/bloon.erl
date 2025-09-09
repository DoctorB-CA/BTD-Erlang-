-module(bloon).
-behaviour(gen_statem).

-include("dbr.hrl").

-export([start_link/6, get_pos/1]).
-export([init/1, callback_mode/0, terminate/3, moving/3]).

-define(MOVE_INTERVAL, 2000).
-define(REGION_WIDTH, 200).

-record(state, {id, path, path_index, type, health, pos, current_region_pid, region_pids, region_id}).

get_pos(Pid) -> gen_statem:call(Pid, get_pos).

start_link(Path, Type, Health, RPid, AllRPids, RegionId) ->
    BloonId = erlang:make_ref(),
    gen_statem:start_link({global, BloonId}, ?MODULE, [{start, Path, Type, Health, RegionId, BloonId}, RPid, AllRPids], []).

callback_mode() -> state_functions.

init([{start, Path, Type, Health, RegionId, BloonId}, RPid, AllRPids]) ->
    [StartPos | _] = Path,
    BloonRecord = #bloon{id=BloonId, type=Type, health=Health, path=Path, path_index=1, region_id=RegionId},
    db:write_bloon(BloonRecord),
    Data = #state{id=BloonId, path=Path, type=Type, health=Health, pos=StartPos, path_index=1,
                  current_region_pid=RPid, region_pids=AllRPids, region_id=RegionId},
    {ok, moving, Data, {state_timeout, ?MOVE_INTERVAL, move}}.

terminate(_Reason, moving, #state{id = BloonId}) ->
    db:delete_bloon(BloonId),
    ok.

moving({call, From}, get_pos, Data) ->
    {keep_state, Data, {reply, From, {ok, Data#state.pos}}};

moving(info, {hit, Dmg}, Data=#state{id=BloonId, type=T, health=H, path=P, path_index=PI, region_id=RId, pos=Pos}) ->
    NewHealth = H - Dmg,
    db:write_bloon(#bloon{id=BloonId, type=T, health=NewHealth, path=P, path_index=PI, region_id=RId}),
    if
        NewHealth =< 0 ->
            io:format("*DEBUG* Bloon died at position: ~p~n", [Pos]),
            {stop, normal, Data#state{health=NewHealth}};
        true ->
            {keep_state, Data#state{health=NewHealth}}
    end;

moving(state_timeout, move, Data=#state{id=BloonId, type=T, health=H, path=P, region_id=RId}) ->
    NextIdx = Data#state.path_index + 1,
    db:write_bloon(#bloon{id=BloonId, type=T, health=H, path=P, path_index=NextIdx, region_id=RId}),
    if
        NextIdx > length(Data#state.path) ->
            {stop, normal, Data};
        true ->
            NewPos = lists:nth(NextIdx, Data#state.path),
            NewData = Data#state{path_index=NextIdx, pos=NewPos},
            Action = handle_region_crossing(NewData),
            case Action of
                {move_to_new_node, _} ->
                    {stop, normal, NewData};
                {stay, _OldRPid} ->
                    {keep_state, NewData, {state_timeout, ?MOVE_INTERVAL, move}}
            end
    end.

handle_region_crossing(#state{pos = {NewX, _}, path = FullPath, path_index = Idx, type = T, health = H, region_pids = AllPids, current_region_pid = OldRPid, region_id = OldRegionId}) ->
    NewRIdx = trunc(NewX / ?REGION_WIDTH),
    NewRPid = lists:nth(NewRIdx + 1, AllPids),
    OldNode = node(OldRPid),
    NewNode = node(NewRPid),
    if
        OldNode /= NewNode ->
            io:format("*DEBUG* --- Bloon ~p MIGRATING from node ~p to ~p ---~n", [self(), OldNode, NewNode]),
            RestOfPath = lists:nthtail(Idx - 1, FullPath),
            NewRegionId = NewRIdx,
            gen_server:cast(NewRPid, {spawn_bloon, RestOfPath, T, H, AllPids, NewRegionId}),
            {move_to_new_node, NewRPid};
        true ->
            NewRegionId = NewRIdx,
            if
                NewRegionId /= OldRegionId ->
                    db:write_bloon(#bloon{id=self(), type=T, health=H, path=FullPath, path_index=Idx, region_id=NewRegionId});
                true -> ok
            end,
            {stay, OldRPid}
    end.