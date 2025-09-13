-module(bloon).
-behaviour(gen_statem).

-include("dbr.hrl").

-export([start_link/5, get_pos/1]).
-export([init/1, callback_mode/0, terminate/3, moving/3]).

-define(MOVE_INTERVAL, 2000).
-define(REGION_WIDTH, 200).

-record(state, {id, path, path_index, health, pos, current_region_pid, region_pids, region_id}).

get_pos(Pid) -> gen_statem:call(Pid, get_pos).

start_link(Path, Health, RPid, AllRPids, RegionId) ->
    BloonId = erlang:make_ref(),
    gen_statem:start_link({global, BloonId}, ?MODULE, [{start, Path, Health, RegionId, BloonId}, RPid, AllRPids], []).

callback_mode() -> state_functions.

init([{start, Path, Health, RegionId, BloonId}, RPid, AllRPids]) ->
    [StartPos | _] = Path,
    BloonRecord = #bloon{id=BloonId, health=Health, path=Path, path_index=1, region_id=RegionId},
    db:write_bloon(BloonRecord),
    % gen_server:cast(RPid, {add_bloon, self(), StartPos}), % No longer needed
    Data = #state{id=BloonId, path=Path, health=Health, pos=StartPos, path_index=1,
                  current_region_pid=RPid, region_pids=AllRPids, region_id=RegionId},
    {ok, moving, Data, {state_timeout, ?MOVE_INTERVAL, move}}.

% FIXED: The second argument is the state *name* (the atom 'moving'), not a general wildcard.
% This function is called by the OTP framework *after* the decision to stop has been made.
terminate(_Reason, moving, #state{id = BloonId, current_region_pid = _RPid}) ->
    db:delete_bloon(BloonId),
    % gen_server:cast(RPid, {remove_bloon, self()}), % No longer needed
    ok. % It's good practice for terminate to explicitly return 'ok'.

moving({call, From}, get_pos, Data) ->
    {keep_state, Data, {reply, From, {ok, Data#state.pos}}};
% FIXED: This clause now correctly returns only the result of the 'if' statement.
moving(info, {hit, Dmg}, Data=#state{id=BloonId, health=H, path=P, path_index=PI, region_id=RId, pos=Pos}) ->
    NewHealth = H - Dmg,
    db:write_bloon(#bloon{id=BloonId, health=NewHealth, path=P, path_index=PI, region_id=RId}),
    if
        NewHealth =< 0 ->
            io:format("*DEBUG* Bloon died at position: ~p~n", [Pos]),
            % When health is zero or less, stop the process.
            {stop, normal, Data#state{health=NewHealth}};
        true ->
            % Otherwise, just update the health and keep going.
            {keep_state, Data#state{health=NewHealth}}
    end;
moving(state_timeout, move, Data=#state{id=BloonId, health=H, path=P, region_id=RId}) ->
    NextIdx = Data#state.path_index + 1,
    db:write_bloon(#bloon{id=BloonId, health=H, path=P, path_index=NextIdx, region_id=RId}),
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
                    % gen_server:cast(OldRPid, {update_bloon_pos, self(), NewPos}), % No longer needed
                    {keep_state, NewData, {state_timeout, ?MOVE_INTERVAL, move}}
            end
    end.

handle_region_crossing(#state{pos = {NewX, _}, path = FullPath, path_index = Idx, health = H, region_pids = AllPids, current_region_pid = OldRPid, region_id = OldRegionId}) ->
    NewRIdx = trunc(NewX / ?REGION_WIDTH),
    NewRPid = lists:nth(NewRIdx + 1, AllPids),
    OldNode = node(OldRPid),
    NewNode = node(NewRPid),
    if
        OldNode /= NewNode ->
            io:format("*DEBUG* --- Bloon ~p MIGRATING from node ~p to ~p ---~n", [self(), OldNode, NewNode]),
            RestOfPath = lists:nthtail(Idx - 1, FullPath),
            % gen_server:cast(OldRPid, {remove_bloon, self()}), % No longer needed
            % The new region ID is simply the index.
            NewRegionId = NewRIdx,
            gen_server:cast(NewRPid, {spawn_bloon, RestOfPath, H, AllPids, NewRegionId}),
            {move_to_new_node, NewRPid};
        true ->
            % If we are staying on the same node, we still need to update the region_id in the database
            NewRegionId = NewRIdx,
            if
                NewRegionId /= OldRegionId ->
                    db:write_bloon(#bloon{id=self(), health=H, path=FullPath, path_index=Idx, region_id=NewRegionId});
                true -> ok
            end,
            {stay, OldRPid}
    end.