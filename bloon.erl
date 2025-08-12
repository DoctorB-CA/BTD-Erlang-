-module(bloon).
-behaviour(gen_statem).

-export([start_link/4, get_pos/1]).
-export([init/1, callback_mode/0, terminate/3, moving/3]).

-define(MOVE_INTERVAL, 2000).
-define(REGION_WIDTH, 50).

-record(state, {path, path_index, health, pos, current_region_pid, region_pids}).

get_pos(Pid) -> gen_statem:call(Pid, get_pos).

start_link(Path, Health, RPid, AllRPids) -> gen_statem:start_link(?MODULE, [{start, Path, Health}, RPid, AllRPids], []).

callback_mode() -> state_functions.

init([{start, Path, Health}, RPid, AllRPids]) ->
    [StartPos | _] = Path,
    gen_server:cast(RPid, {add_bloon, self(), StartPos}),
    Data = #state{path=Path, health=Health, pos=StartPos, path_index=1,
                  current_region_pid=RPid, region_pids=AllRPids},
    {ok, moving, Data, {state_timeout, ?MOVE_INTERVAL, move}}.

% FIXED: The second argument is the state *name* (the atom 'moving'), not a general wildcard.
% This function is called by the OTP framework *after* the decision to stop has been made.
terminate(_Reason, moving, #state{current_region_pid = RPid}) ->
    gen_server:cast(RPid, {remove_bloon, self()}),
    ok. % It's good practice for terminate to explicitly return 'ok'.

moving({call, From}, get_pos, Data) ->
    {keep_state, Data, {reply, From, {ok, Data#state.pos}}};
% FIXED: This clause now correctly returns only the result of the 'if' statement.
moving(info, {hit, Dmg}, Data=#state{health=H}) ->
    NewHealth = H - Dmg,
    if
        NewHealth =< 0 ->
            io:format("die bllon die~n"),
            % When health is zero or less, stop the process.
            {stop, normal, Data#state{health=NewHealth}};
        true ->
            % Otherwise, just update the health and keep going.
            {keep_state, Data#state{health=NewHealth}}
    end;
moving(state_timeout, move, Data) ->
    NextIdx = Data#state.path_index + 1,
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
                {stay, OldRPid} ->
                    gen_server:cast(OldRPid, {update_bloon_pos, self(), NewPos}),
                    {keep_state, NewData, {state_timeout, ?MOVE_INTERVAL, move}}
            end
    end.

handle_region_crossing(#state{pos = {NewX, _}, path = FullPath, path_index = Idx, health = H, region_pids = AllPids, current_region_pid = OldRPid}) ->
    NewRIdx = trunc(NewX / ?REGION_WIDTH),
    NewRPid = lists:nth(NewRIdx + 1, AllPids),
    OldNode = node(OldRPid),
    NewNode = node(NewRPid),
    if
        OldNode /= NewNode ->
            io:format("--- Bloon ~p MIGRATING from node ~p to ~p ---~n", [self(), OldNode, NewNode]),
            RestOfPath = lists:nthtail(Idx - 1, FullPath),
            gen_server:cast(OldRPid, {remove_bloon, self()}),
            gen_server:cast(NewRPid, {spawn_bloon, RestOfPath, H, AllPids}),
            {move_to_new_node, NewRPid};
        true ->
            {stay, OldRPid}
    end.