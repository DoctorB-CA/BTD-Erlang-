%%% bloon.erl
%%% Moves along Path on its node; responds to hits.

-module(bloon).
-behaviour(gen_server).

-export([start_link/5, start_remotely/5, get_pos/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SPEED, 2000).           %% ms per path step
-define(REGION_WIDTH, 50).

-record(state, {
    path, path_index = 1,
    health, pos, type,
    current_region_pid, region_servers
}).

start_remotely(Path, Health, Type, RegionPid, AllRegionPids) ->
    start_link(Path, Health, Type, RegionPid, AllRegionPids).

get_pos(Pid) ->
    gen_server:call(Pid, get_pos).

start_link(Path, Health, Type, RegionPid, AllRegionPids) ->
    gen_server:start_link(?MODULE, [Path, Health, Type, RegionPid, AllRegionPids], []).

init([Path, Health, Type, RegionPid, AllRegionPids]) ->
    erlang:group_leader(erlang:whereis(user), self()),
    [StartPos | _] = Path,
    io:format("Bloon (~p) HP=~p at ~p~n", [Type, Health, StartPos]),
    gen_server:cast(RegionPid, {add_bloon, self(), StartPos}),
    erlang:send_after(?SPEED, self(), move),
    {ok, #state{path=Path, health=Health, pos=StartPos, type=Type,
                current_region_pid=RegionPid, region_servers=AllRegionPids}}.

handle_call(get_pos, _From, State) ->
    {reply, {ok, State#state.pos}, State};
handle_call(_Req, _From, State) ->
    {reply, ok, State}.

%% Accept {hit, Damage} or {hit, Damage, Type}
handle_info({hit, Damage}, State) ->
    handle_info({hit, Damage, unknown}, State);
handle_info({hit, Damage, ArrowType}, State) ->
    NewHealth = State#state.health - Damage,
    io:format("Bloon (~p) at ~p HIT by ~p -> HP: ~p~n",
              [State#state.type, State#state.pos, ArrowType, NewHealth]),
    case NewHealth =< 0 of
        true  -> io:format("*** BLOON (~p) POPPED at ~p ***~n", [State#state.type, State#state.pos]),
                 {stop, normal, State};
        false -> {noreply, State#state{health = NewHealth}}
    end;

handle_info(move, State = #state{path = Path, path_index = I}) ->
    NextI = I + 1,
    case NextI > length(Path) of
        true  -> io:format("Bloon (~p) reached end~n", [State#state.type]),
                 {stop, normal, State};
        false ->
            NewPos = lists:nth(NextI, Path),
            NewRegionPid = handle_region_crossing(State#state.pos, NewPos, self(), State),
            gen_server:cast(NewRegionPid, {update_bloon_pos, self(), NewPos}),
            erlang:send_after(?SPEED, self(), move),
            {noreply, State#state{path_index = NextI, pos = NewPos, current_region_pid = NewRegionPid}}
    end.

handle_cast(_Msg, State) -> {noreply, State}.

handle_region_crossing({OldX, _}, NewPos = {NewX, _}, BloonPid,
                       _State = #state{current_region_pid = CurrPid, region_servers = AllPids}) ->
    OldIdx = trunc(OldX / ?REGION_WIDTH),
    NewIdx = trunc(NewX / ?REGION_WIDTH),
    case OldIdx =/= NewIdx of
        true ->
            io:format("Bloon ~p region ~p -> ~p~n", [BloonPid, OldIdx, NewIdx]),
            NewRegionPid = lists:nth(NewIdx + 1, AllPids),
            gen_server:cast(CurrPid, {remove_bloon, BloonPid}),
            gen_server:cast(NewRegionPid, {add_bloon, BloonPid, NewPos}),
            NewRegionPid;
        false -> CurrPid
    end.
