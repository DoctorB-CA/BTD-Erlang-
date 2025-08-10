-module(bloon).
-behaviour(gen_server).

-export([start_link/4, get_pos/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SPEED, 2000). 
-define(REGION_WIDTH, 50).

-record(state, {
    path,
    path_index = 1,
    health,
    pos,
    current_region_pid,
    region_servers 
}).

%% API Functions
get_pos(Pid) ->
    gen_server:call(Pid, get_pos).

start_link(Path, Health, RegionPid, AllRegionPids) ->
    gen_server:start_link(?MODULE, [Path, Health, RegionPid, AllRegionPids], []).

%% gen_server Callbacks
init([Path, Health, RegionPid, AllRegionPids]) ->
    [StartPos | _] = Path,
    io:format("Bloon starting with ~p HP at ~p~n", [Health, StartPos]),
    gen_server:cast(RegionPid, {add_bloon, self(), StartPos}),
    erlang:send_after(?SPEED, self(), move),
    {ok, #state{path=Path, health=Health, pos=StartPos, current_region_pid=RegionPid, region_servers=AllRegionPids}}.

handle_call(get_pos, _From, State) ->
    {reply, {ok, State#state.pos}, State};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_info({hit, Damage}, State) ->
    NewHealth = State#state.health - Damage,
    io:format("Bloon at ~p hit! Health: ~p -> ~p~n", [State#state.pos, State#state.health, NewHealth]),
    if
        NewHealth =< 0 ->
            % NEW, MORE VISIBLE MESSAGE FOR THE POP!
            io:format("~n*** BLOON POPPED at position ~p! ***~n~n", [State#state.pos]),
            {stop, normal, State};
        true ->
            {noreply, State#state{health = NewHealth}}
    end;
handle_info(move, State) ->
    NextIndex = State#state.path_index + 1,
    if
        NextIndex > length(State#state.path) ->
            io:format("Bloon reached the end!~n"),
            {stop, normal, State};
        true ->
            NewPos = lists:nth(NextIndex, State#state.path),
            handle_region_crossing(State#state.pos, NewPos, self(), State),
            gen_server:cast(State#state.current_region_pid, {update_bloon_pos, self(), NewPos}),
            erlang:send_after(?SPEED, self(), move),
            {noreply, State#state{path_index = NextIndex, pos = NewPos}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_region_crossing({OldX, _}, {NewX, _}, BloonPid, State) ->
    OldRegionIndex = trunc(OldX / ?REGION_WIDTH),
    NewRegionIndex = trunc(NewX / ?REGION_WIDTH),
    if
        OldRegionIndex /= NewRegionIndex ->
            io:format("Bloon ~p crossing from region ~p to ~p~n", [BloonPid, OldRegionIndex, NewRegionIndex]),
            OldRegionPid = lists:nth(OldRegionIndex + 1, State#state.region_servers),
            NewRegionPid = lists:nth(NewRegionIndex + 1, State#state.region_servers),
            gen_server:cast(OldRegionPid, {remove_bloon, BloonPid}),
            gen_server:cast(NewRegionPid, {add_bloon, BloonPid, {NewX, 0}}),
            ok;
        true ->
            ok
    end.