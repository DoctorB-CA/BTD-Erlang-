-module(region_server).
-behaviour(gen_server).
-export([start_link/3, init/1, handle_call/3, handle_cast/2]).

% Renamed the record to avoid conflicts with other modules.
-record(region_state, {id, x_start, x_end, bloons = #{}}).

start_link(Id, StartX, EndX) ->
    RegionName = list_to_atom("region_" ++ integer_to_list(Id)),
    gen_server:start_link({local, RegionName}, ?MODULE, [Id, StartX, EndX], []).

init([Id, StartX, EndX]) ->
    io:format("Region Server ~p (~p-~p) has started on node ~p.~n", [Id, StartX, EndX, node()]),
    {ok, #region_state{id = Id, x_start = StartX, x_end = EndX}}.

handle_call({find_bloon, MonkeyPos, Range}, _From, State) ->
    Closest = find_closest_bloon(MonkeyPos, Range, maps:to_list(State#region_state.bloons), none),
    Reply = case Closest of none -> {error, not_found}; {_Dist, Pid} -> {ok, Pid} end,
    {reply, Reply, State};
handle_call(ping, _From, State) -> {reply, self(), State}.

% This handler is no longer needed with the new migration logic.
% handle_cast({recreate_bloon, BloonState}, State) -> ...

handle_cast({spawn_monkey, Pos, Range}, State) ->
    io:format("~p: Received 'spawn_monkey' request. Starting monkey...~n", [node()]),
    monkey:start_link(Pos, Range, self()),
    {noreply, State};
% This now handles both initial spawns and migrations.
handle_cast({spawn_bloon, Path, Health, AllRegionPids}, State) ->
    bloon:start_link(Path, Health, self(), AllRegionPids),
    {noreply, State};
handle_cast({add_bloon, BloonPid, Pos}, State) ->
    {noreply, State#region_state{bloons = maps:put(BloonPid, Pos, State#region_state.bloons)}};
handle_cast({remove_bloon, BloonPid}, State) ->
    {noreply, State#region_state{bloons = maps:remove(BloonPid, State#region_state.bloons)}};
handle_cast({update_bloon_pos, BloonPid, NewPos}, State) ->
    {noreply, State#region_state{bloons = maps:update(BloonPid, NewPos, State#region_state.bloons)}}.

% Helper functions (unchanged)
distance({X1, Y1}, {X2, Y2}) -> math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).
find_closest_bloon(_, _, [], C) -> C;
find_closest_bloon(MPos, R, [{P, Pos} | Rest], C) ->
    Dist = distance(MPos, Pos),
    NC = if Dist =< R -> case C of none -> {Dist, P}; {CD, _} when Dist < CD -> {Dist, P}; _ -> C end; true -> C end,
    find_closest_bloon(MPos, R, Rest, NC).