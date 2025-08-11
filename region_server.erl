%%% region_server.erl
-module(region_server).
-behaviour(gen_server).

-export([start_link/3, start_remotely/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    id, x_start, x_end,
    bloons = #{}   %% #{Pid => {X,Y}}
}).

start_remotely(Id, StartX, EndX) ->
    start_link(Id, StartX, EndX).

start_link(Id, StartX, EndX) ->
    gen_server:start_link(?MODULE, [Id, StartX, EndX], []).

init([Id, StartX, EndX]) ->
    erlang:group_leader(erlang:whereis(user), self()),
    io:format("Region ~p (~p..~p) started~n", [Id, StartX, EndX]),
    {ok, #state{id = Id, x_start = StartX, x_end = EndX}}.

%% Targeting
handle_call({find_bloon, MonkeyPos, Range}, _From, State) ->
    Closest = find_closest_bloon(MonkeyPos, Range, maps:to_list(State#state.bloons), none),
    Reply = case Closest of
        none -> {error, not_found};
        {_Dist, Pid} -> {ok, Pid}
    end,
    {reply, Reply, State};

%% GUI snapshot
handle_call(dump_positions, _From, State) ->
    {reply, {ok, maps:to_list(State#state.bloons)}, State};

handle_call(_Other, _From, State) ->
    {reply, ok, State}.

%% Track bloons
handle_cast({add_bloon, BloonPid, Pos}, State) ->
    erlang:monitor(process, BloonPid),
    {noreply, State#state{bloons = maps:put(BloonPid, Pos, State#state.bloons)}};
handle_cast({remove_bloon, BloonPid}, State) ->
    {noreply, State#state{bloons = maps:remove(BloonPid, State#state.bloons)}};
handle_cast({update_bloon_pos, BloonPid, NewPos}, State) ->
    {noreply, State#state{bloons = maps:put(BloonPid, NewPos, State#state.bloons)}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', _, process, Pid, _Reason}, State) ->
    io:format("Region ~p: Bloon ~p down~n", [State#state.id, Pid]),
    {noreply, State#state{bloons = maps:remove(Pid, State#state.bloons)}};
handle_info(_, State) ->
    {noreply, State}.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

find_closest_bloon(_MonkeyPos, _Range, [], Closest) -> Closest;
find_closest_bloon(MonkeyPos, Range, [{Pid, Pos} | Rest], Closest) ->
    Dist = distance(MonkeyPos, Pos),
    NewClosest =
        if Dist =< Range ->
               case Closest of
                   none -> {Dist, Pid};
                   {CD, _} when Dist < CD -> {Dist, Pid};
                   _ -> Closest
               end;
           true -> Closest
        end,
    find_closest_bloon(MonkeyPos, Range, Rest, NewClosest).
