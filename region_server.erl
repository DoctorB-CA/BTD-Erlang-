-module(region_server).
-behaviour(gen_server).

-export([start_link/3, start_remotely/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {id, x_start, x_end, bloons = #{}}).

start_remotely(Id, StartX, EndX) ->
    start_link(Id, StartX, EndX).

start_link(Id, StartX, EndX) ->
    gen_server:start_link(?MODULE, [Id, StartX, EndX], []).

init([Id, StartX, EndX]) ->
    % FINAL FIX: Force this process to send its output to the local shell.
    erlang:group_leader(erlang:whereis(user), self()),
    
    % This message will now appear on the WORKER node.
    io:format("Region Server ~p (~p-~p) has started.~n", [Id, StartX, EndX]),
    {ok, #state{id = Id, x_start = StartX, x_end = EndX}}.
    
% ... The rest of the file is the same ...
handle_call({find_bloon, MonkeyPos, Range}, _From, State) ->
    Closest = find_closest_bloon(MonkeyPos, Range, maps:to_list(State#state.bloons), none),
    Reply = case Closest of
        none -> {error, not_found};
        {_Dist, Pid} -> {ok, Pid}
    end,
    {reply, Reply, State}.
    
handle_cast({add_bloon, BloonPid, Pos}, State) ->
    erlang:monitor(process, BloonPid),
    NewBloons = maps:put(BloonPid, Pos, State#state.bloons),
    {noreply, State#state{bloons = NewBloons}};
handle_cast({remove_bloon, BloonPid}, State) ->
    NewBloons = maps:remove(BloonPid, State#state.bloons),
    {noreply, State#state{bloons = NewBloons}};
handle_cast({update_bloon_pos, BloonPid, NewPos}, State) ->
    NewBloons = maps:update(BloonPid, NewPos, State#state.bloons),
    {noreply, State#state{bloons = NewBloons}}.

handle_info({'DOWN', _, process, Pid, _Reason}, State) ->
    io:format("Region ~p: Bloon ~p is down.~n", [State#state.id, Pid]),
    NewBloons = maps:remove(Pid, State#state.bloons),
    {noreply, State#state{bloons = NewBloons}};
handle_info(_Info, State) ->
    {noreply, State}.

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

find_closest_bloon(_, _, [], Closest) -> Closest;
find_closest_bloon(MonkeyPos, Range, [{Pid, Pos} | Rest], Closest) ->
    Dist = distance(MonkeyPos, Pos),
    NewClosest = if
        Dist =< Range ->
            case Closest of
                none -> {Dist, Pid};
                {ClosestDist, _} when Dist < ClosestDist -> {Dist, Pid};
                _ -> Closest
            end;
        true -> Closest
    end,
    find_closest_bloon(MonkeyPos, Range, Rest, NewClosest).