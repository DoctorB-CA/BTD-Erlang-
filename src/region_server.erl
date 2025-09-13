-module(region_server).
-behaviour(gen_server).

-include("dbr.hrl").

-export([start_link/3, init/1, handle_call/3, handle_cast/2]).

% The state no longer needs to track bloons. It only needs its own ID.
-record(region_state, {id, total_regions}).

start_link(Id, TotalRegions, EndX) ->
    RegionName = list_to_atom("region_" ++ integer_to_list(Id)),
    gen_server:start_link({local, RegionName}, ?MODULE, [Id, TotalRegions, EndX], []).

init([Id, TotalRegions, _EndX]) ->
    io:format("*DEBUG* Region Server ~p started on node ~p.~n", [Id, node()]),
    {ok, #region_state{id = Id, total_regions = TotalRegions}}.

handle_call({find_bloon, MonkeyPos, Range}, _From, State = #region_state{id = MyId, total_regions = Total}) ->
    % Determine which regions to scan (self, previous, and next)
    RegionsToScan = lists:usort([
        MyId,
        if MyId > 0 -> MyId - 1; true -> MyId end,
        if MyId < Total - 1 -> MyId + 1; true -> MyId end
    ]),
    
    % Fetch bloons from the database for all relevant regions
    BloonRecords = db:get_bloons_in_regions(RegionsToScan),
    
    % Find the closest bloon from the fetched records
    Closest = find_closest_bloon(MonkeyPos, Range, BloonRecords, none),
    
    Reply = case Closest of
        none -> {error, not_found};
        {_Dist, BloonPid} -> {ok, BloonPid}
    end,
    {reply, Reply, State};

handle_call(ping, _From, State) -> {reply, self(), State}.

handle_cast({spawn_monkey,MT, Pos, Range}, State = #region_state{id = RegionId}) ->
    io:format("~p*DEBUG* ~p: Spawning monkey.~n", [MT,node()]),
    monkey:start_link(MT,Pos, Range, self(), RegionId),
    {noreply, State};

handle_cast({spawn_bloon, Path, Health, AllRegionPids, RegionId}, State) ->
    bloon:start_link(Path, Health, self(), AllRegionPids, RegionId),
    {noreply, State}.

% Helper functions
distance({X1, Y1}, {X2, Y2}) -> math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

find_closest_bloon(_, _, [], Closest) -> Closest;
find_closest_bloon(MonkeyPos, Range, [Bloon | Rest], Closest) ->
    % The bloon's position is now derived from its path and path_index
    #bloon{id=Pid, health=_Health, index=Idx, pos=Pos, region_id=_RegionId} = Bloon,
    BloonPos = lists:nth(Idx, Pos),
    
    Dist = distance(MonkeyPos, BloonPos),
    
    NewClosest = if
        Dist =< Range ->
            case Closest of
                none -> {Dist, Pid};
                {ClosestDist, _} when Dist < ClosestDist -> {Dist, Pid};
                _ -> Closest
            end;
        true ->
            Closest
    end,
    find_closest_bloon(MonkeyPos, Range, Rest, NewClosest).