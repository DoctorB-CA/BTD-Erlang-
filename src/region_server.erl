-module(region_server).
-behaviour(gen_server).

-include("dbr.hrl").

-export([start_link/3, init/1, handle_call/3, handle_cast/2]).

-record(region_state, {
    id,
    total_regions,
    all_region_pids = []
}).

start_link(Id, TotalRegions, _EndX) ->
    RegionName = list_to_atom("region_" ++ integer_to_list(Id)),
    gen_server:start_link({local, RegionName}, ?MODULE, [Id, TotalRegions], []).

init([Id, TotalRegions]) ->
    io:format("Region Server ~p starting on node ~p.~n", [Id, node()]),
    % On startup, find all bloons in our region and start controllers for them.
    % This is the core of the recovery mechanism.
    start_bloon_controllers(Id),
    {ok, #region_state{id = Id, total_regions = TotalRegions, all_region_pids = []}}.

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
        % IMPORTANT: We return the Bloon's ID, not a PID.
        {_Dist, BloonId} -> {ok, BloonId}
    end,
    {reply, Reply, State};

handle_call(ping, _From, State) -> {reply, self(), State}.

handle_cast({spawn_monkey, Pos, Range}, State = #region_state{id = RegionId}) ->
    io:format("Region ~p: Spawning monkey.~n", [RegionId]),
    monkey:start_link(Pos, Range, self(), RegionId),
    {noreply, State};

handle_cast({take_control, BloonId}, State = #region_state{id = MyId}) ->
    io:format("Region ~p taking control of bloon ~p~n", [MyId, BloonId]),
    % A bloon has entered our region. Start a controller FSM for it.
    bloon:start_link(BloonId),
    {noreply, State};

handle_cast({update_region_pids, NewPids}, State = #region_state{id = MyId}) ->
    io:format("Region ~p received updated PID list.~n", [MyId]),
    % Tell all bloon controllers in this region about the new PIDs.
    broadcast_to_bloons({update_region_pids, NewPids}, MyId),
    {noreply, State#state{all_region_pids = NewPids}}.

%% ===================================================================
%% Helper Functions
%% ===================================================================

% On startup, find all bloons in this region and start their FSMs.
start_bloon_controllers(RegionId) ->
    io:format("Region ~p scanning for its bloons...~n", [RegionId]),
    BloonsInRegion = db:get_bloons_in_regions([RegionId]),
    lists:foreach(
        fun(#bloon{id = BloonId}) ->
            io:format("Region ~p starting controller for bloon ~p~n", [RegionId, BloonId]),
            bloon:start_link(BloonId)
        end,
        BloonsInRegion
    ).

distance({X1, Y1}, {X2, Y2}) -> math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).

find_closest_bloon(_, _, [], Closest) -> Closest;
find_closest_bloon(MonkeyPos, Range, [Bloon | Rest], Closest) ->
    % The bloon's position is now derived from its path and path_index
    #bloon{id = BloonId, path = Path, path_index = PathIdx} = Bloon,
    BloonPos = lists:nth(PathIdx, Path),
    
    Dist = distance(MonkeyPos, BloonPos),
    
    NewClosest = if
        Dist =< Range ->
            case Closest of
                none -> {Dist, BloonId};
                {ClosestDist, _} when Dist < ClosestDist -> {Dist, BloonId};
                _ -> Closest
            end;
        true ->
            Closest
    end,
    find_closest_bloon(MonkeyPos, Range, Rest, NewClosest).

% Send a message to all bloon FSMs controlled by this region.
broadcast_to_bloons(Message, RegionId) ->
    BloonsInRegion = db:get_bloons_in_regions([RegionId]),
    lists:foreach(
        fun(#bloon{id = BloonId}) ->
            % Find the process via the GLOBAL gproc registry and cast to it.
            BloonFsm = gproc:where({n, g, {bloon, BloonId}}),
            gen_statem:cast(BloonFsm, Message)
        end,
        BloonsInRegion
    ).
