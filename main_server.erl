-module(main_server).
-behaviour(gen_server).
-export([start_link/0, stop/0, generate_level1/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_balloon/3, move_balloon/2, delete_balloon/1]). % Keep these for now

%% NEW: Updated records for game objects
-record(monkey, {id, type, x, y, range=150, cooldown=0}).
-record(balloon, {id, type, path, hp, path_index=1}).
-record(dart, {id, type, path, path_index=1, target_id}).

-define(TICK_RATE, 50). % ms per game tick
-define(MONKEY_COOLDOWN, 20). % 20 ticks = 1 second cooldown
-define(DART_STEPS, 10). % Dart path will have 10 steps

%% API and other functions...
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:cast(?MODULE, stop).
generate_level1() -> gen_server:cast(?MODULE, generate_level1).
add_balloon(I,T,SP) -> gui:add_balloon(I,T,SP).
move_balloon(I,NP) -> gui:move_balloon(I,NP).
delete_balloon(I) -> gui:delete_balloon(I).

%% Internal Helper Functions
get_dart_type(avatar_monkey) ->
    Darts = [ground_dart, water_dart, fire_dart, air_dart],
    lists:nth(rand:uniform(length(Darts)), Darts);
get_dart_type(ground_monkey) -> ground_dart;
get_dart_type(water_monkey) -> water_dart;
get_dart_type(fire_monkey) -> fire_dart;
get_dart_type(air_monkey) -> air_dart;
get_dart_type(_) -> ground_dart.

calculate_distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2-X1, 2) + math:pow(Y2-Y1, 2)).

calculate_path({Sx, Sy}, {Ex, Ey}, Steps) ->
    lists:map(
        fun(I) ->
            Frac = I / Steps,
            X = round(Sx + (Ex - Sx) * Frac),
            Y = round(Sy + (Ey - Sy) * Frac),
            {X, Y}
        end,
        lists:seq(1, Steps)
    ).

%% Server Callbacks
init([]) ->
    ets:new(game_objects, [set, public, named_table, {keypos, #balloon.id}]),
    {ok, _} = gui:start_link(),
    erlang:send_after(?TICK_RATE, self(), tick),
    {ok, #{}}.

handle_cast(create_red_balloon, State) ->
    Id = make_ref(), Path = p:get_path(),
    NewBalloon = #balloon{id=Id, type=red, path=Path, hp=100},
    ets:insert(game_objects, NewBalloon),
    {StartX, StartY} = lists:nth(1, Path),
    add_balloon(Id, red, {StartX, StartY}),
    {noreply, State};

handle_cast({place_item, {Type, X, Y}}, State) ->
    Id = make_ref(),
    NewMonkey = #monkey{id=Id, type=Type, x=X, y=Y}, % Add with default range and cooldown
    ets:insert(game_objects, NewMonkey),
    gui:add_monkey(Type, X, Y, Id),
    {noreply, State};
handle_cast(generate_level1, S) -> spawn(fun()->lists:foreach(fun(_)->gen_server:cast(main_server,create_red_balloon),timer:sleep(500)end,lists:seq(1,10))end),{noreply,S};
handle_cast(stop, S) -> {stop, normal, S};
handle_cast(_, S) -> {noreply, S}.

%% THE NEW GAME LOOP
handle_info(tick, State) ->
    %% 1. MOVE BALLOONS
    AllBalloons = ets:match_object(game_objects, #balloon{_ = '_'}),
    lists:foreach(fun(B) -> move_one_balloon(B) end, AllBalloons),

    %% 2. MOVE DARTS & CHECK HITS
    AllDarts = ets:match_object(game_objects, #dart{_ = '_'}),
    lists:foreach(fun(D) -> move_one_dart(D) end, AllDarts),

    %% 3. MONKEYS FIND TARGETS & SHOOT
    AllMonkeys = ets:match_object(game_objects, #monkey{_ = '_'}),
    FreshBalloons = ets:match_object(game_objects, #balloon{_ = '_'}), % Get current balloon list
    lists:foreach(fun(M) -> handle_one_monkey(M, FreshBalloons) end, AllMonkeys),

    gui:refresh(),
    erlang:send_after(?TICK_RATE, self(), tick),
    {noreply, State};

handle_info(_,S) -> {noreply,S}.

%% Game Loop Helper Functions
move_one_balloon(B = #balloon{id=Id, path=Path, path_index=Index}) ->
    NewIndex = Index + 1,
    if NewIndex > length(Path) -> ets:delete(game_objects, Id), delete_balloon(Id);
       true ->
        ets:insert(game_objects, B#balloon{path_index = NewIndex}),
        {NewX, NewY} = lists:nth(NewIndex, Path),
        move_balloon(Id, {NewX, NewY})
    end.

move_one_dart(D = #dart{id=Id, path=Path, path_index=Index, target_id=TargetId}) ->
    NewIndex = Index + 1,
    if NewIndex > length(Path) -> % Dart reached end of path (a "hit")
        ets:delete(game_objects, Id),
        gui:delete_dart(Id),
        case ets:lookup(game_objects, TargetId) of
            [] -> ok; % Balloon already gone
            _ -> ets:delete(game_objects, TargetId), delete_balloon(TargetId)
        end;
       true ->
        ets:insert(game_objects, D#dart{path_index = NewIndex}),
        {NewX, NewY} = lists:nth(NewIndex, Path),
        gui:move_dart(Id, {NewX, NewY})
    end.

handle_one_monkey(M = #monkey{cooldown=C, x=Mx, y=My, range=Range, type=MType, id=MId}, Balloons) ->
    NewCooldown = if C > 0 -> C - 1; true -> 0 end,
    ets:insert(game_objects, M#monkey{cooldown = NewCooldown}),
    if NewCooldown == 0 andalso length(Balloons) > 0 -> % Can shoot
        % Find first balloon in range
        Target = lists:keyfind(true, 1,
            lists:map(fun(B = #balloon{path=P, path_index=I}) ->
                BPos = lists:nth(I, P),
                {calculate_distance({Mx,My}, BPos) =< Range, B}
            end, Balloons)),
        case Target of
            {true, TargetBalloon} -> % Found a target in range
                ets:insert(game_objects, M#monkey{cooldown = ?MONKEY_COOLDOWN}),
                shoot_dart(M, TargetBalloon);
            _ -> ok % No target in range
        end;
       true -> ok % On cooldown
    end.

shoot_dart(#monkey{x=Sx, y=Sy, type=MType}, #balloon{id=TargetId, path=Path, path_index=Index}) ->
    DartId = make_ref(),
    DartType = get_dart_type(MType),
    TargetPos = lists:nth(Index, Path),
    DartPath = calculate_path({Sx, Sy}, TargetPos, ?DART_STEPS),
    NewDart = #dart{id=DartId, type=DartType, path=DartPath, target_id=TargetId},
    ets:insert(game_objects, NewDart),
    gui:add_dart(DartId, DartType, {Sx, Sy}).

terminate(_,_) -> ok.
handle_call(_,_,S) -> {reply,ok,S}.
code_change(_,S,_) -> {ok,S}.
