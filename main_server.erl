-module(main_server).
-behaviour(gen_server).
-export([start_link/0, stop/0, generate_level1/0, add_bananas/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([add_balloon/3, move_balloon/2, delete_balloon/1, change_bananas/1]).

-record(monkey, {id, type, x, y, range=150, cooldown=0}).
-record(balloon, {id, type, path, hp, path_index=1}).
-record(dart, {id, type, path, path_index=1, target_id}).

-define(TICK_RATE, 50).
-define(MONKEY_COOLDOWN, 20).
-define(DART_STEPS, 10).

%% Public API and API Layer are unchanged...
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_server:cast(?MODULE, stop).
generate_level1() -> gen_server:cast(?MODULE, generate_level1).
add_bananas(Amount) when is_integer(Amount) -> CurrentBananas = get_bananas(), NewBananas = CurrentBananas + Amount, ets:insert(game_objects, {meta, bananas, NewBananas}), change_bananas(NewBananas), io:format("SERVER: Bananas changed by ~p. New total: ~p~n", [Amount, NewBananas]), NewBananas.
add_balloon(I,T,SP) -> gui:add_balloon(I,T,SP).
move_balloon(I,NP) -> gui:move_balloon(I,NP).
delete_balloon(I) -> gui:delete_balloon(I).
change_bananas(Amount) -> gui:change_bananas(Amount).

%% Internal Helpers
get_dart_type(avatar_monkey)->Darts=[ground_dart,water_dart,fire_dart,air_dart],lists:nth(rand:uniform(length(Darts)),Darts);
get_dart_type(ground_monkey)->ground_dart; get_dart_type(water_monkey)->water_dart; get_dart_type(fire_monkey)->fire_dart;
get_dart_type(air_monkey)->air_dart; get_dart_type(_)->ground_dart.
calculate_distance({X1,Y1},{X2,Y2})->math:sqrt(math:pow(X2-X1,2)+math:pow(Y2-Y1,2)).
calculate_path({Sx,Sy},{Ex,Ey},Steps)->lists:map(fun(I)->F=I/Steps,X=round(Sx+(Ex-Sx)*F),Y=round(Sy+(Ey-Sy)*F),{X,Y}end,lists:seq(1,Steps)).
get_bananas()->case ets:lookup(game_objects,bananas)of[{meta,bananas,A}]->A;[]->0 end.
get_monkey_cost(_)->40.
can_buy_monkey(MonkeyType)->get_bananas()>=get_monkey_cost(MonkeyType).
destroy_balloon(BalloonId)->case ets:member(game_objects,BalloonId)of true->Reward=2,add_bananas(Reward),ets:delete(game_objects,BalloonId),delete_balloon(BalloonId);false->ok end.

%% Server Callbacks
init([])->
    ets:new(game_objects,[set,public,named_table,{keypos,#balloon.id}]),
    rand:seed(exsplus),
    ets:insert(game_objects,{meta,bananas,100}),
    {ok,_} = gui:start_link(),
    change_bananas(get_bananas()),
    io:format("SERVER: Game started with ~p bananas.~n",[get_bananas()]),
    erlang:send_after(?TICK_RATE,self(),tick),
    {ok,#{}}.

handle_cast({place_item,{Type,X,Y}},State)->case can_buy_monkey(Type)of true->Cost=get_monkey_cost(Type),NewTotal=add_bananas(-Cost),io:format("SERVER: Bought ~p for ~p bananas. ~p remaining.~n",[Type,Cost,NewTotal]),Id=make_ref(),NewMonkey=#monkey{id=Id,type=Type,x=X,y=Y},ets:insert(game_objects,NewMonkey),gui:add_monkey(Type,X,Y,Id);false->io:format("SERVER: Not enough bananas to buy ~p! You have ~p.~n",[Type,get_bananas()])end,{noreply,State};
handle_cast(create_red_balloon,S)->I=make_ref(),P=p:get_path(),NB=#balloon{id=I,type=red,path=P,hp=100},ets:insert(game_objects,NB),{SX,SY}=lists:nth(1,P),add_balloon(I,red,{SX,SY}),{noreply,S};
handle_cast(generate_level1,S)->spawn(fun()->lists:foreach(fun(_)->gen_server:cast(main_server,create_red_balloon),timer:sleep(500)end,lists:seq(1,10))end),{noreply,S};
handle_cast(stop,S)->{stop,normal,S};
handle_cast(_,S)->{noreply,S}.

handle_info(tick, S) ->
    AllBalloons = ets:match_object(game_objects, #balloon{_ = '_'}),
    lists:foreach(fun(B) -> move_one_balloon(B) end, AllBalloons),
    AllDarts = ets:match_object(game_objects, #dart{_ = '_'}),
    lists:foreach(fun(D) -> move_one_dart(D) end, AllDarts),
    AllMonkeys = ets:match_object(game_objects, #monkey{_ = '_'}),
    FreshBalloons = ets:match_object(game_objects, #balloon{_ = '_'}),
    CurrentDarts = ets:match_object(game_objects, #dart{_ = '_'}),
    lists:foreach(fun(M) -> handle_one_monkey(M, FreshBalloons, CurrentDarts) end, AllMonkeys),
    gui:refresh(),
    erlang:send_after(?TICK_RATE, self(), tick),
    {noreply, S};
handle_info(_, S) -> {noreply, S}.

%% Game Loop Helper Functions
move_one_balloon(B=#balloon{id=I,path=P,path_index=Idx})->NI=Idx+1,if NI>length(P)->destroy_balloon(I);true->ets:insert(game_objects,B#balloon{path_index=NI}),{NX,NY}=lists:nth(NI,P),move_balloon(I,{NX,NY})end.
move_one_dart(D=#dart{id=I,path=P,path_index=Idx,target_id=TID})->NI=Idx+1,if NI>length(P)->ets:delete(game_objects,I),gui:delete_dart(I),destroy_balloon(TID);true->ets:insert(game_objects,D#dart{path_index=NI}),{NX,NY}=lists:nth(NI,P),gui:move_dart(I,{NX,NY})end.

handle_one_monkey(M = #monkey{cooldown=C, x=Mx, y=My, range=R}, Balloons, Darts) ->
    NewCooldown = if C > 0 -> C - 1; true -> 0 end,
    ets:insert(game_objects, M#monkey{cooldown = NewCooldown}),
    if NewCooldown == 0 andalso length(Balloons) > 0 ->
           BalloonsInRange = lists:filter(
               %% ✅ FIX: Change B to _B to silence warning.
               fun(_B = #balloon{path=P, path_index=I}) ->
                   BPos = lists:nth(I, P),
                   calculate_distance({Mx,My}, BPos) =< R
               end, Balloons),

           TargetedBalloonIds = [D#dart.target_id || D <- Darts],

           AvailableTargets = lists:filter(
               %% ✅ FIX: Change B to _B to silence warning.
               fun(_B = #balloon{id=Id}) ->
                   not lists:member(Id, TargetedBalloonIds)
               end, BalloonsInRange),

           case AvailableTargets of
               [FirstTarget | _] ->
                   ets:insert(game_objects, M#monkey{cooldown = ?MONKEY_COOLDOWN}),
                   shoot_dart(M, FirstTarget);
               [] -> ok
           end;
       true -> ok
    end.

shoot_dart(#monkey{x=Sx, y=Sy, type=MType}, #balloon{id=TargetId, path=Path, path_index=Index}) ->
    DartId = make_ref(),
    DartType = get_dart_type(MType),
    TargetPos = lists:nth(Index, Path),
    DartPath = calculate_path({Sx, Sy}, TargetPos, ?DART_STEPS),
    %% ✅ FIX: Changed DT to DartType and DP to DartPath
    NewDart = #dart{id=DartId, type=DartType, path=DartPath, target_id=TargetId},
    ets:insert(game_objects, NewDart),
    gui:add_dart(DartId, DartType, {Sx, Sy}).

terminate(_,_) -> ok.
handle_call(_,_,S) -> {reply,ok,S}.
code_change(_,S,_) -> {ok,S}.
