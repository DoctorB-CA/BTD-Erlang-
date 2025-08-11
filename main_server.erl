%%% main_server.erl
%%% Drives the whole simulation: ticks, ETS, darts, wave spawns, and render snapshots.

-module(main_server).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([register_gui/1, add_monkey/3, start_wave/2, add_bloon/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(MAP_WIDTH, 200).
-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, ?MAP_WIDTH div ?NUM_REGIONS).

-define(TICK_MS, 100).
-define(DART_SPEED, 10).        %% world units per tick
-define(HIT_THRESHOLD, 8).      %% world units
-define(DEFAULT_HEALTH, 3).

-record(state, {
    regions = [],               %% [{Node, RegionPid}]
    gui_subs = [],              %% [Pid]
    tick_ms = ?TICK_MS,
    next_monkey_id = 1,
    next_dart_id   = 1,
    spawns = []                 %% [{DueMs, Path, Health, Type}]
}).

%% ============ Public API ============
start_link() ->
    %% default worker nodes you gave
    start_link(['worker1@119-lnx-20','worker2@119-lnx-20','worker3@119-lnx-20','worker4@119-lnx-20']).

start_link(RegionNodes) when is_list(RegionNodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [RegionNodes], []).

register_gui(Pid) ->
    gen_server:cast(?MODULE, {register_gui, Pid}).

%% GUI -> server
add_monkey(Pos, Range, Type) ->
    gen_server:cast(?MODULE, {add_monkey, Pos, Range, Type}).

%% GUI -> server (send the FULL WORLD path + level atom)
start_wave(Level, FullWorldPath) ->
    gen_server:cast(?MODULE, {start_wave, Level, FullWorldPath}).

%% (optional) external spawns by tools/tests
add_bloon(Path, Health, Type) ->
    gen_server:cast(?MODULE, {add_bloon, Path, Health, Type}).

%% ============ gen_server ============
init([RegionNodes]) ->
    io:format("Main Server starting...~n"),
    process_flag(trap_exit, true),

    %% ETS tables
    ets:new(monkeys,      [set, public, named_table]),  %% {Id, #{id, pos, range, type, region_pid, node}}
    ets:new(darts,        [set, public, named_table]),  %% {DId, #{id, type, pos, target}}
    ets:new(bloon_types,  [set, public, named_table]),  %% {BloonPid, Type}
    ets:new(gui_stats,    [set, public, named_table]),  %% optional stats

    %% Start regions remotely
    RegionArgs = lists:map(fun(I) ->
        {I, I * ?REGION_WIDTH, I * ?REGION_WIDTH + ?REGION_WIDTH - 1}
    end, lists:seq(0, ?NUM_REGIONS - 1)),
    NodeArgs = lists:zip(RegionNodes, RegionArgs),

    RegionInfo = lists:map(
        fun({Node, {Id, StartX, EndX}}) ->
            io:format("Main: ask ~p to start region ~p (~p..~p)~n", [Node, Id, StartX, EndX]),
            case rpc:call(Node, region_server, start_remotely, [Id, StartX, EndX]) of
                {ok, Pid} -> {Node, Pid};
                Error -> erlang:error({failed_to_start_remote_node, {Node, Error}})
            end
        end, NodeArgs),

    %% Start the GUI on this node (it will register itself)
    spawn(w, start, []),

    erlang:send_after(?TICK_MS, self(), tick),
    {ok, #state{regions = RegionInfo}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast({register_gui, Pid}, State) ->
    io:format("GUI registered: ~p~n", [Pid]),
    {noreply, State#state{gui_subs = lists:usort([Pid | State#state.gui_subs])}};

handle_cast({add_monkey, Pos = {X,_}, Range, Type}, State) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    {Node, RegionPid} = lists:nth(RegionIndex + 1, State#state.regions),
    Id = State#state.next_monkey_id,
    M = #{id => Id, pos => Pos, range => Range, type => Type, region_pid => RegionPid, node => Node},
    ets:insert(monkeys, {Id, M}),
    %% Launch monkey where its region lives; monkey will scan and tell main_server to create darts
    MainNode = node(),
    _ = rpc:call(Node, monkey, start_remotely, [Pos, Range, Type, RegionPid, Id, MainNode]),
    {noreply, State#state{next_monkey_id = Id + 1}};

handle_cast({start_wave, level_1, FullWorldPath}, State) ->
    Now = erlang:monotonic_time(millisecond),
    %% 10 red bloons, 500ms apart
    Offsets = lists:seq(0, 4500, 500),
    Spawns = [{Now + T, FullWorldPath, ?DEFAULT_HEALTH, red} || T <- Offsets],
    {noreply, State#state{spawns = lists:append(State#state.spawns, Spawns)}};

handle_cast({start_wave, _OtherLevel, FullWorldPath}, State) ->
    %% default to single red
    Now = erlang:monotonic_time(millisecond),
    {noreply, State#state{spawns = [{Now, FullWorldPath, ?DEFAULT_HEALTH, red} | State#state.spawns]}};

handle_cast({add_bloon, Path = [{X,_}|_], Health, Type}, State) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    {Node, RegionPid} = lists:nth(RegionIndex + 1, State#state.regions),
    AllRegionPids = [Pid || {_N, Pid} <- State#state.regions],
    io:format("Main: spawning bloon ~p on ~p~n", [Type, Node]),
    case rpc:call(Node, bloon, start_remotely, [Path, Health, Type, RegionPid, AllRegionPids]) of
        {ok, Pid} -> ets:insert(bloon_types, {Pid, Type});
        _ -> ok
    end,
    {noreply, State};

%% Monkey tells us it shot (we simulate the dart)
handle_cast({monkey_shot, MonkeyId, FromPos, TargetPid, ArrowType}, State) ->
    DId = State#state.next_dart_id,
    Dart = #{id => DId, type => ArrowType, pos => FromPos, target => TargetPid},
    ets:insert(darts, {DId, Dart}),
    {noreply, State#state{next_dart_id = DId + 1}}.

handle_info(tick, State) ->
    Now = erlang:monotonic_time(millisecond),

    %% 1) Process due spawns
    {Due, Future} = lists:partition(fun({DueMs,_,_,_}) -> DueMs =< Now end, State#state.spawns),
    lists:foreach(fun({_, Path, Health, Type}) -> self() ! {spawn_bloon, Path, Health, Type} end, Due),

    %% 2) Move darts and resolve hits
    move_darts(),

    %% 3) Build snapshot and broadcast
    Snapshot = build_snapshot(State),
    lists:foreach(fun(G) -> G ! {render, Snapshot} end, State#state.gui_subs),

    %% 4) tick again
    erlang:send_after(State#state.tick_ms, self(), tick),
    {noreply, State#state{spawns = Future}};

handle_info({spawn_bloon, Path, Health, Type}, State) ->
    %% re-use add_bloon path
    handle_cast({add_bloon, Path, Health, Type}, State),
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

%% ===== Helpers =====

move_darts() ->
    Darts = ets:tab2list(darts),
    lists:foreach(fun({Id, D=#{
        pos := {X,Y}, target := TargetPid, type := Type}}) ->
        case catch bloon:get_pos(TargetPid) of
            {ok, {TX,TY}} ->
                DX = TX - X, DY = TY - Y,
                Dist = math:sqrt(DX*DX + DY*DY),
                case Dist < ?HIT_THRESHOLD of
                    true ->
                        %% hit!
                        TargetPid ! {hit, 1, Type},
                        ets:delete(darts, Id);
                    false ->
                        Step = ?DART_SPEED,
                        NX = X + (DX/Dist)*Step,
                        NY = Y + (DY/Dist)*Step,
                        ets:insert(darts, {Id, D#{pos => {NX, NY}}})
                end;
            _ ->
                %% target gone; remove dart
                ets:delete(darts, Id)
        end
    end, Darts).

build_snapshot(State) ->
    %% Monkeys
    Monks = [M || {_Id, M} <- ets:tab2list(monkeys)],

    %% Bloons (pull positions from regions)
    BloonPos = lists:flatmap(
                 fun({_Node, RegPid}) ->
                     case catch gen_server:call(RegPid, dump_positions) of
                         {ok, List} -> List; _ -> []
                     end
                 end, State#state.regions),
    Bloons = lists:map(
               fun({Pid, Pos}) ->
                   Type = case ets:lookup(bloon_types, Pid) of
                              [{_, T}] -> T;
                              [] -> red
                          end,
                   #{id => Pid, type => Type, pos => Pos}
               end, BloonPos),

    %% Darts
    Darts = [D || {_Id, D} <- ets:tab2list(darts)],

    #{tick => erlang:monotonic_time(millisecond),
      monkeys => Monks,
      bloons  => Bloons,
      darts   => Darts}.
