%%% main_server.erl
%%% Boots regions on remote workers, pushes code automatically, ticks and sends GUI snapshots.

-module(main_server).
-behaviour(gen_server).

-export([start_link/1, get_regions/0, add_monkey/2, add_monkey/3, start_wave/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(MAP_WIDTH, 200).
-define(NUM_REGIONS, 4).
-define(REGION_WIDTH, ?MAP_WIDTH / ?NUM_REGIONS).
-define(TICK_MS, 200).

-record(state, {
    regions = []         %% [{Node, RegionPid}]
  , gui = []             %% [Pid]
  , monkeys = []         %% [#{id, pos, type, range, region_pid}]
  , next_monkey_id = 1
}).

%% ---------------- API ----------------
start_link(RegionNodes) when is_list(RegionNodes) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [RegionNodes], []).

get_regions() ->
    gen_server:call(?MODULE, get_regions).

%% GUI sends: add_monkey(WorldPos, Range, TypeAtom)
add_monkey(Pos, Range, Type) ->
    gen_server:cast(?MODULE, {add_monkey3, Pos, Range, Type}).

%% Backward compatible (arity-2 -> ground_monkey)
add_monkey(Pos, Range) ->
    gen_server:cast(?MODULE, {add_monkey3, Pos, Range, ground_monkey}).

%% GUI sends: start_wave(level_name, WorldPathList)
start_wave(Level, WorldPath) ->
    gen_server:cast(?MODULE, {start_wave, Level, WorldPath}).

%% ---------------- init ----------------
init([RegionNodes]) ->
    io:format("Main Server starting...~n"),
    RegionArgs = lists:map(
        fun(I) ->
            {I, I * ?REGION_WIDTH, I * ?REGION_WIDTH + ?REGION_WIDTH - 1}
        end, lists:seq(0, ?NUM_REGIONS - 1)),
    NodeArgs = lists:zip(RegionNodes, RegionArgs),

    %% Push code to workers and start region servers
    Mods = [region_server, bloon, monkey, arrow],
    RegionInfo = lists:map(
        fun({Node, {Id, StartX, EndX}}) ->
            ensure_modules_on(Node, Mods),
            io:format("Main: ask ~p to start region ~p (~p..~p)~n", [Node, Id, StartX, EndX]),
            case rpc:call(Node, region_server, start_remotely, [Id, StartX, EndX]) of
                {ok, Pid} -> {Node, Pid};
                {badrpc, Reason} ->
                    erlang:error({failed_to_start_remote_node, {Node, {badrpc, Reason}}});
                Other -> erlang:error({failed_to_start_remote_node, {Node, Other}})
            end
        end, NodeArgs),

    %% (optional) bring up the GUI locally
    spawn(w, start, []),

    State0 = #state{regions = RegionInfo},

    erlang:send_after(?TICK_MS, self(), tick),
    {ok, State0}.

%% Push modules to Node if missing (robust)
ensure_modules_on(Node, Mods) ->
    case net_adm:ping(Node) of
        pong ->
            lists:foreach(
              fun(M) ->
                  case rpc:call(Node, code, which, [M]) of
                      non_existing ->
                          case code:get_object_code(M) of
                              %% get_object_code returns {M, Bin, File}
                              {M, Bin, File} ->
                                  case rpc:call(Node, code, load_binary, [M, File, Bin]) of
                                      {module, M} -> ok;
                                      {error, Reason} ->
                                          io:format("WARN: load_binary(~p) on ~p failed: ~p~n",
                                                    [M, Node, Reason]);
                                      Other ->
                                          io:format("WARN: load_binary(~p) on ~p returned: ~p~n",
                                                    [M, Node, Other])
                                  end;
                              error ->
                                  io:format("WARN: no object code for ~p on ~p~n", [M, node()])
                          end;
                      _Path -> ok
                  end
              end, Mods),
            ok;
        pang ->
            erlang:error({nodedown, Node})
    end.

%% ---------------- calls ----------------
handle_call(get_regions, _From, State) ->
    {reply, State#state.regions, State};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

%% ---------------- casts ----------------
handle_cast({register_gui, Pid}, State) ->
    io:format("GUI registered: ~p~n", [Pid]),
    NewState = State#state{gui = lists:usort([Pid | State#state.gui])},
    send_snapshot(NewState),
    {noreply, NewState};

handle_cast({add_monkey3, Pos = {X,_Y}, Range, Type}, State) ->
    RegionIndex = trunc(X / ?REGION_WIDTH),
    {Node, RegionPid} = lists:nth(RegionIndex + 1, State#state.regions),
    Id = State#state.next_monkey_id,
    io:format("Add monkey ~p (~p) at ~p in region ~p~n", [Id, Type, Pos, RegionIndex]),
    _ = rpc:call(Node, monkey, start_remotely, [Pos, Range, RegionPid]),
    M = #{id => Id, pos => Pos, type => Type, range => Range, region_pid => RegionPid},
    NewState = State#state{
        monkeys = [M | State#state.monkeys],
        next_monkey_id = Id + 1
    },
    send_snapshot(NewState),
    {noreply, NewState};

handle_cast({start_wave, _Level, WorldPath = [{X0,_}|_]}, State) ->
    RegionIndex = trunc(X0 / ?REGION_WIDTH),
    {Node, RegionPid} = lists:nth(RegionIndex + 1, State#state.regions),
    AllRegionPids = [Pid || {_N, Pid} <- State#state.regions],
    %% simple demo: 5 bloons staggered by 400ms
    lists:foreach(
      fun(I) ->
          timer:apply_after(
            I*400, rpc, call,
            [Node, bloon, start_remotely, [WorldPath, 3, RegionPid, AllRegionPids]])
      end, lists:seq(0,4)),
    {noreply, State};

handle_cast(_Other, State) ->
    {noreply, State}.

%% ---------------- info/tick ----------------
handle_info(tick, State) ->
    send_snapshot(State),
    erlang:send_after(?TICK_MS, self(), tick),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% ---------------- snapshot ----------------
send_snapshot(State = #state{regions = Regions, gui = GUIs}) ->
    Replies = [rpc:call(Node, gen_server, call, [Pid, dump_positions], 3000)
               || {Node, Pid} <- Regions],
    BloonPos = lists:flatmap(fun ({ok, L}) -> L; (_) -> [] end, Replies),
    %% Shape GUI snapshot (types are placeholders; your logic can set real ones)
    Monkeys = State#state.monkeys,
    Bloons  = [#{id => Pid, pos => Pos, type => red} || {Pid, Pos} <- BloonPos],
    Darts   = [],
    Snapshot = #{monkeys => Monkeys, bloons => Bloons, darts => Darts},
    lists:foreach(fun(P) -> P ! {render, Snapshot} end, GUIs),
    ok.
