-module(monkey).
-behaviour(gen_statem).

-export([start_link/3, start_remotely/3]).
-export([init/1, callback_mode/0, searching/3, attacking/3]).

-define(SCAN_INTERVAL, 500).
-define(ATTACK_COOLDOWN, 1000).
-record(data, {pos, range, region_pid}).

%% Launcher function called by main_server
start_remotely(Pos, Range, RegionPid) ->
    start_link(Pos, Range, RegionPid).

%% API
start_link(Pos, Range, RegionPid) ->
    gen_statem:start_link(?MODULE, [Pos, Range, RegionPid], []).

callback_mode() ->
    state_functions.

%% gen_statem Callbacks
init([Pos, Range, RegionPid]) ->
    io:format("Monkey starting at ~p~n", [Pos]),
    Data = #data{pos=Pos, range=Range, region_pid=RegionPid},
    {ok, searching, Data, {state_timeout, 0, scan}}.

searching(state_timeout, scan, Data = #data{pos = MyPos}) ->
    case gen_server:call(Data#data.region_pid, {find_bloon, MyPos, Data#data.range}) of
        {ok, BloonPid} ->
            % --- PRINT STATEMENT FOR FIRING AN ARROW ---
            io:format("~n>>> MONKEY at ~p THROWS ARROW! Target: ~p~n~n", [MyPos, BloonPid]),
            fire_arrow(MyPos, BloonPid),
            {next_state, attacking, Data, {state_timeout, ?ATTACK_COOLDOWN, cooldown_over}};
        {error, not_found} ->
            io:format("Monkey at ~p is idle. No targets in range.~n", [MyPos]),
            {keep_state, Data, {state_timeout, ?SCAN_INTERVAL, scan}}
    end.

attacking(state_timeout, cooldown_over, Data = #data{pos = MyPos}) ->
    io:format("Monkey at ~p finished cooldown. Resuming scan.~n", [MyPos]),
    {next_state, searching, Data, {state_timeout, 0, scan}}.

%% Private
fire_arrow(MyPos, TargetPid) ->
    arrow:fire(MyPos, TargetPid).