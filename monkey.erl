-module(monkey).
-behaviour(gen_statem).

-export([start_link/3]).
-export([init/1, callback_mode/0, searching/3, attacking/3]).

-define(SCAN_INTERVAL, 500). 
-define(ATTACK_COOLDOWN, 1000). % Cooldown is 1 second (1000 ms)

-record(data, {
    pos,
    range,
    region_pid
}).

%% API
start_link(Pos, Range, RegionPid) ->
    gen_statem:start_link(?MODULE, [Pos, Range, RegionPid], []).

%% gen_statem Callbacks
callback_mode() -> state_functions.

init([Pos, Range, RegionPid]) ->
    io:format("Monkey starting at ~p~n", [Pos]),
    Data = #data{pos=Pos, range=Range, region_pid=RegionPid},
    {ok, searching, Data, {state_timeout, 0, scan}}.

%% FIX: The state transition now includes setting the cooldown timer directly.
searching(state_timeout, scan, Data = #data{pos = MyPos}) ->
    case gen_server:call(Data#data.region_pid, {find_bloon, MyPos, Data#data.range}) of
        {ok, BloonPid} ->
            io:format("~n>>> MONKEY at ~p SHOOTS! Target: ~p~n~n", [MyPos, BloonPid]), 
            fire_arrow(MyPos, BloonPid),
            % Go to attacking state AND set the cooldown timer in one action.
            {next_state, attacking, Data, {state_timeout, ?ATTACK_COOLDOWN, cooldown_over}};
        {error, not_found} ->
            %io:format("Monkey at ~p is idle. No targets in range.~n", [MyPos]),
            {keep_state, Data, {state_timeout, ?SCAN_INTERVAL, scan}}
    end.

%% FIX: The attacking state now ONLY waits for the specific 'cooldown_over' message.
%% The 'enter' callback is no longer needed.
attacking(state_timeout, cooldown_over, Data = #data{pos = MyPos}) ->
    % The cooldown is over, now we can go back to searching.
    io:format("Monkey at ~p finished cooldown. Resuming scan.~n", [MyPos]),
    {next_state, searching, Data, {state_timeout, 0, scan}}.

%% Private
fire_arrow(MyPos, TargetPid) ->
    arrow:fire(MyPos, TargetPid).