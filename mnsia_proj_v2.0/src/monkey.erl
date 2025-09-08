-module(monkey).
-behaviour(gen_statem).

-include("db.hrl").

% EXPORTING THE CORRECT /4 ARITY FUNCTIONS
-export([start_link/4]).
-export([init/1, callback_mode/0, searching/3, attacking/3]).

-define(SCAN_INTERVAL, 100).
-define(ATTACK_COOLDOWN, 1000).
-record(data, {pos, range, region_pid}).

start_link(Pos, Range, RegionPid, RegionId) ->
    gen_statem:start_link(?MODULE, [Pos, Range, RegionPid, RegionId], []).

callback_mode() -> state_functions.

init([Pos, Range, RegionPid, RegionId]) ->
    io:format("Monkey starting at ~p on node ~p~n", [Pos, node()]),
    MonkeyId = erlang:make_ref(),
    MonkeyRecord = #monkey{id=MonkeyId, pos=Pos, range=Range, region_id=RegionId},
    db:write_monkey(MonkeyRecord),
    Data = #data{pos=Pos, range=Range, region_pid=RegionPid},
    {ok, searching, Data, {state_timeout, 0, scan}}.

% CORRECTED to be a /3 function (no _State argument)
searching(state_timeout, scan, Data = #data{pos = MyPos, range = Range, region_pid = RegionPid}) ->
    case gen_server:call(RegionPid, {find_bloon, MyPos, Range}) of
        {ok, BloonId} ->
            io:format("~n*DEBUG* MONKEY at ~p THROWS ARROW! Target: ~p~n~n", [MyPos, BloonId]),
            fire_arrow(MyPos, BloonId),
            {next_state, attacking, Data, {state_timeout, ?ATTACK_COOLDOWN, cooldown_over}};
        {error, not_found} ->
            {keep_state, Data, {state_timeout, ?SCAN_INTERVAL, scan}};
        Other ->
            io:format("*DEBUG* Monkey received unexpected reply: ~p~n", [Other]),
            {keep_state, Data, {state_timeout, ?SCAN_INTERVAL, scan}}
    end.

% CORRECTED to be a /3 function (no _State argument)
attacking(state_timeout, cooldown_over, Data = #data{pos = MyPos}) ->
    io:format("Monkey at ~p finished cooldown. Resuming scan.~n", [MyPos]),
    {next_state, searching, Data, {state_timeout, 0, scan}}.

fire_arrow(MyPos, TargetPid) ->
    arrow:fire(MyPos, TargetPid).