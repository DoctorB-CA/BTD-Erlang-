%%% arrow.erl
%%% Not used by the live loop (main_server simulates darts), but kept for parity/tests.

-module(arrow).
-export([fire/3]).

-define(SPEED, 10).
-define(TICK_RATE, 50).
-define(MAX_RANGE, 300).
-define(HIT_THRESHOLD, 8).

fire(_Type, _StartPos, _TargetPid) ->
    spawn(fun() -> ok end).  %% no-op; main_server owns dart sim now.
