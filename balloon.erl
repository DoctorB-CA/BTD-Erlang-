-module(balloon).
-behavior(gen_statem).
-export([start/1,stop/0]).
-export([init/1,callback_mode/0,terminate/3]).    %must on fsm
-export([idle/3]).
-export([b_level5/3,b_level4/3,b_level3/3,b_level2/3,b_level1/3,b_dead/3]).

-define(DEBUG, false).

start(Level) ->
    dprint('hello'),
    gen_statem:start_link({local, ?MODULE}, ?MODULE, Level, []).
init(Level) ->
    process_flag(trap_exit, true),  %for seeing debugs
    LoopData = #{b_level => Level},
    {ok, idle, LoopData}.   %next state -> is idle

idle(_,_,LoopData) ->
    Level = maps:get(b_level, LoopData),
    New_state = case Level of
        5 -> b_level5; %ballon level 5
        4 -> b_level4;
        3 -> b_level3;
        2 -> b_level2;
        1 -> b_level1;
        true       -> b_dead % ballon diead
    end,
    {next_state,New_state,[]}.

callback_mode() ->    % in hat mode are we
    state_functions.

b_level5(cast,Q,LoopData)-> ok5.

b_level4(cast,Q,LoopData)-> ok4.

b_level3(cast,Q,LoopData)-> ok3.

b_level2(cast,Q,LoopData)-> ok2.

b_level1(cast,Q,LoopData)-> ok1.

b_dead(cast,Q,LoopData) -> ok0.

terminate(_Reason, _State, _Data) -> ok.

stop()-> terminate(user_ask,none,{}).


%% @doc Prints a debug message when the ?DEBUG macro is defined.
dprint(Msg) ->
    if ?DEBUG == true ->
        io:format("DEBUG: ~p~n", [Msg]);
    true ->
        ok
    end.