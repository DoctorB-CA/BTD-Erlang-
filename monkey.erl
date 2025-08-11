%%% monkey.erl
%%% Scans its region; when it finds a target, tells main_server to spawn a dart.

-module(monkey).
-behaviour(gen_statem).

-export([start_remotely/6, start_link/6]).
-export([init/1, callback_mode/0, searching/3, attacking/3]).

-define(SCAN_INTERVAL, 500).
-define(ATTACK_COOLDOWN, 1000).

-record(data, {id, pos, range, region_pid, type, main_node}).

start_remotely(Pos, Range, Type, RegionPid, Id, MainNode) ->
    start_link(Pos, Range, Type, RegionPid, Id, MainNode).

start_link(Pos, Range, Type, RegionPid, Id, MainNode) ->
    gen_statem:start_link(?MODULE, [Pos, Range, Type, RegionPid, Id, MainNode], []).

callback_mode() -> state_functions.

init([Pos, Range, Type, RegionPid, Id, MainNode]) ->
    io:format("Monkey ~p (~p) at ~p~n", [Id, Type, Pos]),
    Data = #data{id=Id, pos=Pos, range=Range, region_pid=RegionPid, type=Type, main_node=MainNode},
    {ok, searching, Data, {state_timeout, 0, scan}}.

searching(state_timeout, scan, Data = #data{pos=MyPos, region_pid=Reg, range=R}) ->
    case gen_server:call(Reg, {find_bloon, MyPos, R}) of
        {ok, BloonPid} ->
            ArrowType = arrow_type_for(Data#data.type),
            io:format("Monkey ~p shoots ~p at ~p~n", [Data#data.id, ArrowType, BloonPid]),
            gen_server:cast({main_server, Data#data.main_node},
                            {monkey_shot, Data#data.id, MyPos, BloonPid, ArrowType}),
            {next_state, attacking, Data, {state_timeout, ?ATTACK_COOLDOWN, cooldown_over}};
        {error, not_found} ->
            {keep_state, Data, {state_timeout, ?SCAN_INTERVAL, scan}}
    end.

attacking(state_timeout, cooldown_over, Data) ->
    {next_state, searching, Data, {state_timeout, 0, scan}}.

%% >>> Add air_monkey here <<<
arrow_type_for(ground_monkey) -> ground;
arrow_type_for(fire_monkey)   -> fire;
arrow_type_for(water_monkey)  -> water;
arrow_type_for(air_monkey)    -> air;
arrow_type_for(avatar_monkey) ->
    case rand:uniform(4) of 1 -> ground; 2 -> fire; 3 -> water; 4 -> air end.
