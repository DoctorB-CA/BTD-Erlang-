-module(run).
-export([start/0]).

start() ->
    io:format("Compiling modules...~n"),
    [compile:file(M) || M <- [game_sup, world_server, zone_server, m, b, d]],

    io:format("Starting game supervisor...~n"),
    game_sup:start_link(),

    NormalDart = #{type => normal, damage => 1, throw_interval => 1000},
    FastDart = #{type => fast, damage => 1, throw_interval => 500},

    io:format("Starting monkeys...~n"),
    m:start_link(m_normal, {50, 100}, 120, NormalDart),
    m:start_link(m_fast, {250, 100}, 120, FastDart),

    io:format("Starting balloon spawner...~n"),
    spawn(fun() -> balloon_loop(1) end),

    io:format("~n--- Test is running! Watch the console. ---~n").

balloon_loop(Count) ->
    timer:sleep(3000),
    spawn_balloon(Count),
    balloon_loop(Count + 1).

spawn_balloon(Id) ->
    Hp = 5 + (Id rem 5),
    BalloonId = list_to_atom("b" ++ integer_to_list(Id)),
    Path = [{0,50},{350,50}],
    io:format("Spawning balloon ~p with ~p HP.~n", [BalloonId, Hp]),
    b:start_link(BalloonId, Hp, Path).