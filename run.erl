-module(run).
-export([start/0]).

% Main function to start the test
start() ->
    % Compile all game modules, including the new supervisor and servers
    io:format("Compiling modules...~n"),
    [compile:file(M) || M <- [game_sup, world_server, zone_server, m, b, d]],

    % Start the main game supervisor. This will start the world and all zones.
    io:format("Starting game supervisor...~n"),
    game_sup:start_link(),

    % Define different dart types
    NormalDart = #{type => normal, damage => 1, throw_interval => 1000},
    FastDart = #{type => fast, damage => 1, throw_interval => 500},

    % Start two different monkeys at different positions
    io:format("Starting monkeys...~n"),
    m:start_link(m_normal, {50, 100}, 120, NormalDart),
    m:start_link(m_fast, {150, 100}, 120, FastDart),

    io:format("Starting balloon spawner...~n"),
    % Spawn this process to create balloons periodically
    spawn(fun() -> balloon_loop(1) end),

    io:format("~n--- Test is running! Watch the console. ---~n").


% A loop to spawn balloons every 3 seconds
balloon_loop(Count) ->
    timer:sleep(3000),
    spawn_balloon(Count),
    balloon_loop(Count + 1).

% Helper function to spawn a single balloon
spawn_balloon(Id) ->
    % Adjusted HP to a more reasonable level for testing
    Hp = 2 + (Id rem 5),
    BalloonId = list_to_atom("b" ++ integer_to_list(Id)),
    % Path kept within the 0-399 world boundaries
    Path = [{0,50},{0,100},{0,150},{0,200},{0,250},{0,300},{0,350}],
    io:format("Spawning balloon ~p with ~p HP.~n", [BalloonId, Hp]),
    b:start_link(BalloonId, Hp, Path).
