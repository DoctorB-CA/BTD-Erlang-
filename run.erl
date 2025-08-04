-module(run).
-export([start/0]).

% Main function to start the test
start() ->
    % Compile all game modules
    io:format("Compiling modules...~n"),
    compile:file(s),
    compile:file(m),
    compile:file(b),
    compile:file(d),

    % Start the game server
    io:format("Starting game server...~n"),
    s:start_link(),

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
    % Give balloons varying HP to make it more interesting
    Hp = 2 + (Id rem 3),
    BalloonId = list_to_atom("b" ++ integer_to_list(Id)),
    Path = [{0, 105}, {200, 105}],
    io:format("Spawning balloon ~p with ~p HP.~n", [BalloonId, Hp]),
    b:start_link(BalloonId, Hp, Path).
