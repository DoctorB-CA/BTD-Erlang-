-module(game).
-export([play/0]).

% Main function to start the test
play() ->
    % Compile all game modules
    io:format("Compiling run...~n"),

    % Start the game server
    run:start().
