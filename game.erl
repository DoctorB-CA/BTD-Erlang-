-module(game).
-export([play/0]).

% Main function to play the game
play() ->
    % Compile all game modules
    compile:file(run),
    % Start the game
    run:start().
