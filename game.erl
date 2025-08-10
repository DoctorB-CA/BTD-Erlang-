-module(game).
-export([start/0, compile_all/0]).

% @doc
% Compiles and loads all the source files for the game.
compile_all() ->
    io:format("Compiling all game files...~n"),
    application:ensure_all_started(compiler),

    % The order matters. Compile modules before they are used.
    Modules = [
        w,
        region_server,
        bloon,
        arrow,
        monkey,
        main_server
    ],

    lists:foreach(
        fun(Module) ->
            File = atom_to_list(Module) ++ ".erl",
            case compile:file(File, [binary, report_errors, report_warnings]) of
                {ok, CompiledModule, Bin} ->
                    code:purge(CompiledModule),
                    code:load_binary(CompiledModule, File, Bin),
                    io:format("~-20s -> ok~n", [File]);
                {error, Errors, Warnings} ->
                    io:format("~-20s -> FAILED~n", [File]),
                    io:format("Errors: ~p~nWarnings: ~p~n", [Errors, Warnings]),
                    erlang:halt(1)
            end
        end,
        Modules
    ),
    io:format("All modules compiled and loaded.~n").

% @doc
% Starts the game by compiling and then starting the main_server.
start() ->
    compile_all(),
    io:format("Starting the main server...~n"),
    main_server:start_link().
