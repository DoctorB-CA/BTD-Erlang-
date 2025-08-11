-module(game).
-export([start/0, compile_all/0]).

compile_all() ->
    io:format("Compiling all game files...~n"),
    application:ensure_all_started(compiler),
    Modules = [w, region_server, bloon, arrow, monkey, main_server],
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

start() ->
    compile_all(),
    io:format("Starting the main server on workers...~n"),
    main_server:start_link([
        'worker1@119-lnx-20',
        'worker2@119-lnx-20',
        'worker3@119-lnx-20',
        'worker4@119-lnx-20'
    ]).
