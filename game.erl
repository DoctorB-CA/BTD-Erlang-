%%% game.erl
-module(game).
-export([start/0, compile_all/0]).

compile_all() ->
    io:format("Compiling...~n"),
    application:ensure_all_started(compiler),
    Modules = [w, region_server, bloon, arrow, monkey, main_server],
    lists:foreach(
      fun(M) ->
          File = atom_to_list(M) ++ ".erl",
          case compile:file(File, [binary, report_errors, report_warnings]) of
            {ok, Mod, Bin} ->
                code:purge(Mod),
                code:load_binary(Mod, File, Bin),
                io:format("~-18s ok~n", [File]);
            {error, E, W} ->
                io:format("~-18s FAILED~nErrors: ~p~nWarnings: ~p~n", [File, E, W]),
                erlang:halt(1)
          end
      end, Modules),
    io:format("All modules compiled.~n").

start() ->
    compile_all(),
    io:format("Main Server starting...~n"),
    main_server:start_link([
        'worker1@119-lnx-20',
        'worker2@119-lnx-20',
        'worker3@119-lnx-20',
        'worker4@119-lnx-20'
    ]).
