%%% game.erl
%%% Convenience: compile everything and start main_server (which starts regions + GUI).

-module(game).
-export([compile_all/0, start/0]).

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
              io:format("~-18s FAILED~nE=~p~nW=~p~n", [File, E, W]),
              erlang:halt(1)
        end
      end, Modules),
    io:format("All modules compiled.~n").

start() ->
    compile_all(),
    main_server:start_link().  %% uses your 4 workers and starts the GUI
