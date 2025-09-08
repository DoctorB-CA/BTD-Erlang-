-module(main_supervisor).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("Main Supervisor starting.~n"),

    % This supervisor now starts the database AND the main_server.
    % It no longer needs to know about the worker nodes.
    Children = [
        #{
            id => db,
            start => {db, start_link, []},
            restart => permanent,
            type => worker
        },
        #{
            id => main_server,
            start => {main_server, start_link, []},
            restart => permanent,
            type => worker
        }
    ],

    {ok, {{one_for_one, 5, 10}, Children}}.
