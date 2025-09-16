-module(main_supervisor).
-behaviour(supervisor).
-export([start_link/0, init/1]).

% start_link no longer needs worker nodes - they connect dynamically
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

% init starts with empty worker list
init([]) ->
    io:format("Main Supervisor starting with dynamic worker discovery.~n"),

    MainServerSpec = #{
        id => main_server,
        start => {main_server, start_link, []},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [main_server]
    },

    {ok, {{one_for_one, 5, 10}, [MainServerSpec]}}.
