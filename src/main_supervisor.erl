-module(main_supervisor).
-behaviour(supervisor).
-export([start_link/1, init/1]).

% start_link now takes the list of worker nodes as an argument
start_link(AllWorkerNodes) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AllWorkerNodes]).

% init receives the worker nodes from start_link
init([AllWorkerNodes]) ->
    io:format("Main Supervisor starting.~n"),

    MainServerSpec = #{
        id => main_server,
        start => {main_server, start_link, [AllWorkerNodes]},
        restart => permanent,
        shutdown => 2000,
        type => worker,
        modules => [main_server]
    },

    {ok, {{one_for_one, 5, 10}, [MainServerSpec]}}.
