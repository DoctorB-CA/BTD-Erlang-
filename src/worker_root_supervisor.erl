-module(worker_root_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_worker_supervisor/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker_supervisor() ->
    supervisor:start_child(?MODULE, #{
        id => worker_supervisor,
        start => {worker_supervisor, start_link, []},
        restart => permanent,
        type => supervisor
    }).

init([]) ->
    io:format("Worker Root Supervisor started.~n"),
    {ok, {{one_for_one, 5, 10}, []}}.