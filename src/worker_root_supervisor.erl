-module(worker_root_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_worker_supervisor/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_worker_supervisor(RegionId) ->
    ChildId = list_to_atom("worker_supervisor_" ++ integer_to_list(RegionId)),
    supervisor:start_child(?MODULE, #{
        id => ChildId,
        start => {worker_supervisor, start_link, [RegionId]},
        restart => permanent,
        type => supervisor
    }).

init([]) ->
    io:format("Worker Root Supervisor started.~n"),
    {ok, {{one_for_one, 5, 10}, []}}.