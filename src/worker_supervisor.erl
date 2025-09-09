-module(worker_supervisor).
-behaviour(supervisor).

-export([start_link/1, init/1]).

-define(REGION_WIDTH, 50).
-define(TOTAL_REGIONS, 4). % Assuming 4 regions as before

start_link(RegionId) ->
    supervisor:start_link({local, name_for_region(RegionId)}, ?MODULE, [RegionId]).

init([RegionId]) ->
    io:format("Worker Supervisor starting on node ~p for region ~p~n", [node(), RegionId]),
    
    % This supervisor now only starts ONE region server.
    RegionSpec = #{
        id => {region_server, RegionId},
        start => {region_server, start_link, [RegionId, ?TOTAL_REGIONS, ?REGION_WIDTH * (RegionId + 1) - 1]},
        restart => permanent,
        type => worker
    },
    
    {ok, {#{strategy => one_for_one}, [RegionSpec]}}.

%% Helper function to create a unique registered name for each supervisor
name_for_region(RegionId) ->
    list_to_atom("worker_supervisor_" ++ integer_to_list(RegionId)).
