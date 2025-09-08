-module(worker_supervisor).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(REGION_WIDTH, 50).
-define(TOTAL_REGIONS, 4). % Assuming 4 regions as before

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    % The worker supervisor now determines its own region ID based on its node name.
    RegionId = get_region_id_from_node(node()),
    io:format("Worker Supervisor starting on node ~p for region ~p~n", [node(), RegionId]),
    
    % This supervisor now only starts ONE region server.
    RegionSpec = #{
        id => {region_server, RegionId},
        start => {region_server, start_link, [RegionId, ?TOTAL_REGIONS, ?REGION_WIDTH * (RegionId + 1) - 1]},
        restart => permanent,
        type => worker
    },
    
    {ok, {#{strategy => one_for_one}, [RegionSpec]}}.

%% Helper function to parse the node name 'worker1@host' -> 0
get_region_id_from_node(Node) ->
    [WorkerName | _] = string:split(atom_to_list(Node), "@"),
    % "worker1" -> "1"
    IndexStr = string:trim(WorkerName, leading, "worker"),
    % "1" -> 1
    Index = list_to_integer(IndexStr),
    % Convert 1-based index to 0-based region ID
    Index - 1.
