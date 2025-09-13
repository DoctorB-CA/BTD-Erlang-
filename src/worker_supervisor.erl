-module(worker_supervisor).
-behaviour(supervisor).

-export([start_link/2, start_as_root/2, init/1]).

-define(REGION_WIDTH, 200).

% Start the supervisor as a detached process on a worker node
start_as_root(RegionId, TotalRegions) ->
    spawn(fun() ->
        % Set the I/O for this new process to the local terminal.
        group_leader(whereis(user), self()),

        % Start the supervisor...
        {ok, _SupPid} = start_link(RegionId, TotalRegions),

        % ...and then enter an infinite loop to keep this parent process alive.
        % This prevents the supervisor from shutting down.
        loop()
    end),
    ok.

% Standard supervisor start_link
start_link(RegionId, TotalRegions) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, {RegionId, TotalRegions}).

init({RegionId, TotalRegions}) ->
    io:format("Hello from the worker supervisor!~n"),
    io:format("*DEBUG* ~p: Worker Supervisor started. Managing region: ~p of ~p~n", [node(), RegionId, TotalRegions]),
    
    % This supervisor now only starts ONE region server, with the correct total number of regions.
    RegionSpec = #{
        id => {region_server, RegionId},
        start => {region_server, start_link, [RegionId, TotalRegions, ?REGION_WIDTH * (RegionId + 1) - 1]},
        restart => permanent,
        type => worker
    },
    
    {ok, {#{strategy => one_for_one}, [RegionSpec]}}.

% This is the new helper function that loops forever.
loop() ->
    receive
        _AnyMessage ->
            loop()
    end.