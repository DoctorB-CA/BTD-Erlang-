-module(worker_supervisor).
-behaviour(supervisor).

-export([start_link/1, start_as_root/1, init/1]).

-define(REGION_WIDTH, 50).

start_as_root(RegionIds) ->
    spawn(fun() ->
        % Set the I/O for this new process to the local terminal.
        group_leader(whereis(user), self()),

        % Start the supervisor...
        {ok, _SupPid} = start_link(RegionIds),

        % ...and then enter an infinite loop to keep this parent process alive.
        % This prevents the supervisor from shutting down.
        loop()
    end),
    ok.

start_link(RegionIds) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [RegionIds]).

init([RegionIds]) ->
    io:format("*DEBUG* ~p: Worker Supervisor started. Managing regions: ~p~n", [node(), RegionIds]),
    TotalRegions = length(RegionIds), % Assuming RegionIds is a list like [0, 1, 2, 3]
    RegionSpecs = lists:map(
        fun(Id) ->
            #{
                id => {region_server, Id},
                start => {region_server, start_link, [Id, TotalRegions, ?REGION_WIDTH * (Id + 1) - 1]},
                restart => permanent,
                type => worker
            }
        end,
        RegionIds
    ),
    {ok, {#{strategy => one_for_one}, RegionSpecs}}.

% This is the new helper function that loops forever.
loop() ->
    receive
        _AnyMessage ->
            loop()
    end.