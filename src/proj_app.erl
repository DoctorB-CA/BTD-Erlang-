-module(proj_app).
-behaviour(application).

-include("db.hrl").

-export([start/2, stop/1]).

%% ===================================================================
%% Application Callbacks
%% ===================================================================

% This is the entry point for our entire application.
start(_StartType, _StartArgs) ->
    io:format("Starting 'proj' application...~n"),
    % Decide which supervisor to start based on the node's name.
    case node() of
        ?MAIN_NODE ->
            io:format("Node '~p' is the main node. Starting main_supervisor.~n", [node()]),
            proj:start_main_supervisor();
        _WorkerNode ->
            io:format("Node '~p' is a worker node. Trying to connect to main node...~n", [node()]),
            case net_kernel:connect_node(?MAIN_NODE) of
                true ->
                    io:format("Successfully connected to main node. Starting worker_root_supervisor.~n"),
                    proj:start_worker_root_supervisor();
                false ->
                    io:format("Error: Could not connect to main node ~p. Aborting.~n", [?MAIN_NODE]),
                    {error, could_not_connect_to_main_node}
            end
    end.

stop(_State) ->
    ok.
