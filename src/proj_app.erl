-module(proj_app).
-behaviour(application).

-export([start/2, stop/1]).

%% ===================================================================
%% Application Callbacks
%% ===================================================================

% This is the entry point for our entire application.
start(_StartType, _StartArgs) ->
    io:format("Starting 'proj' application...~n"),
    % Decide which supervisor to start based on the node's name.
    case is_main_node(node()) of
        true ->
            io:format("Node ~p is the main node. Starting main_supervisor.~n", [node()]),
            main_supervisor:start_link();
        false ->
            io:format("Node '~p' is a worker node. Waiting for main server to assign work.~n", [node()]),
            % On a worker node, we just start the application and its dependencies.
            % The main_server will use RPC to start the worker_supervisor later.
            ok
    end.

stop(_State) ->
    ok.

%% ===================================================================
%% Internal Functions
%% ===================================================================

% Check if the current node is the main node by looking at its name.
is_main_node(Node) ->
    case string:split(atom_to_list(Node), "@") of
        ["main", _Host] -> true;
        _ -> false
    end.
