-module(game_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
    ChildSpecs = [
        #{
            id => world_server,
            start => {world_server, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [world_server]
        }
    ],
    {ok, {SupFlags, ChildSpecs}}.
