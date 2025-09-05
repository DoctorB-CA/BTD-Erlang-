-module(main_supervisor).
-behaviour(supervisor).
-export([start_link/1, init/1]).

start_link(AllNodes) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [AllNodes]).

init([WorkerNodes]) ->
    AllNodes = [node() | WorkerNodes],
    ok = db:init(AllNodes),
    MainServerSpec = #{
        id => main_server,
        start => {main_server, start_link, [WorkerNodes]},
        restart => permanent,
        type => worker
    },
    {ok, {#{strategy => one_for_one}, [MainServerSpec]}}.