-module(s).
-behaviour(gen_server).

% Public API
-export([start_link/0, stop/0]).
-export([add_balloon/3, remove_balloon/1, get_closest_balloon/2, get_balloon_pos/1]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%==================================================================
% Public API Functions
%==================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

% Called by a balloon when it spawns or moves
add_balloon(Id, Pid, Pos) ->
    gen_server:cast(?MODULE, {add, Id, Pid, Pos}).

% Called by a balloon when it is popped or finishes the path
remove_balloon(Id) ->
    gen_server:cast(?MODULE, {remove, Id}).

% Called by a monkey to find a target
get_closest_balloon(MonkeyPos, Range) ->
    gen_server:call(?MODULE, {get_closest, MonkeyPos, Range}).

% Called by a dart to get its target's current position
get_balloon_pos(Id) ->
    gen_server:call(?MODULE, {get_pos, Id}).

%==================================================================
% gen_server Callbacks
%==================================================================

init([]) ->
    io:format("Game State Server is starting.~n"),
    % The state is a map of: BalloonId => {BalloonPid, BalloonPosition}
    State = maps:new(),
    {ok, State}.

handle_cast({add, Id, Pid, Pos}, State) ->
    NewState = maps:put(Id, {Pid, Pos}, State),
    {noreply, NewState};
handle_cast({remove, Id}, State) ->
    NewState = maps:remove(Id, State),
    {noreply, NewState}.

handle_call({get_closest, MonkeyPos, Range}, _From, State) ->
    Balloons = maps:to_list(State),
    BalloonsInRange = lists:filter(fun({_Id, {_Pid, Pos}}) ->
        distance(MonkeyPos, Pos) =< Range
    end, Balloons),
    case BalloonsInRange of
        [] ->
            {reply, none, State};
        [_|_] ->
            SortFun = fun({_IdA, {_PidA, PosA}}, {_IdB, {_PidB, PosB}}) ->
                          distance(MonkeyPos, PosA) =< distance(MonkeyPos, PosB)
                      end,
            [ClosestBalloon | _] = lists:sort(SortFun, BalloonsInRange),
            {Id, {Pid, _Pos}} = ClosestBalloon,
            {reply, {ok, Id, Pid}, State}
    end;
handle_call({get_pos, Id}, _From, State) ->
    case maps:find(Id, State) of
        {ok, {_Pid, Pos}} ->
            {reply, {ok, Pos}, State};
        error ->
            {reply, not_found, State}
    end.

%==================================================================
% Internal Functions
%==================================================================

distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).