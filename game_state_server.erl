-module(game_state_server).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([add_balloon/3, remove_balloon/1, get_closest_balloon/2]).
-export([init/1, handle_call/3, handle_cast/2]).
% --- Public API ---

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

% Register a new balloon
add_balloon(Id, Pid, Pos) ->
    gen_server:cast(?MODULE, {add, Id, Pid, Pos}).

% Unregister a balloon that has been popped or has finished the path
remove_balloon(Id) ->
    gen_server:cast(?MODULE, {remove, Id}).

% The main function a monkey will call to find a target
get_closest_balloon(MonkeyPos, Range) ->
    gen_server:call(?MODULE, {get_closest, MonkeyPos, Range}).

% --- gen_server Callbacks ---

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

    % This is the corrected logic
    case BalloonsInRange of
        [] ->
            % If no balloons are in range, reply 'none'
            {reply, none, State};

        [_|_] -> % This pattern matches a non-empty list
            % Define a function to sort balloons by distance
            SortFun = fun({_IdA, {_PidA, PosA}}, {_IdB, {_PidB, PosB}}) ->
                          distance(MonkeyPos, PosA) =< distance(MonkeyPos, PosB)
                      end,
            
            % Sort the list and get the first element (the closest)
            [ClosestBalloon | _] = lists:sort(SortFun, BalloonsInRange),

            % Extract the Id and Pid to reply to the monkey
            {Id, {Pid, _Pos}} = ClosestBalloon,
            {reply, {ok, Id, Pid}, State}
    end.


% --- Private Helper Functions ---

% Basic Euclidean distance formula
distance({X1, Y1}, {X2, Y2}) ->
    math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)).