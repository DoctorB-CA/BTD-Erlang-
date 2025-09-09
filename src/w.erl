-module(w).
-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([gui/1]). % Export the new GUI entry point

-include_lib("wx/include/wx.hrl").
-include("dbr.hrl").

-define(SERVER, ?MODULE).
-define(WINDOW_WIDTH, 850).
-define(WINDOW_HEIGHT, 750).
-define(BUTTON_WIDTH, 80).
-define(BUTTON_HEIGHT, 80).
-define(CANVAS_WIDTH, 800).
-define(CANVAS_HEIGHT, 600).
-define(BALLOON_WIDTH, 40).
-define(BALLOON_HEIGHT, 40).
-define(TICK_INTERVAL, 100). % Refresh rate in milliseconds

-record(state, {
    gui_pid, % To hold the PID of the GUI process
    bitmaps,
    current_brush = unselected,
    monkeys = [],
    bloons = [],
    path
}).

%% ===================================================================
%% Public API & Server Start
%% ===================================================================
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
    % Spawn the dedicated GUI process, passing our (the server's) PID
    GuiPid = spawn_link(fun() -> gui(self()) end),
    timer:send_after(?TICK_INTERVAL, self(), tick),
    {ok, #state{gui_pid = GuiPid}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({init_gui, Bitmaps, Path}, _From, State) ->
    % The GUI process tells us it's ready and gives us the static data
    {reply, ok, State#state{bitmaps = Bitmaps, path = Path}}.

handle_cast({set_brush, Brush}, State) ->
    {noreply, State#state{current_brush = Brush}};
handle_cast({place_monkey, Pos}, State = #state{current_brush = Brush}) ->
    io:format("Server received place_monkey at ~p with brush ~p~n", [Pos, Brush]),
    if Brush /= unselected ->
           main_server:add_monkey(Pos, 100, Brush);
       true ->
           ok
    end,
    {noreply, State#state{current_brush = unselected}};
handle_cast(start_wave, State = #state{path = Path}) ->
    io:format("Server received start_wave~n", []),
    main_server:add_bloon(Path, red_bloon),
    {noreply, State}.

handle_info(tick, State = #state{gui_pid = GuiPid}) ->
    NewState = case main_server:get_game_state() of
                   #{monkeys := Monkeys, bloons := Bloons} ->
                       State#state{monkeys = Monkeys, bloons = Bloons};
                   _ ->
                       State
               end,
    % Tell the GUI process to refresh
    GuiPid ! refresh,
    timer:send_after(?TICK_INTERVAL, self(), tick),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%% ===================================================================
%% GUI Process (runs completely separately from the gen_server)
%% ===================================================================
gui(ServerPid) ->
    wx:new(),
    wxImage:initStandardHandlers(),

    ImagesPath = "/home/csestudent/Desktop/bar/GUI_PROJ/BTD-Erlang-/images/",
    GroundBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "ground_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    FireBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "fire_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    AirBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "air_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    WaterBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "water_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    MapBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "map1.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT)),
    RedBalloonBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "red_balloon.png"), ?BALLOON_WIDTH, ?BALLOON_HEIGHT)),
    BlueBalloonBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "blue_balloon.png"), ?BALLOON_WIDTH, ?BALLOON_HEIGHT)),
    GreenBalloonBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "green_balloon.png"), ?BALLOON_WIDTH, ?BALLOON_HEIGHT)),
    BlackBalloonBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "black_balloon.png"), ?BALLOON_WIDTH, ?BALLOON_HEIGHT)),

    Bitmaps = #{
        ground_monkey => GroundBitmap, fire_monkey => FireBitmap, air_monkey => AirBitmap, water_monkey => WaterBitmap,
        map => MapBitmap,
        red_bloon => RedBalloonBitmap, blue_bloon => BlueBalloonBitmap, green_geo_bloon => GreenBalloonBitmap, black_bloon => BlackBalloonBitmap
    },
    Path = [{10, 100},{20, 100},{30, 100},{40, 100},{50, 100},{60, 100}, {70, 110}, {80, 100}, {120, 90}, {180, 100}],

    % Tell the server we are ready and give it the static data
    gen_server:call(ServerPid, {init_gui, Bitmaps, Path}),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "BTD-Erlang"),
    BackgroundPanel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(BackgroundPanel, {220, 220, 220}),

    GroundButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, GroundBitmap),
    FireButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, FireBitmap),
    AirButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, AirBitmap),
    WaterButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, WaterBitmap),
    CanvasPanel = wxPanel:new(BackgroundPanel, [{style, ?wxBORDER_SUNKEN}, {size, {?CANVAS_WIDTH, ?CANVAS_HEIGHT}}]),
    StartWaveButton = wxButton:new(BackgroundPanel, ?wxID_ANY, [{label, "Start Wave"}]),

    TopRowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(TopRowSizer, GroundButton, [{flag, ?wxLEFT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),
    wxSizer:add(TopRowSizer, FireButton, [{flag, ?wxLEFT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),
    wxSizer:add(TopRowSizer, AirButton, [{flag, ?wxLEFT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),
    wxSizer:add(TopRowSizer, WaterButton, [{flag, ?wxLEFT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),
    wxSizer:addStretchSpacer(TopRowSizer),
    wxSizer:add(TopRowSizer, StartWaveButton, [{flag, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),

    PanelSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(PanelSizer, TopRowSizer, [{flag, ?wxEXPAND bor ?wxTOP bor ?wxBOTTOM}, {border, 10}]),
    wxSizer:add(PanelSizer, CanvasPanel, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxBOTTOM}, {border, 10}]),
    wxPanel:setSizer(BackgroundPanel, PanelSizer),
    wxFrame:setSizerAndFit(Frame, PanelSizer),

    wxFrame:connect(Frame, close_window),
    wxPanel:connect(CanvasPanel, paint, [{callback, fun(Ev, _) -> on_paint(Ev, ServerPid) end}]),
    wxPanel:connect(CanvasPanel, left_down),
    wxButton:connect(StartWaveButton, command_button_clicked),
    wxStaticBitmap:connect(GroundButton, left_down, [{userData, ground_monkey}]),
    wxStaticBitmap:connect(FireButton, left_down, [{userData, fire_monkey}]),
    wxStaticBitmap:connect(AirButton, left_down, [{userData, air_monkey}]),
    wxStaticBitmap:connect(WaterButton, left_down, [{userData, water_monkey}]),

    wxFrame:show(Frame),
    gui_loop(ServerPid, Frame, CanvasPanel).

gui_loop(ServerPid, Frame, Canvas) ->
    receive
        refresh ->
            wxWindow:refresh(Canvas),
            gui_loop(ServerPid, Frame, Canvas);
        #'wx'{event=#'wxMouse'{type=left_down, x=X, y=Y}} ->
            gen_server:cast(ServerPid, {place_monkey, {X, Y}}),
            gui_loop(ServerPid, Frame, Canvas);
        #'wx'{event=#'wxCommand'{type=command_button_clicked}} ->
            gen_server:cast(ServerPid, start_wave),
            gui_loop(ServerPid, Frame, Canvas);
        #'wx'{userData = Brush} ->
            gen_server:cast(ServerPid, {set_brush, Brush}),
            gui_loop(ServerPid, Frame, Canvas);
        #'wx'{obj=Frame, event=#'wxClose'{}} ->
            wxFrame:destroy(Frame);
        _Other ->
            gui_loop(ServerPid, Frame, Canvas)
    end.

on_paint(#wx{obj = Canvas}, ServerPid) ->
    State = gen_server:call(ServerPid, get_state),
    #state{bitmaps = Bitmaps, monkeys = Monkeys, bloons = Bloons, path = Path} = State,
    DC = wxBufferedPaintDC:new(Canvas),
    case Bitmaps of
        undefined -> ok;
        _ ->
            wxDC:drawBitmap(DC, maps:get(map, Bitmaps), {0, 0}),
            draw_path(DC, Path),
            draw_monkeys(DC, Monkeys, Bitmaps),
            draw_bloons(DC, Bloons, Bitmaps)
    end,
    wxBufferedPaintDC:destroy(DC).

draw_path(DC, Path) ->
    wxDC:setPen(DC, wxPen:new({100, 100, 100, 128}, [{width, 5}])),
    wxDC:drawLines(DC, Path).

draw_monkeys(DC, Monkeys, Bitmaps) ->
    lists:foreach(
        fun(#monkey{pos = {X, Y}, type = Type}) ->
            Bitmap = maps:get(Type, Bitmaps),
            wxDC:drawBitmap(DC, Bitmap, {X - (?BUTTON_WIDTH div 2), Y - (?BUTTON_HEIGHT div 2)}, [{useMask, true}])
        end,
        Monkeys).

draw_bloons(DC, Bloons, Bitmaps) ->
    lists:foreach(
        fun(#bloon{path = Path, path_index = PathIdx, type = Type}) ->
            if PathIdx =< length(Path) andalso Path /= undefined ->
                   case maps:find(Type, Bitmaps) of
                       {ok, Bitmap} ->
                           {X, Y} = lists:nth(PathIdx, Path),
                           wxDC:drawBitmap(DC, Bitmap, {X - (?BALLOON_WIDTH div 2), Y - (?BALLOON_HEIGHT div 2)}, [{useMask, true}]);
                       error ->
                           ok
                   end;
               true -> ok
            end
        end,
        Bloons).
