-module(w).
-behaviour(gen_server).

-export([start/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

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
    frame,
    canvas,
    bitmaps,
    current_brush = unselected,
    monkeys = [],
    bloons = [],
    path
}).

%% ===================================================================
%% Public API
%% ===================================================================
start() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================
init([]) ->
    spawn(fun() -> gui_init() end),
    {ok, #state{}}.

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast({set_brush, Brush}, State) ->
    {noreply, State#state{current_brush = Brush}};
handle_cast({place_monkey, Pos}, State = #state{current_brush = Brush}) ->
    if Brush /= unselected ->
           main_server:add_monkey(Pos, 100, Brush);
       true ->
           ok
    end,
    {noreply, State#state{current_brush = unselected}};
handle_cast(start_wave, State = #state{path = Path}) ->
    main_server:add_bloon(Path, red_bloon),
    {noreply, State}.

handle_info(tick, State = #state{canvas = Canvas}) ->
    NewState = case main_server:get_game_state() of
                   #{monkeys := Monkeys, bloons := Bloons} ->
                       State#state{monkeys = Monkeys, bloons = Bloons};
                   _ ->
                       State
               end,
    wxWindow:refresh(Canvas),
    timer:send_after(?TICK_INTERVAL, self(), tick),
    {noreply, NewState};
handle_info(#'wx'{obj=Frame, event=#'wxClose'{}}, State = #state{frame=Frame}) ->
    wxFrame:destroy(Frame),
    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% ===================================================================
%% GUI Initialization and Drawing (runs in a separate process)
%% ===================================================================
gui_init() ->
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
        red_bloon => RedBalloonBitmap, blue_bloon => BlueBalloonBitmap, green_bloon => GreenBalloonBitmap, black_bloon => BlackBalloonBitmap
    },
    Path = [{10, 100},{20, 100},{30, 100},{40, 100},{50, 100},{60, 100}, {70, 110}, {80, 100}, {120, 90}, {180, 100}],

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

    gen_server:call(?SERVER, {init_gui, Frame, CanvasPanel, Bitmaps, Path}),

    wxFrame:connect(Frame, close_window, [{userData, self()}]),
    wxPanel:connect(CanvasPanel, paint, [{callback, fun on_paint/2}]),
    wxPanel:connect(CanvasPanel, left_down),
    wxButton:connect(StartWaveButton, command_button_clicked),
    wxStaticBitmap:connect(GroundButton, left_down, [{userData, ground_monkey}]),
    wxStaticBitmap:connect(FireButton, left_down, [{userData, fire_monkey}]),
    wxStaticBitmap:connect(AirButton, left_down, [{userData, air_monkey}]),
    wxStaticBitmap:connect(WaterButton, left_down, [{userData, water_monkey}]),

    wxFrame:show(Frame),
    timer:send_after(?TICK_INTERVAL, ?SERVER, tick).

on_paint(#wx{obj = Canvas}, _Ev) ->
    State = gen_server:call(?SERVER, get_state),
    #state{bitmaps = Bitmaps, monkeys = Monkeys, bloons = Bloons, path = Path} = State,
    DC = wxBufferedPaintDC:new(Canvas),
    wxDC:drawBitmap(DC, maps:get(map, Bitmaps), {0, 0}),
    draw_path(DC, Path),
    draw_monkeys(DC, Monkeys, Bitmaps),
    draw_bloons(DC, Bloons, Bitmaps),
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
            if PathIdx =< length(Path) ->
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