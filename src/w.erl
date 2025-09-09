-module(w).
-export([start/0]).

-include_lib("wx/include/wx.hrl").
-include("dbr.hrl").

-compile({nowarn_unused_function, [init/0, game_loop/3, draw_path/2, draw_monkeys/3, draw_bloons/3]}).

-define(WINDOW_WIDTH, 850).
-define(WINDOW_HEIGHT, 750).
-define(BUTTON_WIDTH, 80).
-define(BUTTON_HEIGHT, 80).
-define(CANVAS_WIDTH, 800).
-define(CANVAS_HEIGHT, 600).
-define(BALLOON_WIDTH, 40).
-define(BALLOON_HEIGHT, 40).
-define(TICK_INTERVAL, 100). % Refresh rate in milliseconds

start() ->
    spawn(?MODULE, init, []).

init() ->
    wx:new(),
    wxImage:initStandardHandlers(),

    %% --- 1. Load all Bitmaps once ---
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

    %% --- 2. Create Initial State ---
    InitialState = #{
        bitmaps => #{
            ground_monkey => GroundBitmap, fire_monkey => FireBitmap, air_monkey => AirBitmap, water_monkey => WaterBitmap,
            map => MapBitmap,
            red_bloon => RedBalloonBitmap, blue_bloon => BlueBalloonBitmap, green_bloon => GreenBalloonBitmap, black_bloon => BlackBalloonBitmap
        },
        current_brush => unselected,
        monkeys => [],
        bloons => [],
        path => [{10, 100},{20, 100},{30, 100},{40, 100},{50, 100},{60, 100}, {70, 110}, {80, 100}, {120, 90}, {180, 100}]
    },

    %% --- 3. Build the Main Game Window ---
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "BTD-Erlang"),
    BackgroundPanel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(BackgroundPanel, {220, 220, 220}),

    % GUI Components
    GroundButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, GroundBitmap),
    FireButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, FireBitmap),
    AirButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, AirBitmap),
    WaterButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, WaterBitmap),
    CanvasPanel = wxPanel:new(BackgroundPanel, [{style, ?wxBORDER_SUNKEN}, {size, {?CANVAS_WIDTH, ?CANVAS_HEIGHT}}]),
    StartWaveButton = wxButton:new(BackgroundPanel, ?wxID_ANY, [{label, "Start Wave"}]),

    % Sizers for layout
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

    %% --- 4. Connect Events ---
    wxFrame:connect(Frame, close_window),
    wxPanel:connect(CanvasPanel, left_down),
    wxPanel:connect(CanvasPanel, paint),
    wxButton:connect(StartWaveButton, command_button_clicked),
    wxStaticBitmap:connect(GroundButton, left_down, [{userData, ground_monkey}]),
    wxStaticBitmap:connect(FireButton, left_down, [{userData, fire_monkey}]),
    wxStaticBitmap:connect(AirButton, left_down, [{userData, air_monkey}]),
    wxStaticBitmap:connect(WaterButton, left_down, [{userData, water_monkey}]),

    %% --- 5. Show Frame and Start Game Loop ---
    wxFrame:show(Frame),
    timer:send_after(?TICK_INTERVAL, self(), tick),
    game_loop(Frame, CanvasPanel, InitialState).

game_loop(Frame, CanvasPanel, State) ->
    receive
        #'wx'{obj=Frame, event=#'wxClose'{}} ->
            wxFrame:destroy(Frame),
            exit(normal);

        tick ->
            NewState = case main_server:get_game_state() of
                #{monkeys := Monkeys, bloons := Bloons} -> State#{monkeys => Monkeys, bloons => Bloons};
                _ -> State
            end,
            wxWindow:refresh(CanvasPanel),
            timer:send_after(?TICK_INTERVAL, self(), tick),
            game_loop(Frame, CanvasPanel, NewState);

        #'wx'{obj=CanvasPanel, event=#'wxMouse'{type=left_down, x=X, y=Y}} ->
            #{current_brush := BrushType} = State,
            NewState = case BrushType of
                unselected -> State;
                _ ->
                    main_server:add_monkey({X, Y}, 100, BrushType),
                    State#{current_brush => unselected}
            end,
            game_loop(Frame, CanvasPanel, NewState);

        #'wx'{event=#'wxMouse'{type=left_down}, userData = BrushType} ->
            game_loop(Frame, CanvasPanel, State#{current_brush => BrushType});

        #'wx'{event=#'wxCommand'{type=command_button_clicked}} ->
            main_server:add_bloon(maps:get(path, State), red_bloon),
            game_loop(Frame, CanvasPanel, State);

        #'wx'{obj=CanvasPanel, event=#'wxPaint'{}} ->
            #{bitmaps := Bitmaps, monkeys := Monkeys, bloons := Bloons, path := Path} = State,
            DC = wxBufferedPaintDC:new(CanvasPanel),
            wxDC:drawBitmap(DC, maps:get(map, Bitmaps), {0, 0}),
            draw_path(DC, Path),
            draw_monkeys(DC, Monkeys, Bitmaps),
            draw_bloons(DC, Bloons, Bitmaps),
            wxBufferedPaintDC:destroy(DC),
            game_loop(Frame, CanvasPanel, State);

        _Other ->
            game_loop(Frame, CanvasPanel, State)
    end.

%% --- Drawing Helper Functions ---
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
                Bitmap = maps:get(Type, Bitmaps),
                {X, Y} = lists:nth(PathIdx, Path),
                wxDC:drawBitmap(DC, Bitmap, {X - (?BALLOON_WIDTH div 2), Y - (?BALLOON_HEIGHT div 2)}, [{useMask, true}]);
               true -> ok
            end
        end,
        Bloons).
