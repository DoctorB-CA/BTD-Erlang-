-module(w).
-export([start/0, init/0]).

-include_lib("wx/include/wx.hrl").

-define(WINDOW_WIDTH, 1000).
-define(WINDOW_HEIGHT, 1000).
-define(BUTTON_WIDTH, 80).
-define(BUTTON_HEIGHT, 80).
-define(CANVAS_WIDTH, 700).
-define(CANVAS_HEIGHT, 600).
-define(BALLOON_WIDTH, 40).
-define(BALLOON_HEIGHT, 40).

start() ->
    spawn(fun ?MODULE:init/0).

init() ->
    wx:new(),
    wxImage:initStandardHandlers(),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Monkey Game"),
    BackgroundPanel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(BackgroundPanel, {255, 255, 255}),

    %% --- 1. Load Bitmaps ---
    DartBitmap = wxBitmap:new(wxImage:scale(wxImage:new("mmmmmmonkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    FireBitmap = wxBitmap:new(wxImage:scale(wxImage:new("fire_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    MapBitmap = wxBitmap:new(wxImage:scale(wxImage:new("map.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT)),
    RedBalloonBitmap = wxBitmap:new(wxImage:scale(wxImage:new("red_balloon.png"), ?BALLOON_WIDTH, ?BALLOON_HEIGHT)),

    %% --- 2. Create Widgets (NO wxTimer) ---
    DartButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, DartBitmap),
    FireButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, FireBitmap),
    CanvasPanel = wxPanel:new(BackgroundPanel, [{style, ?wxBORDER_SUNKEN}, {size, {?CANVAS_WIDTH, ?CANVAS_HEIGHT}}]),
    StartWaveButton = wxButton:new(BackgroundPanel, ?wxID_ANY, [{label, "Start Wave"}]),

    %% --- 3. Layout ---
    TopRowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(TopRowSizer, DartButton, [{flag, ?wxLEFT}, {border, 10}]),
    wxSizer:add(TopRowSizer, FireButton, [{flag, ?wxLEFT}, {border, 10}]),
    wxSizer:addStretchSpacer(TopRowSizer),
    wxSizer:add(TopRowSizer, StartWaveButton, [{flag, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),
    PanelSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(PanelSizer, TopRowSizer, [{flag, ?wxEXPAND bor ?wxTOP bor ?wxBOTTOM}, {border, 10}]),
    wxSizer:add(PanelSizer, CanvasPanel, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxBOTTOM}, {border, 10}]),
    wxPanel:setSizer(BackgroundPanel, PanelSizer),
    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, BackgroundPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxFrame:setSizer(Frame, MainSizer),
    wxFrame:setSize(Frame, {?WINDOW_WIDTH, ?WINDOW_HEIGHT}),

    %% --- 4. Event Handling ---
    wxFrame:connect(Frame, close_window),
    wxStaticBitmap:connect(DartButton, left_down),
    wxStaticBitmap:connect(FireButton, left_down),
    wxPanel:connect(CanvasPanel, left_down),
    wxPanel:connect(CanvasPanel, paint),
    wxButton:connect(StartWaveButton, command_button_clicked),

    %% --- 5. Start the Loop ---
    wxFrame:show(Frame),
    InitialState = #{
      dart_bitmap => DartBitmap,
      fire_bitmap => FireBitmap,
      map_bitmap => MapBitmap,
      red_balloon_bitmap => RedBalloonBitmap,
      ticks => 0,
      current_brush => unselected,
      stamps => [],
      balloons => []
     },
    loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, InitialState).


loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, State) ->
    receive
        #'wx'{obj=Frame, event=#'wxClose'{}} ->
            io:format("Closing window.~n");

        #'wx'{obj=DartButton, event=#'wxMouse'{type=left_down}} ->
            NewState = State#{current_brush => dart_brush},
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, NewState);

        #'wx'{obj=FireButton, event=#'wxMouse'{type=left_down}} ->
            NewState = State#{current_brush => fire_brush},
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, NewState);

        % If "Start Wave" is clicked, add a balloon and START THE ERLANG TIMER
        #'wx'{obj=StartWaveButton, event=#'wxCommand'{type=command_button_clicked}} ->
            io:format("Starting wave... Placing a red balloon.~n"),
            NewBalloons = [{red, 50, 50}],
            NewState = State#{balloons => NewBalloons, ticks => 0},
            % Send the first 'tick' message to ourselves after 1000ms
            timer:send_after(1000, self(), {tick}),
            wxWindow:refresh(CanvasPanel),
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, NewState);

        % NEW: This clause handles the message from the standard Erlang timer
        {tick} ->
            #{balloons := Balloons, ticks := Ticks} = State,
            if
                Ticks < 3 ->
                    io:format("Tick ~p... moving balloon.~n", [Ticks + 1]),
                    [{Color, X, Y}] = Balloons,
                    NewBalloons = [{Color, X + 100, Y}],
                    NewState = State#{balloons => NewBalloons, ticks => Ticks + 1},
                    wxWindow:refresh(CanvasPanel),
                    % Send the NEXT 'tick' message to ourselves
                    timer:send_after(1000, self(), {tick}),
                    loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, NewState);
                true -> % Ticks >= 3, animation is over
                    io:format("Animation finished.~n"),
                    loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, State)
            end;

        #'wx'{obj=CanvasPanel, event=#'wxMouse'{type=left_down, x=X, y=Y}} ->
            #{current_brush := BrushType, stamps := Stamps} = State,
            case BrushType of
                unselected ->
                    io:format("Please select a brush first.~n"),
                    loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, State);
                _ ->
                    StampX = X - (?BUTTON_WIDTH div 2),
                    StampY = Y - (?BUTTON_HEIGHT div 2),
                    NewStamps = [{BrushType, StampX, StampY} | Stamps],
                    NewState = State#{stamps => NewStamps, current_brush => unselected},
                    wxWindow:refresh(CanvasPanel),
                    loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, NewState)
            end;

        #'wx'{obj=CanvasPanel, event=#'wxPaint'{}} ->
            #{stamps := Stamps, balloons := Balloons, dart_bitmap := DartBitmap,
              fire_bitmap := FireBitmap, map_bitmap := MapBitmap,
              red_balloon_bitmap := RedBalloonBitmap} = State,
            DC = wxBufferedPaintDC:new(CanvasPanel),
            wxDC:drawBitmap(DC, MapBitmap, {0, 0}),
            lists:foreach(fun({BrushType, PX, PY}) ->
                                  Bitmap = case BrushType of dart_brush -> DartBitmap; fire_brush -> FireBitmap end,
                                  wxDC:drawBitmap(DC, Bitmap, {PX, PY}, [{useMask, true}])
                          end, lists:reverse(Stamps)),
            lists:foreach(fun({_Color, PX, PY}) ->
                                  wxDC:drawBitmap(DC, RedBalloonBitmap, {PX, PY}, [{useMask, true}])
                          end, lists:reverse(Balloons)),
            wxBufferedPaintDC:destroy(DC),
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, State);

        _Other ->
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, State)
    end,
    wx:destroy().
