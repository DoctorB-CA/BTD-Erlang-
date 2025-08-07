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
-define(TICK_INTERVAL, 25).
-define(DART_WIDTH, 35).
-define(DART_HEIGHT, 25).

start() ->
    spawn(fun ?MODULE:init/0).

init() ->
    wx:new(),
    wxImage:initStandardHandlers(),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Monkey Game"),
    BackgroundPanel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(BackgroundPanel, {255, 255, 255}),

    DartBitmap = wxBitmap:new(wxImage:scale(wxImage:new("mmmmmmonkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    FireBitmap = wxBitmap:new(wxImage:scale(wxImage:new("fire_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    MapBitmap = wxBitmap:new(wxImage:scale(wxImage:new("map.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT)),
    RedBalloonBitmap = wxBitmap:new(wxImage:scale(wxImage:new("red_balloon.png"), ?BALLOON_WIDTH, ?BALLOON_HEIGHT)),
    DartProjectileBitmap = wxBitmap:new(wxImage:scale(wxImage:new("dart.png"), ?DART_WIDTH, ?DART_HEIGHT)),

    DartButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, DartBitmap),
    FireButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, FireBitmap),
    CanvasPanel = wxPanel:new(BackgroundPanel, [{style, ?wxBORDER_SUNKEN}, {size, {?CANVAS_WIDTH, ?CANVAS_HEIGHT}}]),
    StartWaveButton = wxButton:new(BackgroundPanel, ?wxID_ANY, [{label, "Start Wave"}]),
    ShootButton = wxButton:new(BackgroundPanel, ?wxID_ANY, [{label, "Shoot"}]),

    TopRowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(TopRowSizer, DartButton, [{flag, ?wxLEFT}, {border, 10}]),
    wxSizer:add(TopRowSizer, FireButton, [{flag, ?wxLEFT}, {border, 10}]),
    wxSizer:addStretchSpacer(TopRowSizer),
    wxSizer:add(TopRowSizer, ShootButton, [{flag, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),
    wxSizer:add(TopRowSizer, StartWaveButton, [{flag, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),

    PanelSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(PanelSizer, TopRowSizer, [{flag, ?wxEXPAND bor ?wxTOP bor ?wxBOTTOM}, {border, 10}]),
    wxSizer:add(PanelSizer, CanvasPanel, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxBOTTOM}, {border, 10}]),
    wxPanel:setSizer(BackgroundPanel, PanelSizer),

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, BackgroundPanel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxFrame:setSizer(Frame, MainSizer),
    wxFrame:setSize(Frame, {?WINDOW_WIDTH, ?WINDOW_HEIGHT}),

    wxFrame:connect(Frame, close_window),
    wxStaticBitmap:connect(DartButton, left_down),
    wxStaticBitmap:connect(FireButton, left_down),
    wxPanel:connect(CanvasPanel, left_down),
    wxPanel:connect(CanvasPanel, paint),
    wxButton:connect(StartWaveButton, command_button_clicked),
    wxButton:connect(ShootButton, command_button_clicked),

    wxFrame:show(Frame),
    Path = p:get_path(),
    InitialState = #{
      path => Path,
      dart_bitmap => DartBitmap, fire_bitmap => FireBitmap,
      map_bitmap => MapBitmap, red_balloon_bitmap => RedBalloonBitmap,
      dart_projectile_bitmap => DartProjectileBitmap,
      current_brush => unselected, stamps => [],
      balloons => [],
      projectiles => []
     },
    loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, InitialState).


loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, State) ->
    receive
        #'wx'{obj=Frame, event=#'wxClose'{}} ->
            io:format("Closing window.~n"),
            wxFrame:destroy(Frame),
            exit(normal);

        #'wx'{obj=DartButton, event=#'wxMouse'{type=left_down}} ->
            NewState = State#{current_brush => dart_brush},
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, NewState);

        #'wx'{obj=FireButton, event=#'wxMouse'{type=left_down}} ->
            NewState = State#{current_brush => fire_brush},
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, NewState);

        #'wx'{obj=StartWaveButton, event=#'wxCommand'{type=command_button_clicked}} ->
            io:format("Starting a new balloon at the start of the path.~n"),
            #{balloons := OldBalloons} = State,
            NewBalloon = #{type => red, path_index => 1},
            NewBalloons = [NewBalloon | OldBalloons],
            NewState = State#{balloons => NewBalloons},
            if length(OldBalloons) == 0 -> timer:send_after(?TICK_INTERVAL, self(), {tick}); true -> ok end,
            wxWindow:refresh(CanvasPanel),
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, NewState);

        % Handle clicks on the "Shoot" button
        #'wx'{obj=ShootButton, event=#'wxCommand'{type=command_button_clicked}} ->
            io:format("shot~n"),
            #{projectiles := Projectiles, balloons := Balloons, path := Path, stamps := Stamps} = State,
            % We need at least one monkey and one balloon to shoot
            if (length(Stamps) > 0) and (length(Balloons) > 0) ->
                % For simplicity, we'll make the first monkey shoot at the first balloon
                [{_Type, MonkeyX, MonkeyY} | _] = Stamps,
                [TargetBalloon | _] = Balloons,
                % Calculate the path for the new dart using our game_logic module
                DartPath = game_logic:calculate_dart_path({MonkeyX, MonkeyY}, TargetBalloon, Path),
                % A projectile now has its own path and current position on it
                NewProjectile = #{type => dart, path => DartPath, path_index => 1},
                NewProjectiles = [NewProjectile | Projectiles],
                NewState = State#{projectiles => NewProjectiles},
                wxWindow:refresh(CanvasPanel),
                loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, NewState);
            true ->
                io:format("Need at least one monkey and one balloon to shoot.~n"),
                loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, State)
            end;

        {tick} ->
            #{balloons := Balloons, projectiles := Projectiles, path := Path} = State,
            % Update balloon positions
            UpdatedBalloons = lists:flatmap(
                                fun(B) ->
                                    NewIndex = maps:get(path_index, B) + 1,
                                    if NewIndex < length(Path) -> [B#{path_index => NewIndex}]; true -> [] end
                                end, Balloons),
            % Update projectile positions
            UpdatedProjectiles = lists:flatmap(
                                   fun(P) ->
                                       NewIndex = maps:get(path_index, P) + 1,
                                       if NewIndex =< length(maps:get(path, P)) -> [P#{path_index => NewIndex}]; true -> [] end
                                   end, Projectiles),
            NewState = State#{balloons => UpdatedBalloons, projectiles => UpdatedProjectiles},
            wxWindow:refresh(CanvasPanel),
            if (length(UpdatedBalloons) > 0) or (length(UpdatedProjectiles) > 0) ->
                timer:send_after(?TICK_INTERVAL, self(), {tick});
               true ->
                ok
            end,
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, NewState);

        #'wx'{obj=CanvasPanel, event=#'wxMouse'{type=left_down, x=X, y=Y}} ->
            #{current_brush := BrushType, stamps := Stamps} = State,
            case BrushType of
                unselected ->
                    io:format("Clicked on map at coordinates: {~p, ~p}~n", [X, Y]),
                    loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, State);
                _ ->
                    StampX = X - (?BUTTON_WIDTH div 2), StampY = Y - (?BUTTON_HEIGHT div 2),
                    NewState = State#{stamps => [{BrushType, StampX, StampY} | Stamps], current_brush => unselected},
                    wxWindow:refresh(CanvasPanel),
                    loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, NewState)
            end;

        #'wx'{obj=CanvasPanel, event=#'wxPaint'{}} ->
            #{stamps := Stamps, balloons := Balloons, projectiles := Projectiles,
              dart_bitmap := DartBitmap, fire_bitmap := FireBitmap, map_bitmap := MapBitmap,
              red_balloon_bitmap := RedBalloonBitmap, dart_projectile_bitmap := DartProjectileBitmap,
              path := Path} = State,
            DC = wxBufferedPaintDC:new(CanvasPanel),
            wxDC:drawBitmap(DC, MapBitmap, {0, 0}),
            lists:foreach(fun({BrushType, PX, PY}) ->
                                  Bitmap = case BrushType of dart_brush -> DartBitmap; fire_brush -> FireBitmap end,
                                  wxDC:drawBitmap(DC, Bitmap, {PX, PY}, [{useMask, true}])
                          end, lists:reverse(Stamps)),
            lists:foreach(
              fun(Balloon) ->
                  PathIndex = maps:get(path_index, Balloon),
                  Coords = lists:nth(PathIndex, Path),
                  wxDC:drawBitmap(DC, RedBalloonBitmap, Coords, [{useMask, true}])
              end,
              lists:reverse(Balloons)),
            % Draw all the projectiles along their calculated paths
            lists:foreach(
              fun(Projectile) ->
                  PathIndex = maps:get(path_index, Projectile),
                  ProjectilePath = maps:get(path, Projectile),
                  Coords = lists:nth(PathIndex, ProjectilePath),
                  wxDC:drawBitmap(DC, DartProjectileBitmap, Coords, [{useMask, true}])
              end,
              lists:reverse(Projectiles)),
            wxBufferedPaintDC:destroy(DC),
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, State);

        _Other ->
            loop(Frame, DartButton, FireButton, CanvasPanel, StartWaveButton, ShootButton, State)
    end.
