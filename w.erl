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
% The full map is 4x wider than a single canvas
-define(FULL_MAP_WIDTH, ?CANVAS_WIDTH * 4).
-define(FULL_MAP_HEIGHT, ?CANVAS_HEIGHT).
% NEW: Define the scaling factor for the full map view
-define(FULL_MAP_SCALE, 0.5).

start() ->
    spawn(fun ?MODULE:init/0).

init() ->
    wx:new(),
    wxImage:initStandardHandlers(),

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Monkey Game"),
    BackgroundPanel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(BackgroundPanel, {255, 255, 255}),

    %% --- 1. Load all Bitmaps from the 'images' subfolder ---
    ImagesPath = "/home/csestudent/Desktop/bar/images/",

    GroundBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "ground_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    FireBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "fire_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    AirBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "air_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    WaterBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "water_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    AvatarBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "avatar_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    MapBitmaps = [
        wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "map1.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT)),
        wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "map2.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT)),
        wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "map3.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT)),
        wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "map4.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT))
    ],
    RedBalloonBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "red_balloon.png"), ?BALLOON_WIDTH, ?BALLOON_HEIGHT)),
    GroundProjectileBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "ground_dart.png"), ?DART_WIDTH, ?DART_HEIGHT)),
    FireProjectileBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "fire_dart.png"), ?DART_WIDTH, ?DART_HEIGHT)),
    AirProjectileBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "air_dart.png"), ?DART_WIDTH, ?DART_HEIGHT)),
    WaterProjectileBitmap = wxBitmap:new(wxImage:scale(wxImage:new(ImagesPath ++ "water_dart.png"), ?DART_WIDTH, ?DART_HEIGHT)),

    %% --- 2. Create Widgets ---
    GroundButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, GroundBitmap),
    FireButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, FireBitmap),
    AirButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, AirBitmap),
    WaterButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, WaterBitmap),
    AvatarButton = wxStaticBitmap:new(BackgroundPanel, ?wxID_ANY, AvatarBitmap),
    CanvasPanel = wxPanel:new(BackgroundPanel, [{style, ?wxBORDER_SUNKEN}, {size, {?CANVAS_WIDTH, ?CANVAS_HEIGHT}}]),
    StartWaveButton = wxButton:new(BackgroundPanel, ?wxID_ANY, [{label, "Start Wave"}]),
    ShootButton = wxButton:new(BackgroundPanel, ?wxID_ANY, [{label, "Shoot"}]),
    ChangeMapButton = wxButton:new(BackgroundPanel, ?wxID_ANY, [{label, "Change Map"}]),
    ShowFullMapButton = wxButton:new(BackgroundPanel, ?wxID_ANY, [{label, "Show Full Map"}]),


    %% --- 3. Layout ---
    TopRowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(TopRowSizer, GroundButton, [{flag, ?wxLEFT}, {border, 10}]),
    wxSizer:add(TopRowSizer, FireButton, [{flag, ?wxLEFT}, {border, 10}]),
    wxSizer:add(TopRowSizer, AirButton, [{flag, ?wxLEFT}, {border, 10}]),
    wxSizer:add(TopRowSizer, WaterButton, [{flag, ?wxLEFT}, {border, 10}]),
    wxSizer:add(TopRowSizer, AvatarButton, [{flag, ?wxLEFT}, {border, 10}]),
    wxSizer:addStretchSpacer(TopRowSizer),
    wxSizer:add(TopRowSizer, ShowFullMapButton, [{flag, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),
    wxSizer:add(TopRowSizer, ChangeMapButton, [{flag, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL}, {border, 10}]),
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

    %% --- 4. Event Handling ---
    wxFrame:connect(Frame, close_window),
    wxStaticBitmap:connect(GroundButton, left_down),
    wxStaticBitmap:connect(FireButton, left_down),
    wxStaticBitmap:connect(AirButton, left_down),
    wxStaticBitmap:connect(WaterButton, left_down),
    wxStaticBitmap:connect(AvatarButton, left_down),
    wxPanel:connect(CanvasPanel, left_down),
    wxPanel:connect(CanvasPanel, paint),
    wxButton:connect(StartWaveButton, command_button_clicked),
    wxButton:connect(ShootButton, command_button_clicked),
    wxButton:connect(ChangeMapButton, command_button_clicked),
    wxButton:connect(ShowFullMapButton, command_button_clicked),

    %% --- 5. Start the Loop ---
    wxFrame:show(Frame),
    Path = p:get_path(),
    InitialState = #{
      path => Path,
      ground_bitmap => GroundBitmap, fire_bitmap => FireBitmap,
      air_bitmap => AirBitmap, water_bitmap => WaterBitmap,
      avatar_bitmap => AvatarBitmap,
      map_bitmaps => MapBitmaps,
      current_map_index => 1,
      red_balloon_bitmap => RedBalloonBitmap,
      ground_projectile_bitmap => GroundProjectileBitmap,
      fire_projectile_bitmap => FireProjectileBitmap,
      air_projectile_bitmap => AirProjectileBitmap,
      water_projectile_bitmap => WaterProjectileBitmap,
      current_brush => unselected,
      stamps => #{1 => [], 2 => [], 3 => [], 4 => []},
      balloons => [], projectiles => []
     },
    loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, InitialState).


%% --- Function to show the combined full map ---
show_full_map(ParentFrame, GameState) ->
    % CHANGED: Create the window at 50% of the full map size
    ScaledWidth = round(?FULL_MAP_WIDTH * ?FULL_MAP_SCALE),
    ScaledHeight = round(?FULL_MAP_HEIGHT * ?FULL_MAP_SCALE),
    FullMapFrame = wxFrame:new(ParentFrame, ?wxID_ANY, "Full Map View", [{size, {ScaledWidth, ScaledHeight}}]),
    FullMapPanel = wxPanel:new(FullMapFrame),
    wxPanel:connect(FullMapPanel, paint),
    wxFrame:connect(FullMapFrame, close_window),
    wxFrame:show(FullMapFrame),
    full_map_loop(FullMapFrame, FullMapPanel, GameState).

full_map_loop(FullMapFrame, FullMapPanel, GameState) ->
    receive
        #'wx'{obj=FullMapFrame, event=#'wxClose'{}} ->
            wxFrame:destroy(FullMapFrame);
        #'wx'{obj=FullMapPanel, event=#'wxPaint'{}} ->
            #{stamps := AllStamps, ground_bitmap := GroundBitmap, fire_bitmap := FireBitmap,
              air_bitmap := AirBitmap, water_bitmap := WaterBitmap, avatar_bitmap := AvatarBitmap,
              map_bitmaps := [Map1, Map2, Map3, Map4]} = GameState,
            DC = wxBufferedPaintDC:new(FullMapPanel),
            % NEW: Set the drawing scale to 50%. Everything drawn after this will be smaller.
            wxDC:setUserScale(DC, ?FULL_MAP_SCALE, ?FULL_MAP_SCALE),
            % Draw the four maps side-by-side using their original coordinates
            wxDC:drawBitmap(DC, Map1, {0, 0}),
            wxDC:drawBitmap(DC, Map2, {?CANVAS_WIDTH, 0}),
            wxDC:drawBitmap(DC, Map3, {?CANVAS_WIDTH * 2, 0}),
            wxDC:drawBitmap(DC, Map4, {?CANVAS_WIDTH * 3, 0}),
            % Draw monkeys for each map with the correct offset
            lists:foreach(
              fun({MapNum, OffsetX}) ->
                  StampsForMap = maps:get(MapNum, AllStamps),
                  lists:foreach(
                    fun({BrushType, PX, PY}) ->
                        Bitmap = case BrushType of
                                     ground_brush -> GroundBitmap; fire_brush -> FireBitmap;
                                     air_brush -> AirBitmap; water_brush -> WaterBitmap;
                                     avatar_brush -> AvatarBitmap
                                 end,
                        % We use the original coordinates; the DC handles the scaling
                        wxDC:drawBitmap(DC, Bitmap, {PX + OffsetX, PY}, [{useMask, true}])
                    end,
                    StampsForMap)
              end,
              [{1, 0}, {2, ?CANVAS_WIDTH}, {3, ?CANVAS_WIDTH * 2}, {4, ?CANVAS_WIDTH * 3}]
            ),
            wxBufferedPaintDC:destroy(DC),
            full_map_loop(FullMapFrame, FullMapPanel, GameState);
        _Other ->
            full_map_loop(FullMapFrame, FullMapPanel, GameState)
    end.


loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, State) ->
    receive
        #'wx'{obj=Frame, event=#'wxClose'{}} ->
            io:format("Closing window.~n"),
            wxFrame:destroy(Frame),
            exit(normal);

        #'wx'{obj=GroundButton, event=#'wxMouse'{type=left_down}} ->
            NewState = State#{current_brush => ground_brush},
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState);

        #'wx'{obj=FireButton, event=#'wxMouse'{type=left_down}} ->
            NewState = State#{current_brush => fire_brush},
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState);

        #'wx'{obj=AirButton, event=#'wxMouse'{type=left_down}} ->
            NewState = State#{current_brush => air_brush},
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState);

        #'wx'{obj=WaterButton, event=#'wxMouse'{type=left_down}} ->
            NewState = State#{current_brush => water_brush},
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState);

        #'wx'{obj=AvatarButton, event=#'wxMouse'{type=left_down}} ->
            NewState = State#{current_brush => avatar_brush},
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState);

        #'wx'{obj=StartWaveButton, event=#'wxCommand'{type=command_button_clicked}} ->
            io:format("Starting a new balloon at the start of the path.~n"),
            #{balloons := OldBalloons} = State,
            NewBalloon = #{type => red, path_index => 1},
            NewBalloons = [NewBalloon | OldBalloons],
            NewState = State#{balloons => NewBalloons},
            if length(OldBalloons) == 0 -> timer:send_after(?TICK_INTERVAL, self(), {tick}); true -> ok end,
            wxWindow:refresh(CanvasPanel),
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState);

        #'wx'{obj=ShootButton, event=#'wxCommand'{type=command_button_clicked}} ->
            io:format("All monkeys are shooting...~n"),
            #{projectiles := Projectiles, balloons := Balloons, path := Path,
              stamps := AllStamps, current_map_index := MapIndex} = State,
            CurrentMapStamps = maps:get(MapIndex, AllStamps),
            if (length(CurrentMapStamps) > 0) and (length(Balloons) > 0) ->
                [TargetBalloon | _] = Balloons,
                NewProjectiles = lists:map(
                                   fun({MonkeyType, MonkeyX, MonkeyY}) ->
                                       DartPath = game_logic:calculate_dart_path({MonkeyX, MonkeyY}, TargetBalloon, Path),
                                       DartType = case MonkeyType of
                                                      ground_brush -> ground_dart; fire_brush -> fire_dart;
                                                      air_brush -> air_dart; water_brush -> water_dart;
                                                      avatar_brush ->
                                                          case rand:uniform(4) of
                                                              1 -> ground_dart; 2 -> fire_dart;
                                                              3 -> air_dart; 4 -> water_dart
                                                          end
                                                  end,
                                       #{type => DartType, path => DartPath, path_index => 1}
                                   end,
                                   CurrentMapStamps),
                AllProjectiles = NewProjectiles ++ Projectiles,
                NewState = State#{projectiles => AllProjectiles},
                wxWindow:refresh(CanvasPanel),
                loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState);
            true ->
                io:format("Need at least one monkey and one balloon to shoot.~n"),
                loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, State)
            end;

        #'wx'{obj=ChangeMapButton, event=#'wxCommand'{type=command_button_clicked}} ->
            #{current_map_index := CurrentIndex, map_bitmaps := Maps} = State,
            NewIndex = (CurrentIndex rem length(Maps)) + 1,
            io:format("Changing to map #~p~n", [NewIndex]),
            NewState = State#{current_map_index => NewIndex},
            wxWindow:refresh(CanvasPanel),
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState);

        #'wx'{obj=ShowFullMapButton, event=#'wxCommand'{type=command_button_clicked}} ->
            io:format("Showing full map...~n"),
            show_full_map(Frame, State),
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, State);


        {tick} ->
            #{balloons := Balloons, projectiles := Projectiles, path := Path} = State,
            UpdatedBalloons = lists:flatmap(
                                fun(B) ->
                                    NewIndex = maps:get(path_index, B) + 1,
                                    case NewIndex < length(Path) of
                                        true -> [B#{path_index => NewIndex}];
                                        false -> []
                                    end
                                end, Balloons),
            UpdatedProjectiles = lists:flatmap(
                                   fun(P) ->
                                       NewIndex = maps:get(path_index, P) + 1,
                                       ProjectilePath = maps:get(path, P),
                                       case NewIndex =< length(ProjectilePath) of
                                           true -> [P#{path_index => NewIndex}];
                                           false -> []
                                       end
                                   end, Projectiles),
            NewState = State#{balloons => UpdatedBalloons, projectiles => UpdatedProjectiles},
            wxWindow:refresh(CanvasPanel),
            if (length(UpdatedBalloons) > 0) or (length(UpdatedProjectiles) > 0) ->
                timer:send_after(?TICK_INTERVAL, self(), {tick});
               true ->
                ok
            end,
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState);

        #'wx'{obj=CanvasPanel, event=#'wxMouse'{type=left_down, x=X, y=Y}} ->
            #{current_brush := BrushType, stamps := AllStamps, current_map_index := MapIndex} = State,
            case BrushType of
                unselected ->
                    io:format("Clicked on map at coordinates: {~p, ~p}~n", [X, Y]),
                    loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, State);
                _ ->
                    StampX = X - (?BUTTON_WIDTH div 2), StampY = Y - (?BUTTON_HEIGHT div 2),
                    CurrentMapStamps = maps:get(MapIndex, AllStamps),
                    NewMapStamps = [{BrushType, StampX, StampY} | CurrentMapStamps],
                    UpdatedAllStamps = AllStamps#{MapIndex => NewMapStamps},
                    NewState = State#{stamps => UpdatedAllStamps, current_brush => unselected},
                    wxWindow:refresh(CanvasPanel),
                    loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, NewState)
            end;

        #'wx'{obj=CanvasPanel, event=#'wxPaint'{}} ->
            #{stamps := AllStamps, balloons := Balloons, projectiles := Projectiles,
              ground_bitmap := GroundBitmap, fire_bitmap := FireBitmap, air_bitmap := AirBitmap, water_bitmap := WaterBitmap,
              avatar_bitmap := AvatarBitmap, map_bitmaps := MapBitmaps, current_map_index := MapIndex,
              red_balloon_bitmap := RedBalloonBitmap,
              ground_projectile_bitmap := GroundProjectileBitmap, fire_projectile_bitmap := FireProjectileBitmap,
              air_projectile_bitmap := AirProjectileBitmap, water_projectile_bitmap := WaterProjectileBitmap,
              path := Path} = State,
            DC = wxBufferedPaintDC:new(CanvasPanel),
            CurrentMapBitmap = lists:nth(MapIndex, MapBitmaps),
            wxDC:drawBitmap(DC, CurrentMapBitmap, {0, 0}),
            CurrentMapStamps = maps:get(MapIndex, AllStamps),
            lists:foreach(fun({BrushType, PX, PY}) ->
                                  Bitmap = case BrushType of
                                               ground_brush -> GroundBitmap; fire_brush -> FireBitmap;
                                               air_brush -> AirBitmap; water_brush -> WaterBitmap;
                                               avatar_brush -> AvatarBitmap
                                           end,
                                  wxDC:drawBitmap(DC, Bitmap, {PX, PY}, [{useMask, true}])
                          end, lists:reverse(CurrentMapStamps)),
            lists:foreach(
              fun(Balloon) ->
                  PathIndex = maps:get(path_index, Balloon),
                  Coords = lists:nth(PathIndex, Path),
                  wxDC:drawBitmap(DC, RedBalloonBitmap, Coords, [{useMask, true}])
              end,
              lists:reverse(Balloons)),
            lists:foreach(
              fun(Projectile) ->
                  PathIndex = maps:get(path_index, Projectile),
                  ProjectilePath = maps:get(path, Projectile),
                  Coords = lists:nth(PathIndex, ProjectilePath),
                  DartBitmap = case maps:get(type, Projectile) of
                                   ground_dart -> GroundProjectileBitmap; fire_dart -> FireProjectileBitmap;
                                   air_dart -> AirProjectileBitmap; water_dart -> WaterProjectileBitmap
                               end,
                  wxDC:drawBitmap(DC, DartBitmap, Coords, [{useMask, true}])
              end,
              lists:reverse(Projectiles)),
            wxBufferedPaintDC:destroy(DC),
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, State);

        _Other ->
            loop(Frame, GroundButton, FireButton, AirButton, WaterButton, AvatarButton, CanvasPanel, StartWaveButton, ShootButton, ChangeMapButton, ShowFullMapButton, State)
    end.
