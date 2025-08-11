%%% w.erl — passive renderer talking to main_server

-module(w).
-export([start/0, init/0]).
-include_lib("wx/include/wx.hrl").

-define(WINDOW_WIDTH, 1000).
-define(WINDOW_HEIGHT, 1000).
-define(BUTTON_WIDTH, 80).
-define(BUTTON_HEIGHT, 80).
-define(CANVAS_WIDTH, 700).
-define(CANVAS_HEIGHT, 600).

-define(BALLOON_W, 40).
-define(BALLOON_H, 40).
-define(DART_W, 35).
-define(DART_H, 25).

-define(WORLD_TOTAL_W, 200).
-define(WORLD_MAPS, 4).
-define(WORLD_REGION_W, ?WORLD_TOTAL_W div ?WORLD_MAPS).

start() -> spawn(fun ?MODULE:init/0).

get_main_node() ->
    case os:getenv("BTD_MAIN_NODE") of false -> node(); Str -> list_to_atom(Str) end.

to_world(MapIdx, {Xpx, Ypx}) ->
    WXlocal = round((Xpx / ?CANVAS_WIDTH) * ?WORLD_REGION_W),
    WX = WXlocal + (MapIdx - 1) * ?WORLD_REGION_W,
    WY = round((Ypx / ?CANVAS_HEIGHT) * ?WORLD_TOTAL_W),
    {WX, WY}.

from_world({WX, WY}) ->
    MapIdx = (WX div ?WORLD_REGION_W) + 1,
    WXlocal = WX rem ?WORLD_REGION_W,
    Xpx = round((WXlocal / ?WORLD_REGION_W) * ?CANVAS_WIDTH),
    Ypx = round((WY / ?WORLD_TOTAL_W) * ?CANVAS_HEIGHT),
    {MapIdx, {Xpx, Ypx}}.

get_full_path() ->
    try p:get_path() of L when is_list(L) -> L
    catch _:_ -> [ {X, 300} || X <- lists:seq(10, ?CANVAS_WIDTH * ?WORLD_MAPS - 10, 10) ] end.

to_world_full_list(PixelsList) ->
    lists:map(
      fun({Xpx, Ypx}) ->
          WX = round((Xpx / (?CANVAS_WIDTH * ?WORLD_MAPS)) * ?WORLD_TOTAL_W),
          WY = round((Ypx / ?CANVAS_HEIGHT) * ?WORLD_TOTAL_W),
          {WX, WY}
      end, PixelsList).

init() ->
    wx:new(), wxImage:initStandardHandlers(),

    Images = "/home/csestudent/Desktop/bar/images/",
    GndBmp = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "ground_monkey.png"), ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    FireBmp = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "fire_monkey.png"),   ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    WatBmp = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "water_mnkey.png"),   ?BUTTON_WIDTH, ?BUTTON_HEIGHT)), %% (filename you gave)
    AirBmp = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "air_monkey.png"),     ?BUTTON_WIDTH, ?BUTTON_HEIGHT)),
    AvaBmp = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "avater_monkey.png"),  ?BUTTON_WIDTH, ?BUTTON_HEIGHT)), %% (filename you gave)

    Maps = [
      wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "map1.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT)),
      wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "map2.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT)),
      wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "map3.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT)),
      wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "map4.png"), ?CANVAS_WIDTH, ?CANVAS_HEIGHT))
    ],

    RedB   = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "red_balloon.png"),   ?BALLOON_W, ?BALLOON_H)),
    BlueB  = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "blue_balloon.png"),  ?BALLOON_W, ?BALLOON_H)),
    GreenB = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "green_balloon.png"), ?BALLOON_W, ?BALLOON_H)),
    BlackB = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "black_balloon.png"), ?BALLOON_W, ?BALLOON_H)),

    GDart  = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "ground_dart.png"), ?DART_W, ?DART_H)),
    FDart  = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "fire_dart.png"),   ?DART_W, ?DART_H)),
    WDart  = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "water_dart.png"),  ?DART_W, ?DART_H)),
    ADart  = wxBitmap:new(wxImage:scale(wxImage:new(Images ++ "air_dart.png"),    ?DART_W, ?DART_H)),

    State0 = #{
      map_bitmaps => Maps,
      ground_bmp => GndBmp, fire_bmp => FireBmp, water_bmp => WatBmp, air_bmp => AirBmp, avatar_bmp => AvaBmp,
      red_bmp => RedB, blue_bmp => BlueB, green_bmp => GreenB, black_bmp => BlackB,
      gdart => GDart, fdart => FDart, wdart => WDart, adart => ADart,
      current_map => 1,
      current_brush => unselected,
      snapshot => #{monkeys => [], bloons => [], darts => []},
      full_path_pixels => get_full_path()
    },

    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "BTD (GUI renderer)"),
    Panel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(Panel, {255,255,255}),

    GndB = wxStaticBitmap:new(Panel, ?wxID_ANY, GndBmp),
    FireB = wxStaticBitmap:new(Panel, ?wxID_ANY, FireBmp),
    WatB = wxStaticBitmap:new(Panel, ?wxID_ANY, WatBmp),
    AirB = wxStaticBitmap:new(Panel, ?wxID_ANY, AirBmp),
    AvaB = wxStaticBitmap:new(Panel, ?wxID_ANY, AvaBmp),

    Canvas = wxPanel:new(Panel, [{style, ?wxBORDER_SUNKEN}, {size, {?CANVAS_WIDTH, ?CANVAS_HEIGHT}}]),
    StartWaveBtn = wxButton:new(Panel, ?wxID_ANY, [{label, "Start Wave"}]),
    ChangeMapBtn = wxButton:new(Panel, ?wxID_ANY, [{label, "Change Map"}]),

    Top = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(Top, GndB, [{flag, ?wxLEFT}, {border, 8}]),
    wxSizer:add(Top, FireB, [{flag, ?wxLEFT}, {border, 8}]),
    wxSizer:add(Top, WatB, [{flag, ?wxLEFT}, {border, 8}]),
    wxSizer:add(Top, AirB, [{flag, ?wxLEFT}, {border, 8}]),
    wxSizer:add(Top, AvaB, [{flag, ?wxLEFT}, {border, 8}]),
    wxSizer:addStretchSpacer(Top),
    wxSizer:add(Top, ChangeMapBtn, [{flag, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL}, {border, 8}]),
    wxSizer:add(Top, StartWaveBtn, [{flag, ?wxRIGHT bor ?wxALIGN_CENTER_VERTICAL}, {border, 8}]),

    Root = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Root, Top, [{flag, ?wxEXPAND bor ?wxALL}, {border, 8}]),
    wxSizer:add(Root, Canvas, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxALL}, {border, 8}]),
    wxPanel:setSizer(Panel, Root),
    wxFrame:setSizer(Frame, Root),
    wxFrame:setSize(Frame, {?WINDOW_WIDTH, ?WINDOW_HEIGHT}),

    wxFrame:connect(Frame, close_window),
    wxStaticBitmap:connect(GndB, left_down),
    wxStaticBitmap:connect(FireB, left_down),
    wxStaticBitmap:connect(WatB, left_down),
    wxStaticBitmap:connect(AirB, left_down),
    wxStaticBitmap:connect(AvaB, left_down),

    wxPanel:connect(Canvas, left_down),
    wxPanel:connect(Canvas, paint),
    wxButton:connect(StartWaveBtn, command_button_clicked),
    wxButton:connect(ChangeMapBtn, command_button_clicked),

    wxFrame:show(Frame),

    gen_server:cast({main_server, get_main_node()}, {register_gui, self()}),

    loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State0).

loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State) ->
    receive
        {render, Snapshot} ->
            NewState = State#{snapshot => Snapshot},
            wxWindow:refresh(Canvas),
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, NewState);

        #'wx'{obj=Frame, event=#'wxClose'{}} ->
            wxFrame:destroy(Frame), exit(normal);

        %% brushes
        #'wx'{obj=GndB, event=#'wxMouse'{type=left_down}} ->
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State#{current_brush => ground_monkey});
        #'wx'{obj=FireB, event=#'wxMouse'{type=left_down}} ->
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State#{current_brush => fire_monkey});
        #'wx'{obj=WatB, event=#'wxMouse'{type=left_down}} ->
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State#{current_brush => water_monkey});
        #'wx'{obj=AirB, event=#'wxMouse'{type=left_down}} ->
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State#{current_brush => air_monkey});
        #'wx'{obj=AvaB, event=#'wxMouse'{type=left_down}} ->
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State#{current_brush => avatar_monkey});

        %% place monkey
        #'wx'{obj=Canvas, event=#'wxMouse'{type=left_down, x=X, y=Y}} ->
            #{current_brush := Brush, current_map := MapIdx} = State,
            case Brush of
                unselected ->
                    loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State);
                _ ->
                    WorldPos = to_world(MapIdx, {X, Y}),
                    gen_server:cast({main_server, get_main_node()}, {add_monkey, WorldPos, 60, Brush}),
                    loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State#{current_brush => unselected})
            end;

        %% buttons
        #'wx'{obj=StartWaveBtn, event=#'wxCommand'{type=command_button_clicked}} ->
            FullPixels = maps:get(full_path_pixels, State),
            WorldPath  = to_world_full_list(FullPixels),
            gen_server:cast({main_server, get_main_node()}, {start_wave, level_1, WorldPath}),
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State);

        #'wx'{obj=ChangeMapBtn, event=#'wxCommand'{type=command_button_clicked}} ->
            Cur = maps:get(current_map, State),
            NewState = State#{current_map => (Cur rem 4) + 1},
            wxWindow:refresh(Canvas),
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, NewState);

        %% paint
        #'wx'{obj=Canvas, event=#'wxPaint'{}} ->
            draw(Canvas, State),
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State);

        _Other ->
            loop(Frame, Canvas, GndB, FireB, WatB, AirB, AvaB, StartWaveBtn, ChangeMapBtn, State)
    end.

draw(Canvas, State) ->
    DC = wxBufferedPaintDC:new(Canvas),
    [Map1, Map2, Map3, Map4] = maps:get(map_bitmaps, State),
    Cur = maps:get(current_map, State),
    CurrentMap = case Cur of 1->Map1; 2->Map2; 3->Map3; 4->Map4 end,
    wxDC:drawBitmap(DC, CurrentMap, {0,0}),
    Snap = maps:get(snapshot, State),

    %% Monkeys
    lists:foreach(fun(M) ->
        {MapIdx, {Xpx, Ypx}} = from_world(maps:get(pos, M)),
        case MapIdx =:= Cur of
            true ->
                Bmp = case maps:get(type, M) of
                        ground_monkey -> maps:get(ground_bmp, State);
                        fire_monkey   -> maps:get(fire_bmp, State);
                        water_monkey  -> maps:get(water_bmp, State);
                        air_monkey    -> maps:get(air_bmp, State);
                        avatar_monkey -> maps:get(avatar_bmp, State);
                        _             -> maps:get(ground_bmp, State)
                      end,
                wxDC:drawBitmap(DC, Bmp, {Xpx - (?BUTTON_WIDTH div 2), Ypx - (?BUTTON_HEIGHT div 2)}, [{useMask,true}]);
            false -> ok
        end
    end, maps:get(monkeys, Snap, [])),

    %% Bloons
    lists:foreach(fun(B) ->
        {MapIdx, {Xpx, Ypx}} = from_world(maps:get(pos, B)),
        case MapIdx =:= Cur of
            true ->
                BBmp = case maps:get(type, B) of
                        blue  -> maps:get(blue_bmp, State);
                        green -> maps:get(green_bmp, State);
                        black -> maps:get(black_bmp, State);
                        _     -> maps:get(red_bmp, State)
                       end,
                wxDC:drawBitmap(DC, BBmp, {Xpx, Ypx}, [{useMask,true}]);
            false -> ok
        end
    end, maps:get(bloons, Snap, [])),

    %% Darts
    lists:foreach(fun(D) ->
        {MapIdx, {Xpx, Ypx}} = from_world(maps:get(pos, D)),
        case MapIdx =:= Cur of
            true ->
                DBmp = case maps:get(type, D) of
                         ground -> maps:get(gdart, State);
                         fire   -> maps:get(fdart, State);
                         water  -> maps:get(wdart, State);
                         air    -> maps:get(adart, State);
                         _      -> maps:get(gdart, State)
                       end,
                wxDC:drawBitmap(DC, DBmp, {Xpx, Ypx}, [{useMask,true}]);
            false -> ok
        end
    end, maps:get(darts, Snap, [])),

    wxBufferedPaintDC:destroy(DC).
