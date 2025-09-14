-module(gui).
-behaviour(gen_server).
-include_lib("wx/include/wx.hrl").
-include("dbr.hrl").  % Include database records


-export([start_link/0, add_monkey/4, delete_monkey/1, add_balloon/3, delete_balloon/1, move_balloon/2,
         add_dart/3, delete_dart/1, move_dart/2, change_bananas/1, lose_game/0, clear_board/0,
         refresh/0, update_balloons/1, update_darts/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([load_assets/3]).

-define(WINDOW_WIDTH, 1000).
-define(WINDOW_HEIGHT, 1000).
-define(BUTTON_WIDTH, 80).
-define(BUTTON_HEIGHT, 80).
-define(CANVAS_WIDTH, 200).
-define(CANVAS_HEIGHT, 600).
-define(BALLOON_WIDTH, 40).
-define(BALLOON_HEIGHT, 40).
-define(TICK_INTERVAL, 50).  % Update every 50ms for 20 FPS - better for many balloons
-define(DART_WIDTH, 35).
-define(DART_HEIGHT, 25).
-define(FULL_MAP_WIDTH, ?CANVAS_WIDTH * 4).
-define(FULL_MAP_HEIGHT, ?CANVAS_HEIGHT).
-define(FULL_MAP_SCALE, 0.5).
-record(state, {
    frame, board, map_bitmap, bitmaps, monkeys=#{ }, balloons=#{ }, darts=#{ },
    placing=none,
    banana_text_widget,
    ground_monkey_button_id, water_monkey_button_id, fire_monkey_button_id,
    air_monkey_button_id, avatar_monkey_button_id,
    start_wave_button_id,
    last_wave_click=0  % Timestamp for button debouncing
}).

%% API Functions are unchanged...
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
add_monkey(T,X,Y,I) -> gen_server:cast(gui, {add_monkey,T,X,Y,I}).
delete_monkey(I) -> gen_server:cast(gui, {delete_monkey,I}).
add_balloon(I,T,P) -> gen_server:cast(gui, {add_balloon,I,T,P}).
delete_balloon(I) -> gen_server:cast(gui, {delete_balloon,I}).
move_balloon(I,NP) -> gen_server:cast(gui, {move_balloon,I,NP}).
add_dart(I,T,P) -> gen_server:cast(gui, {add_dart,I,T,P}).
delete_dart(I) -> gen_server:cast(gui, {delete_dart,I}).
move_dart(I,P) -> gen_server:cast(gui, {move_dart,I,P}).
refresh() -> gen_server:cast(gui, refresh).
change_bananas(Amount) -> gen_server:cast(gui, {change_bananas, Amount}).
lose_game() -> gen_server:cast(gui, lose_game).
clear_board() -> gen_server:cast(gui, clear_board).
update_balloons(BalloonMap) -> gen_server:cast(gui, {update_balloons, BalloonMap}).
update_darts(DartMap) -> gen_server:cast(gui, {update_darts, DartMap}).

init([]) ->
    wx:new(),
    ImagePath = "/home/csestudent/Desktop/bar/Proj/images",

    %% Load Assets
    {GroundMonkeyBitmap, GroundMonkeyButtonBitmap} = load_assets(ImagePath, ground_monkey, 90),
    {WaterMonkeyBitmap, WaterMonkeyButtonBitmap} = load_assets(ImagePath, water_monkey, 90),
    {FireMonkeyBitmap, FireMonkeyButtonBitmap} = load_assets(ImagePath, fire_monkey, 90),
    {AirMonkeyBitmap, AirMonkeyButtonBitmap} = load_assets(ImagePath, air_monkey, 90),
    {AvatarMonkeyBitmap, AvatarMonkeyButtonBitmap} = load_assets(ImagePath, avatar_monkey, 120),
    {RedBalloonBitmap, _} = load_assets(ImagePath, red_balloon, 80),
    {GroundDartBitmap, _} = load_assets(ImagePath, ground_dart, 20),
    {WaterDartBitmap, _} = load_assets(ImagePath, water_dart, 20),
    {FireDartBitmap, _} = load_assets(ImagePath, fire_dart, 20),
    {AirDartBitmap, _} = load_assets(ImagePath, air_dart, 20),
    %% ✅ CHANGE: Scale banana icon to 80x80
    BananaImg = wxImage:new(ImagePath ++ "/banana.png"),
    BananaIcon = wxBitmap:new(wxImage:scale(BananaImg, 80, 80)),
    wxImage:destroy(BananaImg),
    %% ✅ CHANGE: Scale map to 800x800
    OriginalMapImg = wxImage:new(ImagePath ++ "/map1.png"),
    ScaledMapImg = wxImage:scale(OriginalMapImg, 800, 800),
    MapBitmap = wxBitmap:new(ScaledMapImg),
    wxImage:destroy(OriginalMapImg), wxImage:destroy(ScaledMapImg),

    Bitmaps = #{ground_monkey=>GroundMonkeyBitmap, water_monkey=>WaterMonkeyBitmap, fire_monkey=>FireMonkeyBitmap, air_monkey=>AirMonkeyBitmap, avatar_monkey=>AvatarMonkeyBitmap, red => RedBalloonBitmap, ground_dart=>GroundDartBitmap, water_dart=>WaterDartBitmap, fire_dart=>FireDartBitmap, air_dart=>AirDartBitmap},
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Game Map"),
    MainPanel = wxPanel:new(Frame),
    TopRowPanel = wxPanel:new(MainPanel),
    Board = wxPanel:new(MainPanel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]),
    %% ✅ CHANGE: Set game board size to 800x800
    wxWindow:setClientSize(Board, {800, 800}),

    %% Create Widgets
    GroundMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, GroundMonkeyButtonBitmap), WaterMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, WaterMonkeyButtonBitmap),
    FireMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, FireMonkeyButtonBitmap), AirMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, AirMonkeyButtonBitmap),
    AvatarMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, AvatarMonkeyButtonBitmap), StartWaveButton = wxButton:new(TopRowPanel, ?wxID_ANY, [{label, "Start Wave"}]),
    BananaIconWidget = wxStaticBitmap:new(TopRowPanel, ?wxID_ANY, BananaIcon), BananaTextWidget = wxStaticText:new(TopRowPanel, ?wxID_ANY, ": ---", [{style, ?wxALIGN_CENTER_VERTICAL}]),
    GroundMonkeyButtonId = wxWindow:getId(GroundMonkeyButton), WaterMonkeyButtonId = wxWindow:getId(WaterMonkeyButton), FireMonkeyButtonId = wxWindow:getId(FireMonkeyButton),
    AirMonkeyButtonId = wxWindow:getId(AirMonkeyButton), AvatarMonkeyButtonId = wxWindow:getId(AvatarMonkeyButton), StartWaveButtonId = wxWindow:getId(StartWaveButton),

    %% Setup Layout
    TopRowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(TopRowSizer, GroundMonkeyButton, [{flag, ?wxALL},{border,5}]), wxSizer:add(TopRowSizer, WaterMonkeyButton, [{flag, ?wxALL},{border,5}]),
    wxSizer:add(TopRowSizer, FireMonkeyButton, [{flag, ?wxALL},{border,5}]), wxSizer:add(TopRowSizer, AirMonkeyButton, [{flag, ?wxALL},{border,5}]),
    wxSizer:add(TopRowSizer, AvatarMonkeyButton, [{flag, ?wxALL},{border,5}]),
    wxSizer:addStretchSpacer(TopRowSizer),
    wxSizer:add(TopRowSizer, BananaIconWidget, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]), wxSizer:add(TopRowSizer, BananaTextWidget, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
    wxSizer:add(TopRowSizer, StartWaveButton, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
    wxWindow:setSizer(TopRowPanel, TopRowSizer),
    PanelSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(PanelSizer, TopRowPanel, [{flag, ?wxEXPAND bor ?wxALL}]),
    wxSizer:add(PanelSizer, Board, [{proportion,1},{flag,?wxEXPAND bor ?wxALL}]),
    wxWindow:setSizer(MainPanel, PanelSizer),

    %% ✅ CHANGE: Set a fixed size for the window instead of fitting it.
    %% This guarantees the window opens at the correct size.
    wxFrame:setClientSize(Frame, {800, 900}),

    %% Connect Events
    wxPanel:connect(Board, paint, []), wxPanel:connect(Board, left_down, []), wxFrame:connect(Frame, close_window),
    wxWindow:connect(GroundMonkeyButton, command_button_clicked), wxWindow:connect(WaterMonkeyButton, command_button_clicked),
    wxWindow:connect(FireMonkeyButton, command_button_clicked), wxWindow:connect(AirMonkeyButton, command_button_clicked),
    wxWindow:connect(AvatarMonkeyButton, command_button_clicked), wxWindow:connect(StartWaveButton, command_button_clicked),
    wxFrame:show(Frame),
    
    % Start a timer for GUI refresh at 30 FPS to prevent white flashes
    timer:send_interval(33, self(), gui_refresh),
    
    {ok, #state{frame=Frame, board=Board, map_bitmap=MapBitmap, bitmaps=Bitmaps, banana_text_widget = BananaTextWidget, ground_monkey_button_id=GroundMonkeyButtonId, water_monkey_button_id=WaterMonkeyButtonId, fire_monkey_button_id=FireMonkeyButtonId, air_monkey_button_id=AirMonkeyButtonId, avatar_monkey_button_id=AvatarMonkeyButtonId, start_wave_button_id = StartWaveButtonId}}.



%handels:
%% The rest of the file is unchanged.
load_assets(ImagePath, TypeName, Size) ->
    FileName = atom_to_list(TypeName) ++ ".png",
    OriginalImg = wxImage:new(ImagePath ++ "/" ++ FileName),
    ButtonBitmap = wxBitmap:new(wxImage:scale(OriginalImg, 80, 80)),
    GameBoardBitmap = wxBitmap:new(wxImage:scale(OriginalImg, Size, Size)),
    wxImage:destroy(OriginalImg), {GameBoardBitmap, ButtonBitmap}.


handle_cast({add_monkey,T,X,Y,I},S)->
    NMaps=maps:put(I,{X,Y,T},S#state.monkeys),
    wxWindow:refresh(S#state.board),
    {noreply,S#state{monkeys=NMaps}};

handle_cast({delete_monkey,I},S)->
    NMaps=maps:remove(I,S#state.monkeys),
    {noreply,S#state{monkeys=NMaps}};

handle_cast({add_balloon,I,T,P},S)->
    % Keep this for compatibility but balloons will be updated from DB
    NMaps=maps:put(I,{T,P},S#state.balloons),
    {noreply,S#state{balloons=NMaps}};

handle_cast({delete_balloon,I},S)->
    % Keep this for compatibility but balloons will be updated from DB
    NMaps=maps:remove(I,S#state.balloons),
    {noreply,S#state{balloons=NMaps}};

handle_cast({move_balloon,I,NP},S=#state{balloons=B})->
    % Keep this for compatibility but balloons will be updated from DB
    case maps:is_key(I,B) of 
        true->
            {T,_}=maps:get(I,B),
            NB=maps:put(I,{T,NP},B),
            {noreply,S#state{balloons=NB}};
         false->
            {noreply,S} 
        end;

handle_cast({add_dart,I,T,P},S)->
    NMaps=maps:put(I,{T,P},S#state.darts),
    {noreply,S#state{darts=NMaps}};

handle_cast({delete_dart,I},S)->
    NMaps=maps:remove(I,S#state.darts),
    {noreply,S#state{darts=NMaps}};

handle_cast({move_dart,I,NP},S=#state{darts=D})->
    case maps:is_key(I,D) of 
        true->
            {T,_}=maps:get(I,D),
            ND=maps:put(I,{T,NP},D),
            {noreply,S#state{darts=ND}};
        
        false->{noreply,S} 
    end;

handle_cast(refresh,S)->
    wxWindow:refresh(S#state.board),
    {noreply,S};

handle_cast({change_bananas,A},S=#state{banana_text_widget=W})->
    wxStaticText:setLabel(W,": "++integer_to_list(A)),
    {noreply,S};

handle_cast(lose_game,S=#state{frame=F})->
    Dialog=wxMessageDialog:new(F,"You lose! Play again?",[{caption,"Game Over"},{style,?wxYES_NO}]),
    Result=wxMessageDialog:showModal(Dialog),
    wxMessageDialog:destroy(Dialog),
    if Result==?wxID_YES->main_server:restart_game();
    Result==?wxID_NO-> 
        wxFrame:close(F)
    end,
    {noreply,S};

handle_cast(clear_board,S)->
    {noreply,S#state{monkeys=#{},balloons=#{},darts=#{}}};

handle_cast({update_balloons, BalloonMap}, S) ->
    % SIMPLE APPROACH: Just update balloons without forcing refresh
    % The paint event will handle the redraw naturally
    {noreply, S#state{balloons=BalloonMap}};  % Store new balloon positions

handle_cast({update_darts, DartMap}, S) ->
    % SIMPLE APPROACH: Just update darts without forcing refresh
    {noreply, S#state{darts=DartMap}};  % Store new dart positions

handle_cast(_,S)->{noreply,S}.


handle_info(gui_refresh, S) ->
    wxWindow:refresh(S#state.board),
    {noreply, S};

handle_info(#wx{event=#wxPaint{}},S)->
    DC=wxBufferedPaintDC:new(S#state.board),  
    
    % SIMPLE: Draw background once
    wxDC:drawBitmap(DC,S#state.map_bitmap,{0,0},[]),
    
    % SIMPLE: Pre-calculate bitmap sizes (avoid repeated wxBitmap calls)
    BitmapCache = maps:map(fun(_, BMP) ->
        {BMP, wxBitmap:getWidth(BMP), wxBitmap:getHeight(BMP)}
    end, S#state.bitmaps),
    
    % SIMPLE: Draw all monkeys
    lists:foreach(fun({_Id, {X,Y,T}}) ->
        {BMP, W, H} = maps:get(T, BitmapCache),
        wxDC:drawBitmap(DC,BMP,{X-round(W/2),Y-round(H/2)},[{useMask,true}])
    end, maps:to_list(S#state.monkeys)),

    % SIMPLE: Draw all balloons (round coordinates to integers)
    lists:foreach(fun({_Id, {T,{X,Y}}}) ->
        {BMP, W, H} = maps:get(T, BitmapCache),
        IntX = round(X), IntY = round(Y),
        wxDC:drawBitmap(DC,BMP,{IntX-round(W/2),IntY-round(H/2)},[{useMask,true}])
    end, maps:to_list(S#state.balloons)),

    % SIMPLE: Draw all darts (round coordinates to integers)
    lists:foreach(fun({_Id, {T,{X,Y}}}) ->
        {BMP, W, H} = maps:get(T, BitmapCache),
        IntX = round(X), IntY = round(Y),
        wxDC:drawBitmap(DC,BMP,{IntX-round(W/2),IntY-round(H/2)},[{useMask,true}])
    end, maps:to_list(S#state.darts)),

    wxBufferedPaintDC:destroy(DC),
    {noreply,S};

handle_info(#wx{event=#wxMouse{type=left_down,x=_X,y=_Y}},S=#state{placing=none})->
    io:format("GUI: Please select a monkey type first.~n"),
    {noreply,S};

handle_info(#wx{event=#wxMouse{type=left_down,x=X,y=Y}},S=#state{placing=MT})->
    gen_server:cast(main_server,{place_item,{MT,X,Y}}),
    {noreply,S#state{placing=none}};

handle_info(#wx{event=#wxClose{}},_S)->
    {stop,normal,_S};

handle_info(#wx{id=Id,event=#wxCommand{}},State)->
    case Id of 
        S when S==State#state.ground_monkey_button_id->
            io:format("GUI: Selected ground_monkey.~n"),
            {noreply,State#state{placing=ground_monkey}};
        
        S when S==State#state.water_monkey_button_id->
            io:format("GUI: Selected water_monkey.~n"),
            {noreply,State#state{placing=water_monkey}};
        
        S when S==State#state.fire_monkey_button_id->
            io:format("GUI: Selected fire_monkey.~n"),
            {noreply,State#state{placing=fire_monkey}};
        
        S when S==State#state.air_monkey_button_id->
            io:format("GUI: Selected air_monkey.~n"),
            {noreply,State#state{placing=air_monkey}};
        
        S when S==State#state.avatar_monkey_button_id->
            io:format("GUI: Selected avatar_monkey.~n"),
            {noreply,State#state{placing=avatar_monkey}};
        
        S when S==State#state.start_wave_button_id->
            Now = erlang:system_time(millisecond),
            LastClick = State#state.last_wave_click,
            if
                Now - LastClick > 1000 ->  % Prevent clicks within 1 second
                    io:format("GUI: Start Wave button clicked!~n"),
                    main_server:generate_level1(),
                    {noreply, State#state{last_wave_click=Now}};
                true ->
                    io:format("GUI: Start Wave button clicked too soon, ignoring~n"),
                    {noreply, State}
            end;

        _->{noreply,State}
    end;

handle_info(_, S) ->
     {noreply, S}.

terminate(_,S)->
    maps:foreach(fun(_,B)->wxBitmap:destroy(B)end,S#state.bitmaps),
    wx:destroy(),
    ok.
handle_call(_,_,S)->
    {reply,ok,S}.

code_change(_,S,_) -> 
    {ok,S}.
