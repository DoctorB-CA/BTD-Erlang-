-module(gui).
-behaviour(gen_server).
-include_lib("wx/include/wx.hrl").

-export([start_link/0, add_monkey/4, delete_monkey/1, add_balloon/3, delete_balloon/1, move_balloon/2,
         add_dart/3, delete_dart/1, move_dart/2,
         refresh/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([load_assets/3]).

-record(state, {
    frame, board, map_bitmap, bitmaps, monkeys = #{}, balloons = #{}, darts = #{},
    placing = none,
    ground_monkey_button_id, water_monkey_button_id, fire_monkey_button_id,
    air_monkey_button_id, avatar_monkey_button_id,
    start_wave_button_id
}).

%% API and other functions are unchanged...
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
load_assets(ImagePath, TypeName, Size) -> FileName = atom_to_list(TypeName) ++ ".png", OriginalImg = wxImage:new(ImagePath ++ "/" ++ FileName), ButtonBitmap = wxBitmap:new(wxImage:scale(OriginalImg, 80, 80)), GameBoardBitmap = wxBitmap:new(wxImage:scale(OriginalImg, Size, Size)), wxImage:destroy(OriginalImg), {GameBoardBitmap, ButtonBitmap}.

init([]) ->
    wx:new(),
    ImagePath = "/home/csestudent/Desktop/bar/images",
    {GroundMonkeyBitmap, GroundMonkeyButtonBitmap} = load_assets(ImagePath, ground_monkey, 90), {WaterMonkeyBitmap, WaterMonkeyButtonBitmap} = load_assets(ImagePath, water_monkey, 90),
    {FireMonkeyBitmap, FireMonkeyButtonBitmap} = load_assets(ImagePath, fire_monkey, 90), {AirMonkeyBitmap, AirMonkeyButtonBitmap} = load_assets(ImagePath, air_monkey, 90),
    {AvatarMonkeyBitmap, AvatarMonkeyButtonBitmap} = load_assets(ImagePath, avatar_monkey, 120), {RedBalloonBitmap, _} = load_assets(ImagePath, red_balloon, 80),
    {GroundDartBitmap, _} = load_assets(ImagePath, ground_dart, 20), {WaterDartBitmap, _} = load_assets(ImagePath, water_dart, 20),
    {FireDartBitmap, _} = load_assets(ImagePath, fire_dart, 20), {AirDartBitmap, _} = load_assets(ImagePath, air_dart, 20),
    OriginalMapImg = wxImage:new(ImagePath ++ "/map1.png"), ScaledMapImg = wxImage:scale(OriginalMapImg, 1000, 1000), MapBitmap = wxBitmap:new(ScaledMapImg), wxImage:destroy(OriginalMapImg), wxImage:destroy(ScaledMapImg),
    Bitmaps = #{ground_monkey=>GroundMonkeyBitmap, water_monkey=>WaterMonkeyBitmap, fire_monkey=>FireMonkeyBitmap, air_monkey=>AirMonkeyBitmap, avatar_monkey=>AvatarMonkeyBitmap, red => RedBalloonBitmap, ground_dart=>GroundDartBitmap, water_dart=>WaterDartBitmap, fire_dart=>FireDartBitmap, air_dart=>AirDartBitmap},
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Game Map"), MainPanel = wxPanel:new(Frame), TopRowPanel = wxPanel:new(MainPanel),
    Board = wxPanel:new(MainPanel, [{style, ?wxFULL_REPAINT_ON_RESIZE}]), wxWindow:setClientSize(Board, {1000, 1000}),
    GroundMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, GroundMonkeyButtonBitmap), WaterMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, WaterMonkeyButtonBitmap),
    FireMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, FireMonkeyButtonBitmap), AirMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, AirMonkeyButtonBitmap),
    AvatarMonkeyButton = wxBitmapButton:new(TopRowPanel, ?wxID_ANY, AvatarMonkeyButtonBitmap),
    StartWaveButton = wxButton:new(TopRowPanel, ?wxID_ANY, [{label, "Start Wave"}]),

    GroundMonkeyButtonId = wxWindow:getId(GroundMonkeyButton), WaterMonkeyButtonId = wxWindow:getId(WaterMonkeyButton),
    FireMonkeyButtonId = wxWindow:getId(FireMonkeyButton), AirMonkeyButtonId = wxWindow:getId(AirMonkeyButton),
    AvatarMonkeyButtonId = wxWindow:getId(AvatarMonkeyButton), StartWaveButtonId = wxWindow:getId(StartWaveButton),

    TopRowSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(TopRowSizer, GroundMonkeyButton, [{flag, ?wxALL},{border,5}]), wxSizer:add(TopRowSizer, WaterMonkeyButton, [{flag, ?wxALL},{border,5}]),
    wxSizer:add(TopRowSizer, FireMonkeyButton, [{flag, ?wxALL},{border,5}]), wxSizer:add(TopRowSizer, AirMonkeyButton, [{flag, ?wxALL},{border,5}]),
    wxSizer:add(TopRowSizer, AvatarMonkeyButton, [{flag, ?wxALL},{border,5}]),
    %% ✅ FIX HERE: Correct syntax for adding the stretchable spacer.
    wxSizer:addStretchSpacer(TopRowSizer),
    wxSizer:add(TopRowSizer, StartWaveButton, [{flag, ?wxALL bor ?wxALIGN_CENTER_VERTICAL}, {border, 5}]),
    wxWindow:setSizer(TopRowPanel, TopRowSizer),

    PanelSizer = wxBoxSizer:new(?wxVERTICAL), wxSizer:add(PanelSizer, TopRowPanel, [{flag, ?wxEXPAND bor ?wxALL}]), wxSizer:add(PanelSizer, Board, [{proportion,1},{flag,?wxEXPAND bor ?wxALL}]),
    wxWindow:setSizer(MainPanel, PanelSizer), wxFrame:fit(Frame),
    wxPanel:connect(Board, paint, []), wxPanel:connect(Board, left_down, []), wxFrame:connect(Frame, close_window),
    wxWindow:connect(GroundMonkeyButton, command_button_clicked), wxWindow:connect(WaterMonkeyButton, command_button_clicked),
    wxWindow:connect(FireMonkeyButton, command_button_clicked), wxWindow:connect(AirMonkeyButton, command_button_clicked),
    wxWindow:connect(AvatarMonkeyButton, command_button_clicked), wxWindow:connect(StartWaveButton, command_button_clicked),

    wxFrame:show(Frame),
    {ok, #state{frame=Frame, board=Board, map_bitmap=MapBitmap, bitmaps=Bitmaps, ground_monkey_button_id=GroundMonkeyButtonId, water_monkey_button_id=WaterMonkeyButtonId, fire_monkey_button_id=FireMonkeyButtonId, air_monkey_button_id=AirMonkeyButtonId, avatar_monkey_button_id=AvatarMonkeyButtonId, start_wave_button_id = StartWaveButtonId}}.

%% The rest of the file is unchanged.
handle_cast({add_monkey,T,X,Y,I},S) -> NMaps=maps:put(I,{X,Y,T},S#state.monkeys), {noreply,S#state{monkeys=NMaps}};
handle_cast({delete_monkey,I},S) -> NMaps=maps:remove(I,S#state.monkeys), {noreply,S#state{monkeys=NMaps}};
handle_cast({add_balloon,I,T,P},S) -> NMaps=maps:put(I,{T,P},S#state.balloons), {noreply,S#state{balloons=NMaps}};
handle_cast({delete_balloon,I},S) -> NMaps=maps:remove(I,S#state.balloons), {noreply,S#state{balloons=NMaps}};
handle_cast({move_balloon,I,NP},S=#state{balloons=B})->case maps:is_key(I,B) of true->{T,_}=maps:get(I,B),NB=maps:put(I,{T,NP},B),{noreply,S#state{balloons=NB}}; false->{noreply,S} end;
handle_cast({add_dart,I,T,P},S) -> NMaps=maps:put(I,{T,P},S#state.darts), {noreply,S#state{darts=NMaps}};
handle_cast({delete_dart,I},S) -> NMaps=maps:remove(I,S#state.darts), {noreply,S#state{darts=NMaps}};
handle_cast({move_dart,I,NP},S=#state{darts=D})->case maps:is_key(I,D) of true->{T,_}=maps:get(I,D),ND=maps:put(I,{T,NP},D),{noreply,S#state{darts=ND}}; false->{noreply,S} end;
handle_cast(refresh, S) -> wxWindow:refresh(S#state.board), {noreply, S};
handle_cast(_,S) -> {noreply,S}.
handle_info(#wx{event=#wxPaint{}},S) -> DC=wxPaintDC:new(S#state.board),wxDC:drawBitmap(DC,S#state.map_bitmap,{0,0},[]),maps:foreach(fun(_,{X,Y,T})->BMP=maps:get(T,S#state.bitmaps),W=wxBitmap:getWidth(BMP),H=wxBitmap:getHeight(BMP),wxDC:drawBitmap(DC,BMP,{X-round(W/2),Y-round(H/2)},[{useMask,true}])end,S#state.monkeys),maps:foreach(fun(_,{T,{X,Y}})->BMP=maps:get(T,S#state.bitmaps),W=wxBitmap:getWidth(BMP),H=wxBitmap:getHeight(BMP),wxDC:drawBitmap(DC,BMP,{X-round(W/2),Y-round(H/2)},[{useMask,true}])end,S#state.balloons),maps:foreach(fun(_,{T,{X,Y}})->BMP=maps:get(T,S#state.bitmaps),W=wxBitmap:getWidth(BMP),H=wxBitmap:getHeight(BMP),wxDC:drawBitmap(DC,BMP,{X-round(W/2),Y-round(H/2)},[{useMask,true}])end,S#state.darts),wxPaintDC:destroy(DC),{noreply,S};
handle_info(#wx{event=#wxMouse{type=left_down,x=X,y=Y}},S=#state{placing=none})->io:format("GUI: Please select a monkey type first.~n"),{noreply,S};
handle_info(#wx{event=#wxMouse{type=left_down,x=X,y=Y}},S=#state{placing=MT})->gen_server:cast(main_server,{place_item,{MT,X,Y}}),{noreply,S#state{placing=none}};
handle_info(#wx{event=#wxClose{}},_S)->{stop,normal,_S};
handle_info(#wx{id=Id,event=#wxCommand{}},State)->case Id of S when S==State#state.ground_monkey_button_id->io:format("GUI: Selected ground_monkey.~n"),{noreply,State#state{placing=ground_monkey}};S when S==State#state.water_monkey_button_id->io:format("GUI: Selected water_monkey.~n"),{noreply,State#state{placing=water_monkey}};S when S==State#state.fire_monkey_button_id->io:format("GUI: Selected fire_monkey.~n"),{noreply,State#state{placing=fire_monkey}};S when S==State#state.air_monkey_button_id->io:format("GUI: Selected air_monkey.~n"),{noreply,State#state{placing=air_monkey}};S when S==State#state.avatar_monkey_button_id->io:format("GUI: Selected avatar_monkey.~n"),{noreply,State#state{placing=avatar_monkey}};S when S==State#state.start_wave_button_id->io:format("GUI: Start Wave button clicked!~n"),main_server:generate_level1(),{noreply,State};_->{noreply,State}end;
handle_info(_, S) -> {noreply, S}.
terminate(_,S)->maps:foreach(fun(_,B)->wxBitmap:destroy(B)end,S#state.bitmaps),wx:destroy(),ok.
handle_call(_,_,S)->{reply,ok,S}.
code_change(_,S,_) -> {ok,S}.
