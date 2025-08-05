-module(w).
-export([start/0, init/0]).

-include_lib("wx/include/wx.hrl").

-define(BUTTON_WIDTH, 80).
-define(BUTTON_HEIGHT, 80).
-define(WINDOW_WIDTH, 1000).
-define(WINDOW_HEIGHT, 1000).
-define(MAP_WIDTH, 700).
-define(MAP_HEIGHT, 600).

start() ->
    spawn(fun ?MODULE:init/0).

init() ->
    wx:new(),
    wxImage:initStandardHandlers(),
    MainPID = self(),
    Frame = wxFrame:new(wx:null(), ?wxID_ANY, "Monkey Menu"),
    Panel = wxPanel:new(Frame),
    wxPanel:setBackgroundColour(Panel, {255, 255, 255}),

    FolderPath = "/home/csestudent/Desktop/bar/",
    DartMonkeyPath = FolderPath ++ "mmmmmmonkey.png",
    FireMonkeyPath = FolderPath ++ "fire_monkey.png",
    MapPath = FolderPath ++ "map.png",

    DartMonkeyImage = wxImage:new(DartMonkeyPath),
    FireMonkeyImage = wxImage:new(FireMonkeyPath),
    MapImage = wxImage:new(MapPath),

    DartMonkeyScaled = wxImage:scale(DartMonkeyImage, ?BUTTON_WIDTH, ?BUTTON_HEIGHT),
    FireMonkeyScaled = wxImage:scale(FireMonkeyImage, ?BUTTON_WIDTH, ?BUTTON_HEIGHT),
    MapScaled = wxImage:scale(MapImage, ?MAP_WIDTH, ?MAP_HEIGHT),

    DartMonkeyBitmap = wxBitmap:new(DartMonkeyScaled),
    FireMonkeyBitmap = wxBitmap:new(FireMonkeyScaled),
    MapBitmap = wxBitmap:new(MapScaled),

    DartMonkeyWidget = wxStaticBitmap:new(Panel, ?wxID_ANY, DartMonkeyBitmap),
    FireMonkeyWidget = wxStaticBitmap:new(Panel, ?wxID_ANY, FireMonkeyBitmap),
    MapWidget = wxStaticBitmap:new(Panel, ?wxID_ANY, MapBitmap),

    PanelSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(PanelSizer, DartMonkeyWidget, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxTOP bor ?wxBOTTOM}, {border, 10}]),
    wxSizer:add(PanelSizer, FireMonkeyWidget, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxBOTTOM}, {border, 10}]),
    wxSizer:add(PanelSizer, MapWidget, [{flag, ?wxALIGN_CENTER_HORIZONTAL bor ?wxBOTTOM}, {border, 10}]),
    wxPanel:setSizer(Panel, PanelSizer),

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(MainSizer, Panel, [{proportion, 1}, {flag, ?wxEXPAND}]),
    wxFrame:setSizer(Frame, MainSizer),
    wxFrame:setSize(Frame, {?WINDOW_WIDTH, ?WINDOW_HEIGHT}),

    wxFrame:connect(Frame, close_window),
    wxStaticBitmap:connect(DartMonkeyWidget, left_down),
    wxStaticBitmap:connect(FireMonkeyWidget, left_down),

    wxFrame:show(Frame),
    InitialState = #{x => undefined, y => undefined},
    loop(Frame, DartMonkeyWidget, FireMonkeyWidget, MainPID, InitialState).

custom_input_dialog(ParentPID, ParentFrame, Prompt) ->
    DialogFrame = wxFrame:new(ParentFrame, ?wxID_ANY, "Input Required"),
    DialogPanel = wxPanel:new(DialogFrame),
    Text = wxStaticText:new(DialogPanel, ?wxID_ANY, Prompt),
    InputBox = wxTextCtrl:new(DialogPanel, ?wxID_ANY, [{value, "0"}]),
    OK_Button = wxButton:new(DialogPanel, ?wxID_ANY, [{label, "OK"}]),

    Sizer = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(Sizer, Text, [{flag, ?wxALL}, {border, 5}]),
    wxSizer:add(Sizer, InputBox, [{flag, ?wxEXPAND bor ?wxALL}, {border, 5}]),
    wxSizer:add(Sizer, OK_Button, [{flag, ?wxALIGN_CENTER bor ?wxALL}, {border, 5}]),
    DialogPanel:setSizer(Sizer),
    wxFrame:fit(DialogFrame),

    wxButton:connect(OK_Button, command_button_clicked),
    wxFrame:connect(DialogFrame, close_window),

    wxFrame:show(DialogFrame),
    dialog_loop(DialogFrame, OK_Button, InputBox, ParentPID).

dialog_loop(DialogFrame, OK_Button, InputBox, ParentPID) ->
    receive
        #'wx'{obj=OK_Button, event=#'wxCommand'{type=command_button_clicked}} ->
            Value = wxTextCtrl:getValue(InputBox),
            ParentPID ! {input_result, Value},
            wxFrame:destroy(DialogFrame);
        #'wx'{obj=DialogFrame, event=#'wxClose'{}} ->
            wxFrame:destroy(DialogFrame)
    end.

loop(Frame, DartMonkeyWidget, FireMonkeyWidget, MainPID, State) ->
    receive
        #'wx'{obj=Frame, event=#'wxClose'{}} ->
            io:format("Closing window.~n");

        #'wx'{obj=DartMonkeyWidget, event=#'wxMouse'{type=left_down}} ->
            io:format("Dart monkey clicked!~n"),
            spawn(fun() -> custom_input_dialog(MainPID, Frame, "Enter X Coordinate:") end),
            loop(Frame, DartMonkeyWidget, FireMonkeyWidget, MainPID, State);

        #'wx'{obj=FireMonkeyWidget, event=#'wxMouse'{type=left_down}} ->
            io:format("Fire monkey clicked!~n"),
            spawn(fun() -> custom_input_dialog(MainPID, Frame, "Enter X Coordinate:") end),
            loop(Frame, DartMonkeyWidget, FireMonkeyWidget, MainPID, State);

        {input_result, ValueX} ->
            io:format("Value for X has been saved: ~s~n", [ValueX]),
            NewState = State#{x => ValueX},
            loop(Frame, DartMonkeyWidget, FireMonkeyWidget, MainPID, NewState);

        _Other ->
            % !! THIS IS THE FIXED LINE !! It now correctly passes all 5 arguments.
            loop(Frame, DartMonkeyWidget, FireMonkeyWidget, MainPID, State)
    end,
    wx:destroy().