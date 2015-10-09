-module(toolbar_statusbar).

-export([start/0, init/0]).

start() -> spawn(?MODULE, init, []).

init() -> 
    Frame = w:new_frame("TESTING!", [{size, {200, 200}}]),
    w:add_statusbar(Frame, "Statusbar text set quickly!"),
    _Panel = w:new_panel(Frame),
    ToolbarButtonDef = [
        {"New", "wxART_NEW", "This is long help for 'New'"},
        {"Press Me", "wxART_ERROR"},
        {"Copy", "wxART_COPY", "Copy something to the clipboard"} %Long Help ends up in status bar!
    ],
    _Buttons = w:add_toolbar(Frame, ToolbarButtonDef),
    w:show(Frame),
    loop().

loop() ->
    receive
        stop -> ok;
        {click, {button, ButtonId, ButtonText}} ->
            io:format("Handling button click: Id ~p Text ~p~n", [ButtonId, ButtonText]),
            loop();
        Message ->
            io:format("CALLBACK INVOKED ~p: ~p~n", [self(), Message]),
            loop()
    end.

-include_lib("eunit/include/eunit.hrl").

all_test() ->
    w_server:start(),
    start().