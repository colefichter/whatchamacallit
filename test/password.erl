-module(password).

-export([start/0, init/0]).

start() -> spawn(?MODULE, init, []).

init() ->
    w_server:start(), %Do this in a supervision tree instead!

    Frame = w:new_frame("Whatchamacallit Password Textbox", [{size, {300, 100}}]),
    Panel = w:new_panel(Frame),
    HBox = w:new_row_sizer(),

    ControlDef = [
        % TODO: vertically align the label! It looks ugly.
        {label, "Password"}, {password, "SECRET!"}
    ],
    Form = w:build_controls(Panel, ControlDef),
    [w:append_child(HBox, C) || C <- Form],

    w:set_sizer(Panel, HBox),

    w:show(Frame),
    loop(Form).

loop(Form) ->
    receive 
        {closing, {frame, _FrameId}} ->
            print_values(Form),
            exit(normal);
        Msg ->
            io:format("CALLBACK ~p~n", [Msg])            
    end,
    loop(Form).

print_values(Form) ->
    [Password] = w:unbind_values_from_controls(Form),
    io:format("The entered password was: ~p~n", [Password]).

-include_lib("eunit/include/eunit.hrl").

all_test() ->
    w_server:start(),
    start().