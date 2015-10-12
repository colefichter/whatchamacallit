-module(dropdownlist).

-export([start/0, init/0]).

start() -> spawn(?MODULE, init, []).

init() ->
    w_server:start(), %Do this in a supervision tree instead!

    Frame = w:new_frame("Whatchamacallit Combobox", [{size, {300, 100}}]),
    Panel = w:new_panel(Frame),
    HBox = w:new_row_sizer(),

    Greetings = ["Hello", "Hi", "Bojour", "Guten Tag"],
    ControlDef = [
        % TODO: vertically align the label! It looks ugly.
        {label, "Salutation"}, {dropdownlist, Greetings}
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

    % TODO: this gets the selection. How to get entered text?
    %  Beter find a WX tutorial on the combobox.

    [Greeting] = w:unbind_values_from_controls(Form),
    io:format("The selected greeting was: ~p~n", [Greeting]).

-include_lib("eunit/include/eunit.hrl").

all_test() ->
    w_server:start(),
    start().