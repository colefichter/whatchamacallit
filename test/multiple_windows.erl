-module(multiple_windows).

-export([start/0]).

start() -> spawn(fun init/0).

init() ->
    Windows = [make_window(Title) || Title <- ["Window 1", "Window 2"]],
    loop(Windows).

make_window(Title) ->
    Frame = w:new_frame(Title, [{size, {100, 150}}]),
    Panel = w:new_panel(Frame),
    Textbox = w:new_textbox(Panel, Title), % Add it to the panel?
    w:show(Frame),
    {Frame, Textbox}.

loop([]) -> io:format("GUI exiting.~n");
loop(Windows) ->
    receive 
        {closing, Handle = {frame, _FrameId}} ->
            io:format("CLOSING CALLBACK~n"),
            print_textbox_values(Windows),
            NewWindows = lists:keydelete(Handle, 1, Windows),
            loop(NewWindows);
        Msg ->
            io:format("CALLBACK ~p~n", [Msg]),
            print_textbox_values(Windows),
            loop(Windows)
    end.

print_textbox_values([]) -> ok;
print_textbox_values([{_Window, TB}|Tail]) ->
    Text = w:get_text(TB),
    io:format(" TEXTBOX id ~p text ~p~n", [TB, Text]),
    print_textbox_values(Tail).

-include_lib("eunit/include/eunit.hrl").

all_test() ->
    w_server:start(),
    start().