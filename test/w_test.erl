-module(w_test).

-compile([export_all]).

-include_lib("wx/include/wx.hrl").
-include_lib("eunit/include/eunit.hrl").

to_wx_style_test() ->
    Input = [{value, "Cole"}, align_right, align_center, align_left, multiline, {size, {1,2}}, {pos, {3,4}}],
    Output = w:to_wx_style(Input),
    ?assertEqual(4, length(Output)),
    ?assertEqual({value, "Cole"}, proplists:lookup(value, Output)),
    ?assertEqual({size, {1,2}}, proplists:lookup(size, Output)),
    ?assertEqual({pos, {3,4}}, proplists:lookup(pos, Output)),
    ?assertEqual({style, ?wxTE_RIGHT bor ?wxTE_CENTER bor ?wxTE_LEFT bor ?wxTE_MULTILINE}, proplists:lookup(style, Output)).

to_wx_style_password_test() ->
    Input = [password],
    Output = w:to_wx_style(Input),
    ?assertEqual({style, ?wxTE_PASSWORD}, proplists:lookup(style, Output)).

to_wx_flag_test() ->
    Input = [{value, "Cole"}, expand, top, bottom],
    Output = w:to_wx_flag(Input),
    ?assertEqual(2, length(Output)),
    ?assertEqual({value, "Cole"}, proplists:lookup(value, Output)),
    ?assertEqual({flag, ?wxEXPAND bor ?wxTOP bor ?wxBOTTOM}, proplists:lookup(flag, Output)).


-define(CONTROLS, [blank, {button, "Button 1"}, {label, "This is a label"},
                    {textbox, "Textbox 1"}]).

build_controls_test() ->
    w_server:start(),
    Frame = w:new_frame("My Frame"),
    Panel = w:new_panel(Frame),
    [Blank, B1, L1, T1] = w:build_controls(Panel, ?CONTROLS),
    ?assertEqual(blank, Blank),
    {button, B1Id, "Button 1"} = B1,
    ?assert(is_integer(B1Id)),
    {label, L1Id} = L1,
    ?assert(is_integer(L1Id)),
    ?assertNotEqual(B1Id, L1Id),
    {textbox, T1Id} = T1,
    ?assert(is_integer(T1Id)),
    ?assertNotEqual(L1Id, T1Id),
    ?assertEqual("Textbox 1", w:get_text(T1)),
    % TODO: test textbox creation with options!
    w_server:stop().

simple_window_test() ->
    w_server:start(), %Do this in a supervision tree instead!
    Frame = {frame, _FrameId} = w:new_frame("TESTING!", [{size, {200, 200}}]),
    ok = w:add_statusbar(Frame, "Statusbar text set quickly!"),
    {panel, _PanelId} = w:new_panel(Frame),
    ToolbarButtonDef = [
        {"New", "wxART_NEW", "This is long help for 'New'"},
        {"Press Me", "wxART_ERROR"},
        {"Copy", "wxART_COPY", "Copy something to the clipboard"} %Long Help ends up in status bar!
    ],
    [
        {button, _B1Id, "New"},
        {button, _B2Id, "Press Me"},
        {button, _B3Id, "Copy"}
    ] = w:add_toolbar(Frame, ToolbarButtonDef),
    ok = w:show(Frame),
    timer:sleep(50),
    ok = w:hide(Frame),
    ok = w_server:stop().

textbox_test() ->
    w_server:start(),
    Frame = w:new_frame("Textbox tests!"),
    Panel = w:new_panel(Frame),
    Textbox1 = w:new_textbox(Panel),
    Textbox2 = w:new_textbox(Panel, "This has text"),
    ?assertEqual("", w:get_text(Textbox1)),
    ?assertEqual("This has text", w:get_text(Textbox2)),
    w:append_text(Textbox1, "more"),
    w:append_text(Textbox2, "more"),
    ?assertEqual("more", w:get_text(Textbox1)),
    ?assertEqual("This has textmore", w:get_text(Textbox2)),
    ok = w:clear(Textbox1),
    ok = w:clear(Textbox2),
    ?assertEqual("", w:get_text(Textbox1)),
    ?assertEqual("", w:get_text(Textbox2)),
    w:set_text(Textbox1, "Hi"),
    w:set_text(Textbox2, "Hello"),
    ?assertEqual("Hi", w:get_text(Textbox1)),
    ?assertEqual("Hello", w:get_text(Textbox2)),
    ok = w_server:stop().

listbox_test() ->
    w_server:start(),
    Genres = ["Comedy", "Drama", "Epic", "Erotic", "Nonsense"],
    Frame = w:new_frame("Listbox tests!"),
    Panel = w:new_panel(Frame),
    Listbox = {listbox, _Id} = w:new_listbox(Panel, Genres),
    w:bind_values_to_controls([Listbox], ["Epic"]),    
    ?assertEqual("Epic", w:get_selection(Listbox)),
    ?assertEqual(["Epic"], w:unbind_values_from_controls([Listbox])),
    ok = w_server:stop().

combobox_test() ->
    w_server:start(),
    Genres = ["Comedy", "Drama", "Epic", "Erotic", "Nonsense"],
    Frame = w:new_frame("Listbox tests!"),
    Panel = w:new_panel(Frame),
    Combobox = {combobox, _Id} = w:new_combobox(Panel, Genres),
    w:bind_values_to_controls([Combobox], ["Epic"]),    
    ?assertEqual("Epic", w:get_selection(Combobox)),
    ?assertEqual(["Epic"], w:unbind_values_from_controls([Combobox])),
    ok = w_server:stop().

dropdownlist_test() ->
    w_server:start(),
    Genres = ["Comedy", "Drama", "Epic", "Erotic", "Nonsense"],
    Frame = w:new_frame("Listbox tests!"),
    Panel = w:new_panel(Frame),
    DDL = {dropdownlist, _Id} = w:new_dropdownlist(Panel, Genres),
    w:bind_values_to_controls([DDL], ["Epic"]),    
    ?assertEqual("Epic", w:get_selection(DDL)),
    ?assertEqual(["Epic"], w:unbind_values_from_controls([DDL])),
    ok = w_server:stop().   

label_test() ->
    w_server:start(),
    Frame = w:new_frame("Listbox tests!"),
    Panel = w:new_panel(Frame),
    L1 = w:new_label(Panel, "Label One"),
    ?assertEqual("Label One", w:get_text(L1)),
    ok = w:set_text(L1, "New Text"),
    ?assertEqual("New Text", w:get_text(L1)),
    ok = w:append_text(L1, " More"),
    ?assertEqual("New Text More", w:get_text(L1)),
    ok = w:clear(L1),
    ?assertEqual("", w:get_text(L1)),
    ok = w_server:stop().

databind_unbind_test() ->
    w_server:start(),
    Frame = w:new_frame("Listbox tests!"),
    Panel = w:new_panel(Frame),
    Genres = ["Comedy", "Drama", "Epic", "Erotic", "Nonsense"],
    ControlDef = [  {label, "Text 1"}, {textbox},
                    {label, "Text 2"}, {textbox, "", [multiline]},
                    {label, "List 1"}, {listbox, Genres},
                    {button, "OK"}],
    Form = w:build_controls(Panel, ControlDef),
    ValuesToBind = ["Textbox 1", "Multiline\nTextbox 2", "Epic"],
    ok = w:bind_values_to_controls(Form, ValuesToBind),
    Values = w:unbind_values_from_controls(Form),
    ?assert(is_list(Values)),
    ?assertEqual(3, length(Values)),
    [Tb1, Tb2, Lb1] = Values,
    ?assertEqual("Textbox 1", Tb1),
    ?assertEqual("Multiline\nTextbox 2", Tb2),
    ?assertEqual("Epic", Lb1),
    ok = w_server:stop().