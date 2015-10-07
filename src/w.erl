-module(w).

-compile([export_all]).

-define(SERVER, w_server).

-include_lib("wx/include/wx.hrl").

% API for building wx GUIs.
%------------------------------------------------------------------

% Return types for this API:
-type frame_handle()            :: {frame, integer()}.
-type panel_handle()            :: {panel, integer()}.
-type button_handle()           :: {button, integer(), string()}.
-type toolbar_button_handle()   :: {button, integer(), string()}.
-type label_handle()            :: {label, integer()}.
-type textbox_handle()          :: {textbox, integer()}.
-type listbox_handle()          :: {listbox, integer()}.
-type combobox_handle()         :: {combobox, integer()}.
-type sizer_handle()            :: {box_sizer, integer()}.
-type grid_sizer_handle()       :: {grid_sizer, integer()}.
-type flexgrid_sizer_handle()   :: {flexgrid_sizer, integer()}.
-type control_handle() :: blank
                        | button_handle()
                        | toolbar_button_handle()
                        | label_handle()
                        | textbox_handle()
                        | listbox_handle()
                        | combobox_handle().

% Reusable types for common options:
-type opt_pos()     :: {pos, {integer(), integer()}}.
-type opt_size()    :: {size, {integer(), integer()}}.
-type opt_style()   :: {style, integer()}.

% Types for the various Options list arguments.
-type frame_options() :: [frame_option()].
-type frame_option() :: opt_pos()
                      | opt_size()
                      | opt_style().

-type panel_options() :: [panel_option()].
-type panel_option() :: {winid, integer()} % TODO: do we need this option?
                      |  opt_pos()
                      |  opt_size()
                      |  opt_style().

-type textbox_options() :: [textbox_option()].
-type textbox_option() :: opt_pos()
                        | opt_size().
            %TODO: style and validators! http://www.erlang.org/doc/man/wxTextCtrl.html#new-3

% Input types for arguments to builders:
-type toolbar_def() :: [toolbar_button_def()].
-type toolbar_button_def() :: {string(), string()}
                            | {string(), string(), string()}.

-type control_list_def() :: [control_def()].
-type control_def() :: blank
                     | {button, string()}
                     | {textbox, string()}
                     | {textbox, string(), textbox_options()}
                     | {label, string()}
                     | {listbox}
                     | {listbox, [string()]}
                     | {combobox}
                     | {combobox, [string()]}.

% Frame
%------------------------------------------------------------------
-spec new_frame(string()) -> frame_handle().
-spec new_frame(string(), frame_options()) -> frame_handle().
new_frame(Title) -> new_frame(Title, []).
new_frame(Title, Options) -> wx_object:call(?SERVER, {new_frame, Title, Options}).

-spec show(frame_handle()) -> ok.
show({frame, FrameId}) -> wx_object:call(?SERVER, {show, {frame, FrameId}}).

% Panel
%------------------------------------------------------------------
-spec new_panel(frame_handle()) -> panel_handle().
-spec new_panel(frame_handle(), panel_options()) -> panel_handle(). 
new_panel({frame, FrameId}) -> new_panel({frame, FrameId}, []).
new_panel({frame, FrameId}, Options) -> wx_object:call(?SERVER, {new_panel, FrameId, Options}).

-spec set_sizer(panel_handle(), sizer_handle()) -> ok.
set_sizer({panel, PanelId}, {box_sizer, SizerId}) -> 
    wx_object:call(?SERVER, {set_sizer, PanelId, SizerId}).

% Label
%------------------------------------------------------------------
-spec new_label(panel_handle(), string()) -> label_handle().
new_label({panel, PanelId}, Text) -> wx_object:call(?SERVER, {new_label, PanelId, Text}).

% See text manipulation section below, for get_text, set_text, append_text, clear.

% Statusbar
%------------------------------------------------------------------
-spec add_statusbar(frame_handle()) -> ok.
-spec add_statusbar(frame_handle(), string()) -> ok.
add_statusbar({frame, FrameId}) -> wx_object:call(?SERVER, {add_statusbar, FrameId}).
add_statusbar({frame, FrameId}, Text) -> 
    ok = wx_object:call(?SERVER, {add_statusbar, FrameId}),
    set_status({frame, FrameId}, Text).

-spec set_status(frame_handle(), string()) -> ok.
set_status({frame, FrameId}, Text) -> wx_object:call(?SERVER, {set_status, FrameId, Text}).

% Toolbar
%------------------------------------------------------------------
-spec add_toolbar(frame_handle(), toolbar_def()) -> [toolbar_button_handle()].
-spec add_toolbar(frame_handle(), toolbar_def(), integer(), integer ()) -> [toolbar_button_handle()].
% TODO: pass in styles
add_toolbar({frame, FrameId}, Definition) -> wx_object:call(?SERVER, {add_toolbar, FrameId, Definition, 16, 16}).
add_toolbar({frame, FrameId}, Definition, W, H) -> wx_object:call(?SERVER, {add_toolbar, FrameId, Definition, W, H}).

% Boxsizer
%------------------------------------------------------------------
% Create a new verticle box sizer.
-spec new_column_sizer() -> ok.
new_column_sizer() -> wx_object:call(?SERVER, {new_box_sizer, ?wxVERTICAL}).

% Create a new horizontal box sizer.
-spec new_row_sizer() -> sizer_handle().
new_row_sizer() -> wx_object:call(?SERVER, {new_box_sizer, ?wxHORIZONTAL}).

-spec set_min_size(sizer_handle(), integer(), integer()) -> ok.
set_min_size({box_sizer, SizerId}, Width, Height) -> wx_object:call(?SERVER, {set_min_size, SizerId, Width, Height}).

% TODO: appending spacers and children should work more like filling the grid sizer. See form2 example where the buttons are added.
append_child({box_sizer, ParentId}, {button, ChildId, _Text}) -> 
    append_child(ParentId, ChildId, []);
append_child({box_sizer, ParentId}, {Type, ChildId}) when Type == box_sizer; Type == grid_sizer; Type == flexgrid_sizer; Type == textbox; Type == label; Type == listbox -> 
    append_child(ParentId, ChildId, []).

append_child({box_sizer, ParentId}, {button, ChildId, _Text}, Options) -> 
    append_child(ParentId, ChildId, Options);
append_child({box_sizer, ParentId}, {Type, ChildId}, Options) when Type == box_sizer; Type == grid_sizer; Type == flexgrid_sizer; Type == textbox ->
    append_child(ParentId, ChildId, Options);
append_child(ParentId, ChildId, Flags) when is_integer(ParentId), is_integer(ChildId) -> 
    Flags2 = to_wx_flag(Flags),
    wx_object:call(?SERVER, {append_child, ParentId, ChildId, Flags2}).

append_spacer({box_sizer, ParentId}) -> append_spacer({box_sizer, ParentId}, 0).
append_spacer({box_sizer, SizerId}, Amount) -> wx_object:call(?SERVER, {append_spacer, SizerId, Amount}).

% Gridsizer
%------------------------------------------------------------------
%TODO: add an overload that uses a default padding value?
-spec new_grid_sizer(integer(), integer(), integer(), integer()) -> grid_sizer_handle().
new_grid_sizer(Rows, Columns, VerticlePadding, HorizontalPadding) ->
    wx_object:call(?SERVER, {new_grid_sizer, Rows, Columns, VerticlePadding, HorizontalPadding}).

% FlexGridsizer
%------------------------------------------------------------------
%TODO: add an overload that uses a default padding value?
-spec new_flexgrid_sizer(integer(), integer(), integer(), integer()) -> flexgrid_sizer_handle().
new_flexgrid_sizer(Rows, Columns, VerticlePadding, HorizontalPadding) ->
    wx_object:call(?SERVER, {new_flexgrid_sizer, Rows, Columns, VerticlePadding, HorizontalPadding}).

%NOTE: these Index parameters are zero-based to match the C++ indices!
expand_row(Handle = {flexgrid_sizer, _Id}, Index) -> expand_row(Handle, Index, 1).
expand_row({flexgrid_sizer, Id}, Index, Proportion) -> 
    wx_object:call(?SERVER, {expand_row, Id, Index, Proportion}).

%NOTE: these Index parameters are zero-based to match the C++ indices!
expand_col(Handle = {flexgrid_sizer, _Id}, Index) -> expand_col(Handle, Index, 1).
expand_col({flexgrid_sizer, Id}, Index, Proportion) -> wx_object:call(?SERVER, {expand_col, Id, Index, Proportion}).

% Population of Grid Sizer and FlexGrid Sizer:
%------------------------------------------------------------------
-spec fill_grid_sizer(grid_sizer_handle() | flexgrid_sizer_handle(), control_list_def()) -> ok.
fill_grid_sizer({Type, Id}, Controls) when Type == grid_sizer; Type == flexgrid_sizer -> wx_object:call(?SERVER, {fill_grid_sizer, Id, Controls}).
% TODO: how to pass expand option?


% Buttons
%------------------------------------------------------------------
-spec new_button(panel_handle, string()) -> button_handle().
new_button({panel, PanelId}, Text) -> wx_object:call(?SERVER, {new_button, PanelId, Text}).

-spec build_controls(panel_handle(), control_list_def()) -> [control_handle()].
build_controls(H = {panel, _PanelId}, Def) ->
    [build_control(H, Control) || Control <- Def].

build_control({panel, _Id}, blank) -> blank;
build_control(H = {panel, _Id}, {button, Text})           -> new_button(H, Text);
build_control(H = {panel, _Id}, {label, Text})            -> new_label(H, Text);
build_control(H = {panel, _Id}, {listbox})                -> new_listbox(H);
build_control(H = {panel, _Id}, {listbox, Items})         -> new_listbox(H, Items);
build_control(H = {panel, _Id}, {textbox})                -> new_textbox(H, "");
build_control(H = {panel, _Id}, {combobox})               -> new_combobox(H);
build_control(H = {panel, _Id}, {combobox, Items})        -> new_combobox(H, Items);
build_control(H = {panel, _Id}, {textbox, Text})          -> new_textbox(H, Text);
build_control(H = {panel, _Id}, {textbox, Text, Options}) -> new_textbox(H, Text, Options).

% Textbox constructors
%------------------------------------------------------------------
-spec new_textbox(panel_handle()) -> textbox_handle().
new_textbox(PanelHandle) -> new_textbox(PanelHandle, "", []).

-spec new_textbox(panel_handle(), string()) -> textbox_handle().
new_textbox(PanelHandle, Text) -> new_textbox(PanelHandle, Text, []).

%TODO: validators! http://www.erlang.org/doc/man/wxTextCtrl.html#new-3
-spec new_textbox(panel_handle(), string(), textbox_options()) -> textbox_handle().
new_textbox({panel, PanelId}, Text, Options) ->
    Options2 = to_wx_style([{value, Text}|Options]),
    wx_object:call(?SERVER, {new_textbox, PanelId, Options2}).

% Text manipulation functions for: Labels, Textboxes.
%------------------------------------------------------------------
-spec append_text(textbox_handle() | label_handle(), string()) -> ok.
append_text({Type, Id}, Text) when Type == textbox; Type == label -> 
    wx_object:call(?SERVER, {append_text, Id, Text}).

-spec get_text(textbox_handle() | label_handle()) -> string().
get_text({Type, Id}) when Type == textbox; Type == label -> 
    wx_object:call(?SERVER, {get_text, Id}).

-spec set_text(textbox_handle() | label_handle(), string()) -> ok.
set_text({Type, Id}, Text) when Type == textbox; Type == label ->
    wx_object:call(?SERVER, {set_text, Id, Text}).

-spec clear(textbox_handle() | label_handle()) -> ok.
clear({Type, Id}) when Type == textbox; Type == label ->
    wx_object:call(?SERVER, {clear, Id}).

% Listbox constructors
%------------------------------------------------------------------

% TODO: add options and listitems here ??
% TODO: size needs to be an option!
% TODO: multi-select listboxes?

-spec new_listbox(panel_handle()) -> listbox_handle().
new_listbox(H = {panel, _PanelId}) -> new_listbox(H, []).

-spec new_listbox(panel_handle(), list()) -> listbox_handle().
new_listbox({panel, PanelId}, Items) -> wx_object:call(?SERVER, {new_listbox, PanelId, Items}).

% Listbox manipulation functions
%------------------------------------------------------------------
fill_listbox({listbox, Id}, Items) -> wx_object:call(?SERVER, {fill_listbox, Id, Items}).

get_selection({listbox, Id}) -> wx_object:call(?SERVER, {get_listbox_selection, Id}).

set_selection({listbox, Id}, Text) -> wx_object:call(?SERVER, {select_listbox_selection, Id, Text}).


% Combobox constructors
%------------------------------------------------------------------
new_combobox(H = {panel, _PanelId}) -> new_combobox(H, []).

new_combobox({panel, PanelId}, Items) -> wx_object:call(?SERVER, {new_combobox, PanelId, Items}).

% Helpful utils to make WX easier to work with
%------------------------------------------------------------------

% Given a proplist, convert known styles into a single aggregated WX style tuple
% and return a new list containing that WX style tuple and all other non-style
% tuples from the original list. See unit tests below.
to_wx_style(Items) when is_list(Items) -> 
    Items2 = lists:map(fun to_wx_style/1, Items),
    {WxStyle, {others, UnmodifiedTuples}} = extract_style_codes(Items2),
    [WxStyle|UnmodifiedTuples];

to_wx_style(align_left)     -> ?wxTE_LEFT;
to_wx_style(align_center)   -> ?wxTE_CENTER;
to_wx_style(align_right)    -> ?wxTE_RIGHT;
to_wx_style(multiline)      -> ?wxTE_MULTILINE;
to_wx_style(Unknown)        -> Unknown.

extract_style_codes(Items) -> extract_style_codes(Items, 0, []).
extract_style_codes([], StyleCode, Tuples) ->
    {{style, StyleCode}, {others, Tuples}};
extract_style_codes([H|T], StyleCode, Tuples) when is_integer(H) ->
    extract_style_codes(T, StyleCode bor H, Tuples);
extract_style_codes([H|T], StyleCode, Tuples) ->
    extract_style_codes(T, StyleCode, [H|Tuples]).

% TODO combine these two features into one section!

to_wx_flag(Items) when is_list(Items) -> 
    Items2 = lists:map(fun to_wx_flag/1, Items),
    {WxStyle, {others, UnmodifiedTuples}} = extract_flag_codes(Items2),
    [WxStyle|UnmodifiedTuples];

to_wx_flag(all)         -> ?wxALL;
to_wx_flag(expand)      -> ?wxEXPAND;
to_wx_flag(top)         -> ?wxTOP;
to_wx_flag(bottom)      -> ?wxBOTTOM;
to_wx_flag(center)      -> ?wxCENTER;
to_wx_flag(Unknown)     -> Unknown.

extract_flag_codes(Items) -> extract_flag_codes(Items, 0, []).
extract_flag_codes([], FlagCode, Tuples) ->
    {{flag, FlagCode}, {others, Tuples}};
extract_flag_codes([H|T], FlagCode, Tuples) when is_integer(H) ->
    extract_flag_codes(T, FlagCode bor H, Tuples);
extract_flag_codes([H|T], FlagCode, Tuples) ->
    extract_flag_codes(T, FlagCode, [H|Tuples]).

% Experimental data binding.
%------------------------------------------------------------------
bind_values_to_controls([], []) -> ok;
bind_values_to_controls([{textbox, Id}|OtherControls], [Text|OtherValues]) ->
    ok = w:set_text({textbox, Id}, Text),
    bind_values_to_controls(OtherControls, OtherValues);
bind_values_to_controls([H = {listbox, _Id}|OtherControls], [Text|OtherValues]) ->
    ok = w:set_selection(H, Text),
    bind_values_to_controls(OtherControls, OtherValues);
%Ignore other types of controls. This way, we can bind to a mixed list without binding to, for example, labels.
bind_values_to_controls([_AnythingElse|OtherControls], Values) -> bind_values_to_controls(OtherControls, Values).


unbind_values_from_controls(Controls) when is_list(Controls) ->
    unbind_values_from_controls(Controls, []).

unbind_values_from_controls([], Values) -> lists:reverse(Values);
unbind_values_from_controls([H = {textbox, _Id}|Controls], Values) ->
    Value = w:get_text(H),
    unbind_values_from_controls(Controls, [Value | Values]);
unbind_values_from_controls([H = {listbox, _Id}|Controls], Values) ->
    Value = w:get_selection(H),
    unbind_values_from_controls(Controls, [Value | Values]);
unbind_values_from_controls([_AnythingElse|Controls], Values) ->
    unbind_values_from_controls(Controls, Values).

% TODO: get listbox selection

% %------------------------------------------------------------------
% % Unit tests: Move these into a separate module
% %------------------------------------------------------------------
% -include_lib("eunit/include/eunit.hrl").

% to_wx_style_test() ->
%     Input = [{value, "Cole"}, align_right, align_center, align_left, multiline, {size, {1,2}}, {pos, {3,4}}],
%     Output = to_wx_style(Input),
%     ?assertEqual(4, length(Output)),
%     ?assertEqual({value, "Cole"}, proplists:lookup(value, Output)),
%     ?assertEqual({size, {1,2}}, proplists:lookup(size, Output)),
%     ?assertEqual({pos, {3,4}}, proplists:lookup(pos, Output)),
%     ?assertEqual({style, ?wxTE_RIGHT bor ?wxTE_CENTER bor ?wxTE_LEFT bor ?wxTE_MULTILINE}, proplists:lookup(style, Output)).

% to_wx_flag_test() ->
%     Input = [{value, "Cole"}, expand, top, bottom],
%     Output = to_wx_flag(Input),
%     ?assertEqual(2, length(Output)),
%     ?assertEqual({value, "Cole"}, proplists:lookup(value, Output)),
%     ?assertEqual({flag, ?wxEXPAND bor ?wxTOP bor ?wxBOTTOM}, proplists:lookup(flag, Output)).


% -define(CONTROLS, [blank, {button, "Button 1"}, {label, "This is a label"},
%                     {textbox, "Textbox 1"}]).

% build_controls_test() ->
%     w_server:start(),
%     Frame = new_frame("My Frame"),
%     Panel = new_panel(Frame),
%     [Blank, B1, L1, T1] = build_controls(Panel, ?CONTROLS),
%     ?assertEqual(blank, Blank),
%     {button, B1Id, "Button 1"} = B1,
%     ?assert(is_integer(B1Id)),
%     {label, L1Id} = L1,
%     ?assert(is_integer(L1Id)),
%     ?assertNotEqual(B1Id, L1Id),
%     {textbox, T1Id} = T1,
%     ?assert(is_integer(T1Id)),
%     ?assertNotEqual(L1Id, T1Id),
%     ?assertEqual("Textbox 1", get_text(T1)),
%     % TODO: test textbox creation with options!
%     w_server:stop().

% simple_window_test() ->
%     w_server:start(), %Do this in a supervision tree instead!
%     Frame = {frame, _FrameId} = w:new_frame("TESTING!", [{size, {200, 200}}]),
%     ok = w:add_statusbar(Frame, "Statusbar text set quickly!"),
%     {panel, _PanelId} = w:new_panel(Frame),
%     ToolbarButtonDef = [
%         {"New", "wxART_NEW", "This is long help for 'New'"},
%         {"Press Me", "wxART_ERROR"},
%         {"Copy", "wxART_COPY", "Copy something to the clipboard"} %Long Help ends up in status bar!
%     ],
%     [
%         {button, _B1Id, "New"},
%         {button, _B2Id, "Press Me"},
%         {button, _B3Id, "Copy"}
%     ] = w:add_toolbar(Frame, ToolbarButtonDef),
%     ok = w:show(Frame),
%     ok = w_server:stop().

% textbox_test() ->
%     w_server:start(), %Do this in a supervision tree instead!
%     Frame = w:new_frame("Textbox tests!"),
%     Panel = w:new_panel(Frame),
%     Textbox1 = w:new_textbox(Panel),
%     Textbox2 = w:new_textbox(Panel, "This has text"),
%     ?assertEqual("", w:get_text(Textbox1)),
%     ?assertEqual("This has text", w:get_text(Textbox2)),
%     w:append_text(Textbox1, "more"),
%     w:append_text(Textbox2, "more"),
%     ?assertEqual("more", w:get_text(Textbox1)),
%     ?assertEqual("This has textmore", w:get_text(Textbox2)),
%     ok = w:clear(Textbox1),
%     ok = w:clear(Textbox2),
%     ?assertEqual("", w:get_text(Textbox1)),
%     ?assertEqual("", w:get_text(Textbox2)),
%     w:set_text(Textbox1, "Hi"),
%     w:set_text(Textbox2, "Hello"),
%     ?assertEqual("Hi", w:get_text(Textbox1)),
%     ?assertEqual("Hello", w:get_text(Textbox2)),
%     ok = w_server:stop().

% listbox_test() ->
%     w_server:start(), %Do this in a supervision tree instead!
%     Genres = ["Comedy", "Drama", "Epic", "Erotic", "Nonsense"],
%     Frame = w:new_frame("Listbox tests!"),
%     Panel = w:new_panel(Frame),
%     Listbox = w:new_listbox(Panel),
%     bind_values_to_controls([Listbox], [Genres]),
%     ok = w_server:stop().

% label_test() ->
%     w_server:start(),
%     Frame = w:new_frame("Listbox tests!"),
%     Panel = w:new_panel(Frame),
%     L1 = w:new_label(Panel, "Label One"),
%     ?assertEqual("Label One", w:get_text(L1)),
%     ok = w:set_text(L1, "New Text"),
%     ?assertEqual("New Text", w:get_text(L1)),
%     ok = w:append_text(L1, " More"),
%     ?assertEqual("New Text More", w:get_text(L1)),
%     ok = w:clear(L1),
%     ?assertEqual("", w:get_text(L1)),
%     ok = w_server:stop().

% databind_unbind_test() ->
%     w_server:start(),
%     Frame = w:new_frame("Listbox tests!"),
%     Panel = w:new_panel(Frame),
%     Genres = ["Comedy", "Drama", "Epic", "Erotic", "Nonsense"],
%     ControlDef = [  {label, "Text 1"}, {textbox},
%                     {label, "Text 2"}, {textbox, "", [multiline]},
%                     {label, "List 1"}, {listbox, Genres},
%                     {button, "OK"}],
%     Form = w:build_controls(Panel, ControlDef),
%     ValuesToBind = ["Textbox 1", "Multiline\nTextbox 2", "Epic"],
%     ok = w:bind_values_to_controls(Form, ValuesToBind),
%     Values = w:unbind_values_from_controls(Form),
%     ?assert(is_list(Values)),
%     ?assertEqual(3, length(Values)),
%     [Tb1, Tb2, Lb1] = Values,
%     ?assertEqual("Textbox 1", Tb1),
%     ?assertEqual("Multiline\nTextbox 2", Tb2),
%     ?assertEqual("Epic", Lb1),
%     ok = w_server:stop().


% % TODO: WRITE LOTS AND LOTS OF UNIT TESTS!
