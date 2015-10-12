-module(w_server).
-behaviour(wx_object).

% Client API
-export([start/0, start/1, stop/0]).

% wx_object callbacks
-export([init/1, terminate/2,  code_change/3, handle_info/2, handle_cast/2, handle_call/3, handle_event/2]).

% Exports for convenience/testing
-export([new_panel/2, build_toolbar/5, to_record/4, to_record/5, type_of/1, id_of/1, owner_of/1, text_of/1,
        wx_control_of/1]).

-include_lib("w.hrl").
-include_lib("wx/include/wx.hrl").

% Client API
%------------------------------------------------------------------
start() -> 
    wx_object:start_link({local, ?MODULE}, ?MODULE, [w_repo], []),
    {ok, self()}.

start(RepoModule) -> 
    wx_object:start_link({local, ?MODULE}, ?MODULE, [RepoModule], []),
    {ok, self()}.

stop() -> wx_object:cast(?MODULE, stop).

% Server Implementation (wx_object behaviour callbacks)
%------------------------------------------------------------------
init([RepoModule]) ->
    UselessWindow = initialize_services(RepoModule),
    {UselessWindow, []}.

% Terminate the process loop when the window closes (via the close button X in the top corner).
handle_event(Msg = #wx{id=ControlId, event=#wxClose{}}, State) -> 
    io:format("*** wxClose. ~p~n   STATE: ~p~n", [Msg, State]),
    ControlRecord = get_control(ControlId),
    % Simulate closing the window. This allows the client PID to access state as long as needed.
    % When the client PID exits, we'll destroy the WX state for real.
    WxControl = wx_control_of(ControlRecord),
    wxWindow:hide(WxControl), 
    % Send callback so the GUI can cleanup get state of window controls:
    ClientPid = owner_of(ControlRecord),
    ClientPid ! {closing, {frame, ControlId}},
    {noreply, State};

% handle_event(Msg = #wx{id=?wxID_EXIT}, State) -> 
%     % TODO: this is wrong. Cleanup state (which is not in State).
%     io:format("*** wxID_EXIT. ~p~n   STATE: ~p~n", [Msg, State]),
%     wxWindow:destroy(State),
%     {noreply, State};
% Handle menu & toolbar click events:

handle_event(Msg = #wx{id=ControlId, event=#wxCommand{ type=command_menu_selected }}, State) -> 
    io:format("wxCommand menu click. ~p ~p~n", [self(), Msg]),
    ControlRecord = get_control(ControlId),
    ClientPid = owner_of(ControlRecord),
    Text = text_of(ControlRecord),
    io:format("   sending callback to client ~p~n", [ClientPid]),
    ClientPid ! {click, {button, ControlId, Text}},
    {noreply, State};
% Handle regular button click events:
handle_event(Msg = #wx{id=ButtonId, event=#wxCommand{type = command_button_clicked}}, State) ->
    io:format("wxCommand button click. ~p ~p~n", [self(), Msg]),
    ControlRecord = get_control(ButtonId),
    ClientPid = owner_of(ControlRecord),
    Text = text_of(ControlRecord),    
    io:format("   sending callback to client ~p~n", [ClientPid]),
    ClientPid ! {click, {button, ButtonId, Text}},  %TODO: differentiate between toolbar & regular buttons?
    {noreply, State};
handle_event(_Msg, State) -> {noreply, State}.


% Frame
%------------------------------------------------------------------
handle_call({new_frame, Title, Options}, From, State) ->
    {Id, WxFrame} = new_window(Title, Options), %TODO: options
    set_control(to_record(From, Id, frame, WxFrame)),
    {ClientPid, _} = From,
    % This can cause multiple DOWN messages to be sent. Can we prevent that from happening?
    erlang:monitor(process, ClientPid),
    {reply, {frame, Id}, State};
handle_call({show, {frame, FrameId}}, _From, State) ->
    load_control_and_run(FrameId, wxWindow, show),
    {reply, ok, State};
handle_call({hide, {frame, FrameId}}, _From, State) ->
    load_control_and_run(FrameId, wxWindow, hide),
    {reply, ok, State};

% Panel
%------------------------------------------------------------------
handle_call({new_panel, FrameId, Options}, From, State) ->
    {Id, WxPanel} = load_control_and_run(FrameId, ?MODULE, new_panel, [Options]),
    set_control(to_record(From, Id, panel, WxPanel)),
    {reply, {panel, Id}, State};

handle_call({set_sizer, PanelId, SizerId}, _From, State) ->
    WxPanel = get_wx_control(PanelId),
    WxSizer = get_wx_control(SizerId),
    wxPanel:setSizer(WxPanel, WxSizer),
    {reply, ok, State};

% Label
%------------------------------------------------------------------
handle_call({new_label, PanelId, Text}, From, State) ->
    WxPanel = get_wx_control(PanelId),
    Id = next_id(),
    WxControl = wxStaticText:new(WxPanel, Id, Text),
    set_control(to_record(From, Id, label, WxControl, Text)),
    {reply, {label, Id}, State};

% Statusbar
%------------------------------------------------------------------
handle_call({add_statusbar, FrameId}, _From, State) ->
    load_control_and_run(FrameId, wxFrame, createStatusBar),
    {reply, ok, State};
handle_call({set_status, FrameId, Text}, _From, State) ->
    load_control_and_run(FrameId, wxFrame, setStatusText, [Text]),
    {reply, ok, State};

% Toolbar
%------------------------------------------------------------------
handle_call({add_toolbar, FrameId, Def, W, H}, From, State) ->
    Buttons = load_control_and_run(FrameId, ?MODULE, build_toolbar, [From, Def, W, H]),
    {reply, Buttons, State};

% Boxsizer
%------------------------------------------------------------------
handle_call({new_box_sizer, Orientation}, From, State) ->
    WxSizer = wxBoxSizer:new(Orientation),
    Id = next_id(),
    set_control(to_record(From, Id, box_sizer, WxSizer)),
    {reply, {box_sizer, Id}, State};

handle_call({set_min_size, SizerId, Width, Height}, _From, State) ->
    load_control_and_run(SizerId, wxSizer, setMinSize, [Width, Height]),
    {reply, ok, State};

handle_call({append_child, ParentId, ChildId, Flags}, _From, State) ->
    WxParent = get_wx_control(ParentId),
    WxChild = get_wx_control(ChildId),
    wxSizer:add(WxParent, WxChild, Flags),
    {reply, ok, State};

handle_call({append_spacer, SizerId, Amount}, _From, State) ->
    load_control_and_run(SizerId, wxSizer, addSpacer, [Amount]),
    {reply, ok, State};

% Gridsizer
%------------------------------------------------------------------
handle_call({new_grid_sizer, Rows, Columns, VerticlePadding, HorizontalPadding}, From, State) ->
    WxSizer = wxGridSizer:new(Rows, Columns, VerticlePadding, HorizontalPadding),
    Id = next_id(),
    set_control(to_record(From, Id, grid_sizer, WxSizer)),
    {reply, {grid_sizer, Id}, State};

% FlexGrid sizer
%------------------------------------------------------------------
handle_call({new_flexgrid_sizer, Rows, Columns, VerticlePadding, HorizontalPadding}, From, State) ->
    WxSizer = wxFlexGridSizer:new(Rows, Columns, VerticlePadding, HorizontalPadding),
    Id = next_id(),
    set_control(to_record(From, Id, flexgrid_sizer, WxSizer)),
    {reply, {flexgrid_sizer, Id}, State};

% handle_call({fill_flexgrid_sizer, GsId, Controls}, _From, State) ->
%     WxSizer = get_wx_control(GsId),
%     fill_grid_sizer(WxSizer, Controls),
%     {reply, ok, State};

handle_call({expand_row, Id, Index, Proportion}, _From, State) ->
    load_control_and_run(Id, wxFlexGridSizer, addGrowableRow, [Index, [{proportion, Proportion}]]),
    {reply, ok, State};
handle_call({expand_col, Id, Index, Proportion}, _From, State) ->
    load_control_and_run(Id, wxFlexGridSizer, addGrowableCol, [Index, [{proportion, Proportion}]]),
    {reply, ok, State};

% Population of Grid Sizer and FlexGrid Sizer:
%------------------------------------------------------------------
handle_call({fill_grid_sizer, GsId, Controls}, _From, State) ->
    WxSizer = get_wx_control(GsId),
    fill_grid_sizer(WxSizer, Controls),
    {reply, ok, State};

% Buttons
%------------------------------------------------------------------
handle_call({new_button, PanelId, Text}, From, State) ->
    WxPanel = get_wx_control(PanelId),
    Button = new_button(WxPanel, From, Text),
    {reply, Button, State};


% Textbox constructors
%------------------------------------------------------------------
handle_call({new_textbox, PanelId, Options}, From, State) ->
    Id = next_id(),
    WxTextbox = load_control_and_run(PanelId, wxTextCtrl, new, [Id, Options]),
    set_control(to_record(From, Id, textbox, WxTextbox)),
    {reply, {textbox, Id}, State};

% Text manipulation functions for: Label, Textbox
%------------------------------------------------------------------
handle_call({append_text, ControlId, Text}, _From, State) ->    
    append_text(ControlId, Text),
    {reply, ok, State};
handle_call({get_text, ControlId}, _From, State) ->
    Text = get_text(ControlId),
    {reply, Text, State};
handle_call({set_text, ControlId, Text}, _From, State) -> 
    set_text(ControlId, Text),
    {reply, ok, State};
handle_call({clear, ControlId}, _From, State) ->
    clear_text(ControlId),
    {reply, ok, State};

% Listbox constructors
%------------------------------------------------------------------
handle_call({new_listbox, PanelId, Items}, From, State) ->
    Id = next_id(),
    % TODO: deal with options
    WxListbox = load_control_and_run(PanelId, wxListBox, new, [Id, [{size, {-1,50}}, {choices, Items}]]), % TODO: size needs to be an option!
    set_control(to_record(From, Id, listbox, WxListbox)),
    {reply, {listbox, Id}, State};

% Listbox manipulation functions
%------------------------------------------------------------------
handle_call({fill_listbox, ListboxId, Items}, _From, State) ->
    load_control_and_run(ListboxId, wxListBox, set, [Items]),
    {reply, ok, State};

handle_call({get_listbox_selection, Id}, _From, State) ->
    Selection = load_control_and_run(Id, wxControlWithItems, getStringSelection),
    {reply, Selection, State};

handle_call({select_listbox_selection, Id, Text}, _From, State) ->
    load_control_and_run(Id, wxControlWithItems, setStringSelection, [Text]),
    {reply, ok, State};

% Combobox constructors
%------------------------------------------------------------------
handle_call({new_combobox, PanelId, Items}, From, State) ->
    Id = next_id(),
    % TODO: deal with options
    WxListbox = load_control_and_run(PanelId, wxComboBox, new, [Id, [{choices, Items}]]), % TODO: size needs to be an option!
    set_control(to_record(From, Id, combobox, WxListbox)),
    {reply, {combobox, Id}, State};


% Dropdownlist constructors
%------------------------------------------------------------------
handle_call({new_dropdownlist, PanelId, Items}, From, State) ->
    Id = next_id(),
    % TODO: deal with options
    WxListbox = load_control_and_run(PanelId, wxChoice, new, [Id, [{choices, Items}]]), % TODO: size needs to be an option!
    set_control(to_record(From, Id, dropdownlist, WxListbox)),
    {reply, {dropdownlist, Id}, State};


% Unused wx_object callbacks
%------------------------------------------------------------------
handle_call(Msg, _From, State) -> {reply, {unknown_message, Msg}, State}.

handle_info({'DOWN', _Ref, process, ClientPid, _Reason}, State) ->
    %io:format("CLEANUP CONTROLS ~p ~p ~p~n", [Ref, ClientPid, Reason]),
    cleanup_all_controls(ClientPid),
    {noreply, State};
handle_info(_Msg, State)    -> {noreply, State}.
handle_cast(stop, State)    -> {stop, normal, State};
handle_cast(_Msg, State)    -> {noreply, State}.
code_change(_, _, State)    -> {stop, ignore, State}.
terminate(_Reason, _State)  -> ok.

%%%%%
%% TODO: break internal functions into a stand-alone module!
%%%%%

% Internal Functions
%------------------------------------------------------------------
initialize_services(RepoModule) ->
    put(repo, RepoModule),
    create_identity(),
    WxServer = wx:new(),
    set_wx_server(WxServer),
    UselessWindow = wxWindow:new(),
    UselessWindow.

new_window(Title, Options) ->
    Id = next_id(),
    WxFrame = wxFrame:new(get_wx_server(), Id, Title, Options),
    % Terminate the process loop when the window closes:
    wxFrame:connect(WxFrame, close_window),
    wxFrame:connect(WxFrame, command_menu_selected), %Toolbar & menu commands
    wxFrame:connect(WxFrame, command_button_clicked), %Regular button commands
    {Id, WxFrame}.

new_panel(WxFrame, [])      -> {next_id(), wxPanel:new(WxFrame)};
new_panel(WxFrame, Options) -> {next_id(), wxPanel:new(WxFrame, Options)}.

% Load a wxControl by id, then invoke the given Fun with the wxControl as the first or only argument.
% EG:   load_control_and_run(ControlId, wxFrame, setStatusText, [Text]) invokes
%       wxFrame:setStatusText(TheWxFrameObject, Text)
load_control_and_run(ControlId, Mod, Fun) -> load_control_and_run(ControlId, Mod, Fun, []).
load_control_and_run(ControlId, Mod, Fun, ExtraArgs) ->
    WxControl = get_wx_control(ControlId), % TODO: what to do if control is not found?
    erlang:apply(Mod, Fun, [WxControl|ExtraArgs]).

build_toolbar(WxFrame, From, Def, W, H) ->
    % TODO: pass in styles
    WxToolbar = wxFrame:createToolBar(WxFrame, [{style, ?wxNO_BORDER bor ?wxTB_HORIZONTAL}]),
    Buttons = [new_toolbar_button(WxToolbar, From, X, W, H) || X <- Def],
    wxToolBar:realize(WxToolbar),
    wxFrame:setToolBar(WxFrame,WxToolbar),
    Buttons.

new_toolbar_button(Toolbar, From, {Title, IconName}, W, H) ->
    Icon = get_bitmap(IconName, W, H),    
    Id = next_id(),    
    WxButton = wxToolBar:addTool(Toolbar, Id, Title, Icon, [{shortHelp, Title}]),
    set_control(to_record(From, Id, button, WxButton, Title)),
    {button, Id, Title};
new_toolbar_button(Toolbar, From, {Title, IconName, LongHelp}, W, H) ->
    Button = {button, Id, Title} = new_toolbar_button(Toolbar, From, {Title, IconName}, W, H),
    wxToolBar:setToolLongHelp(Toolbar, Id, LongHelp),
    Button.

get_bitmap(Name, W, H) -> wxArtProvider:getBitmap(Name, [{size, {W, H}}]).

fill_grid_sizer(Sizer, Def) -> [add_to_grid_sizer(Sizer, X) || X <- Def].


% Text manipulation helpers
%------------------------------------------------------------------
get_text(ControlId) when is_integer(ControlId) ->
    Control = get_control(ControlId),
    get_text(Control, type_of(Control)).

get_text(Control, textbox) -> wxTextCtrl:getValue(wx_control_of(Control));
get_text(Control, label) -> wxStaticText:getLabel(wx_control_of(Control)).

set_text(ControlId, Text) when is_integer(ControlId) ->
    Control = get_control(ControlId),
    set_text(Control, type_of(Control), Text).

set_text(Control, textbox, Text) -> wxTextCtrl:setValue(wx_control_of(Control), Text);
set_text(Control, label, Text)   -> wxStaticText:setLabel(wx_control_of(Control), Text).

clear_text(ControlId) when is_integer(ControlId) ->
    Control = get_control(ControlId),
    clear_text(Control, type_of(Control)).

clear_text(Control, textbox) -> wxTextCtrl:clear(wx_control_of(Control));
clear_text(Control, label)   -> wxStaticText:setLabel(wx_control_of(Control), "").

append_text(ControlId, Text) ->
    Control = get_control(ControlId),
    append_text(Control, type_of(Control), Text).

append_text(Control, textbox, Text) -> wxTextCtrl:appendText(wx_control_of(Control), Text);
append_text(Control, label, Text)   ->
    WxControl = wx_control_of(Control),
    CurrentText = wxStaticText:getLabel(WxControl),
    wxStaticText:setLabel(WxControl, CurrentText ++ Text).


% TODO: pull options out. Refactor and conbine similar clauses.
add_to_grid_sizer(Sizer, blank) -> 
    wxSizer:addSpacer(Sizer, 0);
add_to_grid_sizer(Sizer, {label, Id}) ->
    add_to_grid_sizer_by_id(Sizer, Id, [{flag, ?wxEXPAND}]); % TODO: need to handle proportion too.
add_to_grid_sizer(Sizer, {textbox, Id}) ->
    add_to_grid_sizer_by_id(Sizer, Id, [{flag, ?wxEXPAND}]); % TODO: does it make sense to have this as a default?
add_to_grid_sizer(Sizer, {listbox, Id}) ->
    add_to_grid_sizer_by_id(Sizer, Id, [{flag, ?wxEXPAND}]); % TODO: options??
add_to_grid_sizer(Sizer, {button, Id, _Text}) ->
    add_to_grid_sizer_by_id(Sizer, Id, [{proportion, 0}, {flag, ?wxEXPAND}]); % TODO: options!
add_to_grid_sizer(Sizer, {box_sizer, Id}) -> add_to_grid_sizer_by_id(Sizer, Id, []).

add_to_grid_sizer_by_id(Sizer, Id, Options) ->
    WxChildControl = get_wx_control(Id),
    wxSizer:add(Sizer, WxChildControl, Options).

new_button(WxPanel, From, Text) ->
    Id = next_id(),
    WxButton = wxButton:new(WxPanel, Id, [{label, Text}]),
    set_control(to_record(From, Id, button, WxButton, Text)),
    {button, Id, Text}.

cleanup_all_controls(ClientPid) ->
    ControlRecords = get_controls_by_owner_pid(ClientPid),
    % It's not clear WHY, but we seem to have to delete the parent controls before their children to avoid strange
    % errors from the C library that underpins the WX stuff.
    SortedControlRecords = sort_controls_by_id(ControlRecords),
    cleanup(SortedControlRecords).

cleanup([]) -> ok;
cleanup([ControlRecord|T]) ->
    %io:format(" deleting control ~p ~p ~p~n", [id_of(ControlRecord), type_of(ControlRecord), wx_control_of(ControlRecord)]),
    destroy_wx_control(ControlRecord),
    remove_control(id_of(ControlRecord)),
    cleanup(T).

sort_controls_by_id(Controls) when is_list(Controls) ->
    lists:sort(fun(A,B) -> A#control.id =< B#control.id end, Controls).

% WxWindow is the base class for most controls, but not invisible things like sizers.
destroy_wx_control(#control{type=box_sizer, wx_control=WxControl} = Control) when is_record(Control, control) ->
    % ERL docs don't show a destroy() function for wxSizer: http://www.erlang.org/doc/man/wxSizer.html
    wxBoxSizer:destroy(WxControl);
destroy_wx_control(#control{type=grid_sizer, wx_control=WxControl} = Control) when is_record(Control, control) ->
    wxFlexGridSizer:destroy(WxControl);
destroy_wx_control(#control{type=flexgrid_sizer, wx_control=WxControl} = Control) when is_record(Control, control) ->
    wxGridSizer:destroy(WxControl);
destroy_wx_control(#control{type=button, wx_control=WxControl} = Control) when is_record(Control, control) ->
    wxButton:destroy(WxControl);
destroy_wx_control(#control{wx_control=WxControl} = Control) when is_record(Control, control) ->
    wxWindow:destroy(WxControl).

% Record Manipulation
%------------------------------------------------------------------
type_of(R) when is_record(R, control)       -> R#control.type.
id_of(R) when is_record(R, control)         -> R#control.id.
text_of(R) when is_record(R, control)       -> R#control.text.
owner_of(R) when is_record(R, control)      -> R#control.owner_pid.
wx_control_of(R) when is_record(R, control) -> R#control.wx_control.

to_record({OwnerPid, _}, Id, Type, WxControl) -> 
    #control{owner_pid=OwnerPid, id=Id, type=Type, wx_control=WxControl}.
to_record({OwnerPid, _}, Id, Type, WxControl, Text) -> 
    #control{owner_pid=OwnerPid, id=Id, type=Type, wx_control=WxControl, text=Text}.

% Wrapper around the repo:
%------------------------------------------------------------------
repo() -> get(repo).

create_identity() -> (repo()):create_identity().
next_id() -> (repo()):next_id().

get_wx_server() -> (repo()):get_wx_server().

set_wx_server(Server) -> (repo()):set_wx_server(Server).

get_control(ControlId) ->
    {ok, ControlRecord} = (repo()):get_control(ControlId),
    ControlRecord.

get_wx_control(ControlId) ->
    {ok, ControlRecord} = (repo()):get_control(ControlId),
    wx_control_of(ControlRecord).

set_control(ControlRecord) when is_record(ControlRecord, control) ->
    (repo()):set_control(id_of(ControlRecord), ControlRecord).

remove_control(ControlId) ->
    ok = (repo()):remove_control(ControlId).

get_controls_by_owner_pid(OwnerPid) ->
    (repo()):get_controls_by_owner_pid(OwnerPid).