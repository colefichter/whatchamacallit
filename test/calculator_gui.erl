-module(calculator_gui).

-export([start/0, init/0]).

start() -> 
    %Start the calculator's compute process:
    calculator:start_link(),
    spawn(?MODULE, init, []).

init() -> 
    Frame = w:new_frame("Whatchamacallit Calculator!", [{size, {350, 200}}]),
    Panel = w:new_panel(Frame),
    Sizer = w:new_column_sizer(), % for the whole window
    GS = w:new_grid_sizer(5, 4, 3, 3), % for the buttons

    % The digit readout
    Display = w:new_textbox(Panel, "", [align_right]),
    w:append_child(Sizer, Display, [expand, top, bottom]),
    w:append_spacer(Sizer, 10),

    ButtonDef = [
        % Top button row
        {button, "Quit"}, blank, blank, {button, "Clear"}, % TODO: how to handle quit button?
        % Second button row
        {button, "7"}, {button, "8"}, {button, "9"}, {button, "Del"},
        % Third button row
        {button, "4"}, {button, "5"}, {button, "6"}, {button, "-"},
        % Fourth button row
        {button, "1"}, {button, "2"}, {button, "3"}, {button, "+"},
        % Bottom button row
        blank, {button, "0"}, {button, "."}, {button, "="}
    ],
    Buttons = w:build_controls(Panel, ButtonDef),
    w:fill_grid_sizer(GS, Buttons),

    % Put the assembled controls onto the panel
    w:append_child(Sizer, GS, [expand]), % TODO: this expand flag doesn't seem to do much of anything...
    w:set_sizer(Panel, Sizer),
    w:set_min_size(Sizer, 300, 200),

    w:show(Frame),
    loop(Display).

loop(Display) -> %Display is the textbox control at the top of the calculator.
    receive
        {click, {button, _Id, "Quit"}} -> 
            % TODO: figure out how to handle exits (in both directions)
            exit(normal);
        {click, {button, _Id, "Clear"}} -> 
            w:clear(Display);
        {click, {button, _Id, "Del"}} ->
            Text = w:get_text(Display),
            case length(Text) of
                0 -> nothing;
                Len -> w:set_text(Display, string:left(Text, Len - 1))
            end;
        {click, {button, _Id, "+"}} ->
            Text = w:get_text(Display),
            X = calculator:list_to_numeric(Text),
            calculator:add(X),
            w:clear(Display);
        {click, {button, _Id, "-"}} -> 
            Text = w:get_text(Display),
            X = calculator:list_to_numeric(Text),
            calculator:subtract(X),
            w:clear(Display);
        {click, {button, _Id, "="}} ->
            Text = w:get_text(Display),
            X = calculator:list_to_numeric(Text),
            Result = calculator:enter(X),
            io:format("RESULT ~p~n", [Result]),
            w:set_text(Display, calculator:numeric_to_list(Result));
        {click, {button, _Id, Character}} -> % "0" to "9" and "."
            w:append_text(Display, Character);
        Message -> io:format("Unknown callback invoked ~p: ~p~n", [self(), Message])
    end,
    loop(Display).

-include_lib("eunit/include/eunit.hrl").

all_test() ->
    w_server:start(),
    start().