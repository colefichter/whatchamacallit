-module(form).

-export([start/0, init/0]).

start() -> spawn(?MODULE, init, []).

init() ->
    Frame = w:new_frame("Whatchamacallit Form", [{size, {300, 400}}]),
    Panel = w:new_panel(Frame),
    HBox = w:new_column_sizer(),
    ButtonBox = w:new_row_sizer(),
    FlexGridSizer = w:new_flexgrid_sizer(5, 2, 9, 25),

    Genres = ["Comedy", "Drama", "Epic", "Erotic", "Nonsense"],
    ControlDef = [
        {label, "Title"}, {textbox, ""}, % Just for fun, we'll use data binding (below).
        {label, "Author"}, {textbox, ""},
        {label, "Genre"}, {listbox, Genres},
        {label, "Review"}, {textbox, "", [multiline]},
        blank % This keeps the buttons in the right column.
    ],
    Form = w:build_controls(Panel, ControlDef),

    % This is a bit clumsy, but I want two buttons in the right column, beneath the textboxes:
    ButtonDef = [{button, "Save"}, {button, "Cancel"}],
    Buttons = w:build_controls(Panel, ButtonDef),    
    [w:append_child(ButtonBox, B, [center]) || B <- Buttons],

    % TODO: how to send EXPAND flag?
    w:fill_grid_sizer(FlexGridSizer, Form ++ [ButtonBox]),

    % Make the multiline Review TB (in the 3rd row) grow vertically when the window is resized.    
    w:expand_row(FlexGridSizer, 3), %NOTE: the index is zero-based!

    % Make the right col grow horizontally when the window is resized.
    w:expand_col(FlexGridSizer, 1), %NOTE: the index is zero-based!

    % Border ~= margin in css? Seems like it.    
    w:append_child(HBox, FlexGridSizer, [{border, 15}, {proportion, 1}, all, expand]),
    % TODO: change "border" to "margin", since that's what it is?

    w:set_sizer(Panel, HBox),

    % Experimental data binding.
    Values = ["War and Peace", "Tolstoy", "Drama", "Seems like it will never end..."],
    w:bind_values_to_controls(Form, Values), % Note: the library is ignoring the label controls!

    w:show(Frame),
    loop(Form).

loop(Form) ->
    receive 
        {click, {button, _Id, "Save"}} ->
            print_values(Form),
            exit(normal);
        {click, {button, _Id, "Cancel"}} ->
            io:format("Cancelling~n"),
            exit(normal);
        Msg ->
            io:format("CALLBACK ~p~n", [Msg])            
    end,
    loop(Form).

print_values(Form) ->
    [Title, Author, Genre, Review] = w:unbind_values_from_controls(Form),
    io:format("Saving values!~n Title: ~p~n Author: ~p~n Genre: ~p~n Review: ~p~n", 
        [Title, Author, Genre, Review]).

-include_lib("eunit/include/eunit.hrl").

all_test() ->
    w_server:start(),
    start().