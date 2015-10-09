-module(simple_window).

-compile([export_all]).

start() -> 
    io:format("wtest:start ~p~n", [self()]),
    w:new_frame("TESTING!").

-include_lib("eunit/include/eunit.hrl").

all_test() ->
    w_server:start(),
    start().