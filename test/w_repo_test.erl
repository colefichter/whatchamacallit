-module(w_repo_test).

-compile([export_all]).

-include_lib("../src/w.hrl").

-include_lib("eunit/include/eunit.hrl").

set_get_control_test() ->
    ?assertEqual(error, w_repo:get_control(5)),
    Control = #control{owner_pid=self(), id=5, type=button, wx_control={nothing_here}},
    ?assertEqual(ok, w_repo:set_control(5, Control)),
    ?assertEqual({ok, Control}, w_repo:get_control(5)).

set_get_wx_server_test() ->
    ?assertEqual(ok, w_repo:set_wx_server(blah)),
    ?assertEqual(blah, w_repo:get_wx_server()).

identity_test() ->
    ?assertError(badarith, w_repo:next_id()), % ID not created yet.
    ?assertEqual(ok, w_repo:create_identity()),
    ?assertEqual(1, w_repo:next_id()),
    ?assertEqual(2, w_repo:next_id()),
    ?assertEqual(3, w_repo:next_id()).