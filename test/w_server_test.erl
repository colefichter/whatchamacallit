-module(w_server_test).

-compile([export_all]).

-include_lib("../src/w.hrl").
-include_lib("wx/include/wx.hrl").
-include_lib("eunit/include/eunit.hrl").

to_record_test() ->
    R1 = w_server:to_record({self(), nothing}, 999, textbox, not_a_real_wx_control),
    ?assertEqual(self(), w_server:owner_of(R1)),
    ?assertEqual(999, w_server:id_of(R1)),
    ?assertEqual(textbox, w_server:type_of(R1)),
    ?assertEqual(not_a_real_wx_control, w_server:wx_control_of(R1)),
    R2 = w_server:to_record({self(), nothing}, 1000, listbox, not_a_real_wx_control, "Some Text"),
    ?assertEqual(self(), w_server:owner_of(R2)),
    ?assertEqual(1000, w_server:id_of(R2)),
    ?assertEqual(listbox, w_server:type_of(R2)),
    ?assertEqual("Some Text", w_server:text_of(R2)),
    ?assertEqual(not_a_real_wx_control, w_server:wx_control_of(R2)).

simple_record_test() ->
    R = #control{id=999, type=button, owner_pid=self(), text="Fake Button", wx_control={test}},
    ?assertEqual(button, w_server:type_of(R)),
    ?assertEqual(999, w_server:id_of(R)),
    ?assertEqual(self(), w_server:owner_of(R)),
    ?assertEqual("Fake Button", w_server:text_of(R)),
    ?assertEqual({test}, w_server:wx_control_of(R)).