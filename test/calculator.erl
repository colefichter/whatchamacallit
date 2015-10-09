-module(calculator).
-behaviour(gen_fsm).

-export([start_link/0, stop/0]).
-export([enter/1, add/1, subtract/1, waiting/2, waiting/3]).
-export([list_to_numeric/1, numeric_to_list/1]).
-export([init/1, terminate/3, code_change/4, handle_info/3, handle_event/3, handle_sync_event/4]).

% Client API
%-------------------------------------------------------------------------
start_link() -> gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

enter(Number) -> gen_fsm:sync_send_event(?MODULE, {enter, Number}).
add(Number) -> gen_fsm:send_event(?MODULE, {add, Number}).
subtract(Number) -> gen_fsm:send_event(?MODULE, {subtract, Number}).

stop() -> gen_fsm:send_all_state_event(?MODULE, stop).

% FSM Implementation
%-------------------------------------------------------------------------
init([]) -> {ok, waiting, []}.

waiting({add, Number}, LoopData) -> {next_state, waiting, ['+'|[Number|LoopData]]};
waiting({subtract, Number}, LoopData) -> {next_state, waiting, ['-'|[Number|LoopData]]}.

waiting({enter, Number}, _From, LoopData) -> 
    Expression = [Number|LoopData],
    Result = evaluate(Expression),
    io:format("~p = ~p~n", [lists:reverse(Expression), Result]),
    {reply, Result, waiting, []}.

% Internal helpers
evaluate(Operations) -> evaluate_int(lists:reverse(Operations), 0).

evaluate_int([], Result) -> Result;
evaluate_int([H|T], _Result) when is_number(H) -> evaluate_int(T, H);
evaluate_int(['+'|T], Result) -> 
    [Next|T2] = T,
    io:format("~p + ~p~n", [Result, Next]),
    evaluate_int(T2, Result+Next);
evaluate_int(['-'|T], Result) -> 
    [Next|T2] = T,
    io:format("~p - ~p~n", [Result, Next]),
    evaluate_int(T2, Result-Next).


list_to_numeric(L) when is_list(L) ->
    Float = (catch erlang:list_to_float(L)),
    Int = (catch erlang:list_to_integer(L)),
    case is_float(Float) of
        true -> Float;
        false -> Int
    end.

numeric_to_list(X) when is_float(X) -> float_to_list(X, [{decimals, 9}, compact]);
numeric_to_list(X) when is_integer(X) -> integer_to_list(X).

%---------------------------------------------------------------------------------------------
% Unused callbacks required by gen_fsm
%---------------------------------------------------------------------------------------------
%handle_sync_event(stop, _From, StateName, StateData) -> {stop, normal, StateData};
handle_sync_event(_Any, _From, StateName, StateData) -> {ok, StateName, StateData}.
handle_info(_Any, StateName, StateData) -> {ok, StateName, StateData}.
handle_event(stop, _StateName, LoopData) -> {stop, normal, LoopData};
handle_event(_Any, StateName, StateData) -> {ok, StateName, StateData}.
code_change(_OldVsn, StateName, StateData, _Extra) -> {ok, StateName, StateData}.
terminate(_Any, StateName, StateData) -> {ok, StateName, StateData}.

% Unit tests
%-------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").

evaluate_test() ->
    calculator:start_link(),
    ?assertEqual(5, evaluate([2, '+', 3])),
    ?assertEqual(9, evaluate([4, '+', 2, '+', 3])),
    ?assertEqual(1, evaluate([4, '-', 2, '+', 3])),
    ?assertEqual(5, evaluate([4, '+', 2, '-', 3])),
    ?assertEqual(-15, evaluate([20, '-', 4, '+', 2, '-', 3])),
    ?assertEqual(5.85, evaluate([2.71, '+', 3.14])),
    calculator:stop().

list_to_numeric_test() ->
    ?assertEqual(10, list_to_numeric("10")),
    ?assertEqual(true, is_integer(list_to_numeric("10"))),
    ?assertEqual(10.5, list_to_numeric("10.5")),
    ?assertEqual(true, is_number(list_to_numeric("10"))),
    ?assertEqual(0, list_to_numeric("0")),
    ?assertEqual(0.0, list_to_numeric("0.0")),
    ?assertEqual(-1, list_to_numeric("-1")),
    ?assertEqual(-10.5, list_to_numeric("-10.5")).

numeric_to_list_test() ->
    ?assertEqual("10", numeric_to_list(10)),
    ?assertEqual("10.5", numeric_to_list(10.5)),
    ?assertEqual("0", numeric_to_list(0)),
    ?assertEqual("0.0", numeric_to_list(0.0)),
    ?assertEqual("-1", numeric_to_list(-1)),
    ?assertEqual("-10.5", numeric_to_list(-10.5)).