-module(deck_tests).
-include_lib("eunit/include/eunit.hrl").

test_test() ->
    ?assertEqual(false,false).

start_test() ->
    {Res, _Pid} = deck:start(),
    ?assertEqual(ok, Res).
