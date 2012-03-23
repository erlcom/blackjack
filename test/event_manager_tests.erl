-module(event_manager_tests).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

start_server_test() ->
    ?assertMatch({ok,_Pid}, table:start()),
    ?assertEqual(ok, table:stop()).

table_test_() ->
    {foreach,
     fun setup/0,               % setup function
     fun teardown/1,            % teardown function
     [fun ?MODULE:should_foo/0]}. % tests

should_foo() ->
    ?assertMatch(1,1).

%% Internal functions
setup() ->
    table:start().

teardown(_) ->
    table:stop().
