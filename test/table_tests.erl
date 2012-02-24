-module(table_tests).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(NRPOSITIONSATTABLE, 4). 

start_server_test() ->
    ?assertMatch({ok,_Pid}, table:start()),
    ?assertEqual(ok, table:stop()).

table_test_() ->
    {foreach,
     fun setup/0,               % setup function
     fun teardown/1,            % teardown function
      [fun ?MODULE:should_add_player/0, % tests
       fun ?MODULE:should_add_player_when_entering_if_vacant_position/0,
       fun ?MODULE:should_be_possible_to_have_multiple_positions/0,
       fun ?MODULE:should_return_error_on_occupied_position/0,
       fun ?MODULE:should_return_error_on_invalid_position/0,
       fun ?MODULE:should_get_vacant_positions/0,
       fun ?MODULE:should_free_up_position_if_player_leaves_position/0,
       fun ?MODULE:should_return_error_if_leaving_non_taken_position/0,
       fun ?MODULE:should_leave_all_positions_when_leaving_table/0]}.

should_add_player() ->
    ?assertMatch({ok,1},table:enter_table("Kalle")).

should_add_player_when_entering_if_vacant_position() ->
    ?assertMatch({ok,1},table:enter_table("Kalle",1)),
    ?assertMatch({ok,2},table:enter_table("Pelle",2)).

should_be_possible_to_have_multiple_positions() ->
    ?assertMatch({ok,1},table:enter_table("Pelle",1)),
    ?assertMatch({ok,2},table:enter_table("Pelle",2)).

should_return_error_on_invalid_position() ->
    ?assertMatch({invalid_position,?NRPOSITIONSATTABLE+1},table:enter_table("Kalle",?NRPOSITIONSATTABLE+1)).

should_return_error_on_occupied_position() ->
    ?assertMatch({ok,1},table:enter_table("Kalle",1)),
    ?assertMatch({position_taken,1},table:enter_table("Pelle",1)).

should_get_vacant_positions() ->
    List = lists:seq(1,?NRPOSITIONSATTABLE),
    ?assertMatch({ok,List}, table:get_vacant_positions()),
    table:enter_table("Pelle", 3),
    List2 = lists:delete(3, List),
    ?assertMatch({ok,List2}, table:get_vacant_positions()).
    
should_free_up_position_if_player_leaves_position() ->
    ?assertMatch({ok,1}, table:enter_table("Kalle",1)),
    ?assertMatch(ok, table:leave_position(1)),
    List = lists:seq(1,?NRPOSITIONSATTABLE),
    ?assertMatch({ok,List}, table:get_vacant_positions()).

%Definition here is uncertain... No need to return error if leaving nontaken?
should_return_error_if_leaving_non_taken_position() ->
    ?assertMatch(position_not_taken, table:leave_position(1)).
  
%Definition here is uncertain... What if player is not at table?
%How to spawn another session and check that those players isn't removed?
should_leave_all_positions_when_leaving_table() ->
    ?assertMatch({ok,1}, table:enter_table("Kalle",1)),
    ?assertMatch({ok,2}, table:enter_table("Kalle",2)),
    ?assertMatch({ok,3}, table:enter_table("Pelle",3)), %Pid identifies player, not name....
    ?assertMatch(ok, table:leave_table()),
    List = lists:seq(1,?NRPOSITIONSATTABLE),
    ?assertMatch({ok,List}, table:get_vacant_positions()).

%% Internal functions
setup() ->
    table:start().

teardown(_) ->
    table:stop().
