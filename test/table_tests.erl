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
       fun ?MODULE:should_leave_all_positions_when_leaving_table/0,
       fun ?MODULE:should_handle_multiple_player_pids/0,
       fun ?MODULE:should_handle_player_deaths/0,
       fun ?MODULE:should_not_leave_other_players_position/0]}.

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
  
should_leave_all_positions_when_leaving_table() ->
    ?assertMatch({ok,1}, table:enter_table("Kalle",1)),
    ?assertMatch({ok,2}, table:enter_table("Kalle",2)),
    ?assertMatch({ok,3}, table:enter_table("Pelle",3)), %Pid identifies player, not name....
    ?assertMatch(ok, table:leave_table()),
    List = lists:seq(1,?NRPOSITIONSATTABLE),
    ?assertMatch({ok,List}, table:get_vacant_positions()).

should_handle_multiple_player_pids() ->
    {Player1,Player2} = spawn_and_enter_two_players("Kalle","Pelle"),
    List = lists:seq(3,?NRPOSITIONSATTABLE),
    ?assertMatch({ok,List}, table:get_vacant_positions()),
    sendMessage(Player1,die),
    sendMessage(Player2,die).

should_handle_player_deaths() ->
    Player1 = spawn_and_enter("Kalle"),
    sendMessage(Player1,die),
    List = lists:seq(1,?NRPOSITIONSATTABLE),
    ?assertMatch({ok,List}, table:get_vacant_positions()).

should_not_leave_other_players_position() ->
    {Player1,Player2} = spawn_and_enter_two_players("Kalle","Pelle"),
    sendMessage(Player2,leave_first_position),
    List = lists:seq(3,?NRPOSITIONSATTABLE),
    ?assertMatch({ok,List}, table:get_vacant_positions()),
    sendMessage(Player1,die),
    sendMessage(Player2,die).

%% Internal functions
setup() ->
    table:start().

teardown(_) ->
    table:stop().

spawn_player(Name) ->
    spawn(?MODULE, playerLoop,[Name]).

sendMessage(Player,Msg) ->
    Player ! {Msg,self()},
    receive
	{_PlayerPid,_Msg,done} -> 
	    ok
    end.

playerLoop(Name) ->
    receive 
        {enter,Pid} ->
	    table:enter_table(Name),
	    Pid ! {self(),enter,done},
	    playerLoop(Name);
	{leave_table,Pid} ->
	    table:leave_table(),
	    Pid ! {self(),leaveTable,done},
	    playerLoop(Name);
	{leave_first_position,Pid} ->
	    table:leave_position(1),
	    Pid ! {self(),leave_first_position,done},
	    playerLoop(Name);
	{die,Pid} ->
	    Pid ! {self(),die,done},
	    ok
	end.

spawn_and_enter(PlayerName) ->
    PlayerPid = spawn_player(PlayerName),
    sendMessage(PlayerPid,enter),
    PlayerPid.
    
spawn_and_enter_two_players(PlayerName1,PlayerName2) ->
    Player1 = spawn_and_enter(PlayerName1),
    Player2 = spawn_and_enter(PlayerName2),
    {Player1,Player2}.
