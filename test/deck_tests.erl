-module(deck_tests).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(TOTALNRCARDS, 6*52).
-define(NRINITIALCARDSTOSHUFFLE, round(?TOTALNRCARDS*0.8)).

start_server_test() ->
    ?assertMatch({ok,_Pid}, deck:start()),
    ?assertEqual(ok, deck:stop()).

deck_test_() ->
    {foreach,
     fun setup/0,               % setup function
     fun teardown/1,            % teardown function
      [fun ?MODULE:should_get_card/0,     % tests
       fun ?MODULE:should_not_be_time_to_shuffle/0,
       fun ?MODULE:should_be_time_to_shuffle/0,
       fun ?MODULE:should_shuffle_deck/0,
       fun ?MODULE:should_assert_on_to_many_drawn_cards/0]}.

should_get_card() ->
    Card = deck:get_card(),
    validate_card(Card).

should_not_be_time_to_shuffle() ->
    deck:get_card(),
    ?assertEqual(false,deck:is_time_to_shuffle()),
    draw_cards(random:uniform(?NRINITIALCARDSTOSHUFFLE-2)),    
    ?assertEqual(false,deck:is_time_to_shuffle()).

should_be_time_to_shuffle() -> 
    draw_cards(?NRINITIALCARDSTOSHUFFLE),
    ?assertEqual(true,deck:is_time_to_shuffle()).

should_shuffle_deck() ->
    draw_cards(?NRINITIALCARDSTOSHUFFLE),
    deck:shuffle(),
    ?assertEqual(false,deck:is_time_to_shuffle()).

should_assert_on_to_many_drawn_cards() ->
    draw_cards(?TOTALNRCARDS),
    ?assertError(undef, deck:draw_card()).

%% Internal functions
setup() ->
    deck:start().

teardown(_) ->
    deck:stop().

draw_cards(N) ->
     [deck:get_card() || _ <- lists:seq(1,N)].

validate_card({Suite,Value}) ->
    ?assertEqual(true, validate_card_suite(Suite)),
    ?assertEqual(true, validate_card_value(Value)).

validate_card_suite(Suite) ->
    ValidSuites = [spades, hearts, clubs, diamonds],
    lists:member(Suite,ValidSuites).    
    
validate_card_value(Value)-> 
    ValidValues = lists:seq(2,10) ++ [jack, queen, king, ace],
    lists:member(Value, ValidValues).
    
