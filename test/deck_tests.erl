-module(deck_tests).
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(TOTALNRCARDS, 6*52).
-define(NRINITIALCARDSTOSHUFFLE, round(?TOTALNRCARDS*0.8)).

start_server_test_() ->
    ?_assertMatch({ok,_Pid}, deck:start()).

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
    validateCard(Card).

should_not_be_time_to_shuffle() ->
    deck:get_card(),
    ?assertEqual(false,deck:is_time_to_shuffle()),
    drawNcards(random:uniform(?NRINITIALCARDSTOSHUFFLE-2)),    
    ?assertEqual(false,deck:is_time_to_shuffle()).

should_be_time_to_shuffle() -> 
    drawNcards(?NRINITIALCARDSTOSHUFFLE),
    ?assertEqual(true,deck:is_time_to_shuffle()).

should_shuffle_deck() ->
    drawNcards(?NRINITIALCARDSTOSHUFFLE),
    deck:shuffle(),
    ?assertEqual(false,deck:is_time_to_shuffle()).

should_assert_on_to_many_drawn_cards() ->
  drawNcards(?TOTALNRCARDS-1),
  ?assertError(undef, deck:draw_card()).

%% Internal functions
setup() ->
    deck:start().

teardown(_) ->
    deck:stop().

drawNcards(N) ->
     [deck:get_card() || _Foo <- lists:seq(1,N)].

validateCard({Value,Suite}) ->
    ?assertEqual(true,validateCardSuite(Suite)),
    ?assertEqual(true,validateCardValue(Value)).

validateCardSuite(Suite) ->
    ValidSuites = [spades, hearts, clubs, diamonds],
    lists:member(Suite,ValidSuites).    
    
validateCardValue(Value)-> 
    ValidValues = lists:seq(2,10) ++ [jack, queen, king, ace],
    lists:member(Value,ValidValues).
    
