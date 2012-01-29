 %%%-------------------------------------------------------------------
%%% @doc 
%%% The task for week 1 is to implement this module, deck.
%%% @end
%%%-------------------------------------------------------------------
-module(deck).

-behaviour(gen_server).

%% API
-export([start/0]).
-export([stop/1]).
-export([is_time_to_shuffle/1]).
-export([shuffle/1]).
-export([get_card/1]).
-export([get_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {deck=[], cardsLeft=0}).

-define(NRDECKS, 4).
-define(SPLITPERCENTAGE, 0.8).

-type suite() :: spades | hearts | clubs | diamonds.
-type value() :: 2..10 | jack | queen | king | ace.
-type card() :: {suite(), value()}.

%%Questions:
%% Sync call vs async call?
%% Return val? Spara undan gen server PID?
%% 1st arg to gen_server:call = gen server PID?
%% Ändra state vs "göra ett nytt"
%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start() -> {ok, pid()}.
start() ->
    gen_server:start_link(?MODULE, [], []). 

-spec stop(_Pid) -> ok.
stop(Pid) ->
    gen_server:call(Pid, terminate).

-spec is_time_to_shuffle(_Pid) -> boolean().
is_time_to_shuffle(Pid) ->
    gen_server:call(Pid, isItTimeToShuffle).

-spec shuffle(_Pid) -> {ok}.
shuffle(Pid) ->
    gen_server:call(Pid, shuffle).

-spec get_card(_Pid) -> card().
get_card(Pid) ->
    gen_server:call(Pid, getCard).

-spec get_info(_Pid) -> {ok}.
get_info(Pid) ->
    gen_server:call(Pid, getInfo),
    {ok}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {_result, NewState} = internal_shuffle(),
    {ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(getCard, _From, State) ->    
    NewState = State#state{deck=tl(State#state.deck), cardsLeft=State#state.cardsLeft -1},
    {reply, hd(State#state.deck), NewState};

handle_call(isItTimeToShuffle, _From, State) ->
    {reply, verifyShuffle(State), State};

handle_call(shuffle, _From, _State) ->
    {Result, NewState} = internal_shuffle(),
    {reply, Result, NewState};

handle_call(getInfo, _From, State) ->
    io:fwrite("The number of cards until shuffle is ~w \n", [State#state.cardsLeft]),
    io:fwrite("The deck is ~w \n", [State#state.deck]),
    {reply, ok, State};

handle_call(terminate, _From, State) ->
    NewState = State#state{deck=[], cardsLeft=0},    
    {reply, ok, NewState}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

internal_shuffle() ->
    Deck = [{S,V} || S <- lists:seq(2,10) ++ [jack, queen, king, ace] , V <- [spades, hearts,clubs,diamonds]],
    DeckList = lists:flatten(lists:duplicate(?NRDECKS,Deck)),
    ShuffledDecks = random_list(DeckList),
    SplitNr = round(length(ShuffledDecks)*?SPLITPERCENTAGE),
    NewState = #state{deck=ShuffledDecks,cardsLeft=SplitNr},
    {ok, NewState}.

random_list(List) ->                                          
   random:seed(now()),
   {NewList, _} = lists:foldl( fun(_El, {Acc,Rest}) ->          
       RandomEl = lists:nth( random:uniform(length(Rest)), Rest),
       {[RandomEl|Acc], lists:delete(RandomEl, Rest)}            
   end, {[],List}, List),                                        
   NewList.


verifyShuffle(State) when State#state.cardsLeft =< 0 -> true;
verifyShuffle(_) -> false.
