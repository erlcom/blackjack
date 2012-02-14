 %%%-------------------------------------------------------------------
%%% @doc 
%%% The task for week 1 is to implement this module, deck.
%%% @end
%%%-------------------------------------------------------------------
-module(deck).

-behaviour(gen_server).

%% API
-export([start/0]).
-export([stop/0]).
-export([is_time_to_shuffle/0]).
-export([shuffle/0]).
-export([get_card/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {deck=[], cards_left=0}).

-define(NRDECKS, 4).
-define(SPLITPERCENTAGE, 0.8).

-type suite() :: spades | hearts | clubs | diamonds.
-type value() :: 2..10 | jack | queen | king | ace.
-type card() :: {suite(), value()}.

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
    gen_server:start_link({local,deckPid},?MODULE, [], []). 

-spec stop() -> ok.
stop() ->
    gen_server:call(deckPid, terminate).

-spec is_time_to_shuffle() -> boolean().
is_time_to_shuffle() ->
    gen_server:call(deckPid, is_it_time_to_shuffle).

-spec shuffle() -> {ok}.
shuffle() ->
    gen_server:call(deckPid, shuffle).

-spec get_card() -> card().
get_card() ->
    gen_server:call(deckPid, get_card).

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
    {ok, internal_shuffle()}.

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
handle_call(get_card, _From, State) ->    
    NewState = State#state{deck=tl(State#state.deck), cards_left=State#state.cards_left -1},
    {reply, hd(State#state.deck), NewState};

handle_call(is_it_time_to_shuffle, _From, State) ->
    {reply, State#state.cards_left =< 0, State};

handle_call(shuffle, _From, _State) ->
    {reply, ok, internal_shuffle()};

handle_call(terminate, _From, State) ->
    NewState = State#state{deck=[], cards_left=0},    
    {stop, normal, ok, NewState}.


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
    Deck = [{S,V} || S <- lists:seq(2,10) ++ [jack, queen, king, ace] , V <- [spades, hearts, clubs, diamonds]],
    DeckList = lists:flatten(lists:duplicate(?NRDECKS,Deck)),
    ShuffledDecks = random_list(DeckList),
    SplitNr = round(length(ShuffledDecks)*?SPLITPERCENTAGE),
    #state{deck=ShuffledDecks,cards_left=SplitNr}.
    

random_list(List) ->
   random:seed(now()),
   NewList = [X||{_,X} <- lists:sort([ {random:uniform(), N} || N <- List])],
   NewList.
