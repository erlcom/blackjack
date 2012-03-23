%%%-------------------------------------------------------------------
%%% @doc 
%%% Task4:The eventmanager
%%% @end
%%%-------------------------------------------------------------------
-module(event_manager).

-behaviour(gen_event).

%% API
-export([start_link/0, add_handler/1]).

-export([notify_player_enter/2]).
-export([notify_player_leave/2]).
-export([notify_game_start/0]).
-export([notify_game_end/0]).
-export([notify_card_dealt/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-record(state, {subscribers=[]}).

-type position() :: integer().
-type name() :: string().
-type suite() :: spades | hearts | clubs | diamonds.
-type value() :: 2..10 | jack | queen | king | ace.
-type card() :: {suite(), value()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec notify_player_enter(name(), position()) -> ok.
notify_player_enter(Name, Position) ->
    gen_event:notify(?MODULE, {player_enter,Name,Position}),
    ok.

-spec notify_player_leave(name(), position()) -> ok.
notify_player_leave(Name, Position) ->
    gen_event:notify(?MODULE, {player_leave,Name,Position}),
    ok.

-spec notify_game_start() -> ok.
notify_game_start() -> 
    gen_event:notify(?MODULE, {game_start}),
    ok.

-spec notify_game_end() -> ok.
notify_game_end() ->
    gen_event:notify(?MODULE, {game_end}),
    ok.

-spec notify_card_dealt(name(), position(), card()) -> ok.
notify_card_dealt(Player,Position,Card) -> 
    gen_event:notify(?MODULE, {card_dealt,Player,Position,Card}),
    ok.


%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates an event manager
%%
%% @spec start_link() -> {ok, Pid} | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%%--------------------------------------------------------------------
%% @doc
%% Adds an event handler
%%
%% @spec add_handler() -> ok | {'EXIT', Reason} | term()
%% @end
%%--------------------------------------------------------------------
add_handler(Name) ->
    gen_event:add_handler(?MODULE, Name, []).

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({player_enter,Name,Position}, State) ->
    io:format("Player ~w entered at position ~w. ~n", [Name,Position]),
    {ok, State};   
handle_event({player_leave,Name,Position}, State) ->
    io:format("Player ~w left position ~w. ~n", [Name,Position]),
    {ok, State};
handle_event({game_start}, State) ->
    io:format("Game started. ~n"),
    {ok, State};
handle_event({game_end}, State) ->
    io:format("Game ended. ~n"),
    {ok, State};
handle_event({card_dealt,Player,Position,Card}, State) ->
    io:format("Player ~w at position ~w received card ~w. ~n",[Player,Position,Card]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
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
