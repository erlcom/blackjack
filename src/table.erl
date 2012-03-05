%%%-------------------------------------------------------------------
%%% @doc 
%%% Task3:The table
%%% @end
%%%-------------------------------------------------------------------
-module(table).
-behaviour(gen_server).

%% API
-export([start/0]).
-export([stop/0]).
-export([enter_table/1]).
-export([enter_table/2]).
-export([leave_position/1]).
-export([leave_table/0]).
-export([get_vacant_positions/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(NRPOSITIONSATTABLE, 4). 

-record(player, {name=[], from=0}).
-record(state, {players=[]}).

-type position() :: integer().
-type name() :: string().
-type id() :: integer().
-type enter_table_error_reason() :: {position_taken, position()} | {banned, name()} | {invalid_position, position()}. 
-type leave_table_error_reason() :: {position_not_taken, position()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> {ok, pid()}.
start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, terminate).

-spec enter_table(name()) -> {ok, id()} | {error, enter_table_error_reason()}.
enter_table(Name) -> 
    gen_server:call(?MODULE, {enter_table, Name}).

-spec enter_table(name(), position()) -> {ok, id()} | {error, enter_table_error_reason()}.
enter_table(Name, Position) -> 
    gen_server:call(?MODULE, {enter_table, Name, Position}).

-spec leave_position(position()) -> ok | {error, leave_table_error_reason()}.
leave_position(Position) ->
    gen_server:call(?MODULE, {leave_position, Position}).

-spec leave_table() -> ok.
leave_table() -> 
    gen_server:call(?MODULE, {leave_table}).

-spec get_vacant_positions() -> {ok, [position()]}.
get_vacant_positions() -> 
    gen_server:call(?MODULE, {get_vacant_positions}).

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
    {ok, #state{players=orddict:new()}}.

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
handle_call({enter_table, Name}, From, State) ->
    VacantPositions = get_vacant_position_list(State),
    add_player(Name,hd(VacantPositions),From,State);

handle_call({enter_table, Name, Position}, From, State) ->
    add_player(Name,Position,From,State);    

handle_call({get_vacant_positions}, _From, State) ->
    {reply, {ok, get_vacant_position_list(State)}, State};

handle_call({leave_position, Position}, {FromPid,_FromRef}, State) ->
    {Result, NewState} = leave_position(FromPid,State,Position),
    {reply, Result, NewState};

handle_call({leave_table}, {FromPid,_FromRef}, State) ->
    NewState = leave_table(FromPid,State,?NRPOSITIONSATTABLE),
    {reply, ok, NewState};

handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

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

handle_info({'DOWN', _MonitorReference, process, Pid, _Reason}, State) ->
    NewState = leave_table(Pid,State,?NRPOSITIONSATTABLE),
    {noreply, NewState};
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

add_player(Name,Position,From,State) ->
    {Result, NewState} = add_player_if_valid_position(Name,Position,From,State),
    case Result of
	ok -> 
	    {PlayerPid,_Ref} = From,
	    erlang:monitor(process, PlayerPid),
	    {reply, {ok, Position}, NewState};
	position_taken -> {reply, {position_taken, Position}, State};
	invalid_position -> {reply, {invalid_position, Position}, State}
    end.    

add_player_if_valid_position(_Name, Position,_From, State) when Position > ?NRPOSITIONSATTABLE->
   {invalid_position,State};
add_player_if_valid_position(Name,Position,{From,_Ref},State) ->
    case orddict:is_key(Position,State#state.players) of
	true -> 
	      {position_taken,State};
	false -> 
	     NewOrddict = orddict:store(Position,#player{name=Name,from=From},State#state.players),
	     {ok,State#state{players=NewOrddict}}
    end.


leave_position(Pid,State,Position) ->
    case orddict:is_key(Position,State#state.players) of
	true -> 
	    Player = orddict:fetch(Position,State#state.players),
	    NewState = verify_pid_of_player(Pid,Player,State,Position),
   	    {ok, NewState};
	false -> 
	    {position_not_taken,State}
    end.


leave_table(Pid,State,Position) when Position > 0  ->
    case orddict:is_key(Position,State#state.players) of
	true -> 
	    Player = orddict:fetch(Position,State#state.players),
	    NewState = verify_pid_of_player(Pid,Player,State,Position),
	    leave_table(Pid,NewState,Position-1);
	false -> 
	    leave_table(Pid,State,Position-1)
    end;
leave_table(_From,State,0) ->
    State.
    
verify_pid_of_player(Pid,Player,State,Position) when Player#player.from == Pid ->
    State#state{players=orddict:erase(Position,State#state.players)};
verify_pid_of_player(_Pid,_Player,State,_Position) -> 
    State.

get_vacant_position_list(State) ->
    OccupiedKeys = orddict:fetch_keys(State#state.players),
    List = lists:seq(1,?NRPOSITIONSATTABLE),
    %Non-Intersection of List and OccupiedKeys
    [I || I <- List, not(lists:member(I,OccupiedKeys))].
