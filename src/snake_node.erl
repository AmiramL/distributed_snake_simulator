%%%-------------------------------------------------------------------
%%% @author epsilon
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. Jul 2019 8:42 PM
%%%-------------------------------------------------------------------
-module(snake_node).
-author("epsilon").

-behaviour(gen_server).

-include("../include/snake.hrl").

%-compile(export_all).

%% API
-export([start_link/5, start_link/4, start_link/2, start_link/3, start/5, start/4, call/2,cast/2, stop/2]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    id = [],
    role = link,
    next = none,
    direction,
    location,
    move = {},
    grow = false,
    target
    }).

%%%===================================================================
%%% API
%%%===================================================================
call(ID, Msg) ->
  gen_server:call(ID, Msg,infinity).

stop(ID, Reason) ->
  gen_server:stop(ID, Reason,infinity).

cast(ID, Msg) ->
  gen_server:cast(ID, Msg).

start(ID, Role, Location, Direction) ->
  gen_server:start({local, ID}, ?MODULE, [ID, Role,  Location, Direction], []).

start(ID, Role,  Location, Direction, Next) ->
  gen_server:start({local, ID}, ?MODULE, [ID, Role,  Location, Direction,Next], []).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ID::atom(), Role::term(), Location::term(), Direction::integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ID, Role, Location, Direction) ->
  gen_server:start_link({local, ID}, ?MODULE, [ID, Role, Location, Direction], []).
-spec(start_link(ID::atom(), Role::term(), Location::term(), Direction::integer(), Next::pid()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ID, Role, Location, Direction, Next) ->
  gen_server:start_link({local, ID}, ?MODULE, [ID, Role,Location, Direction,Next], []).


start_link(link, Location) ->
  gen_server:start_link(?MODULE, [link,Location], []).
start_link(link, Location,Next) ->
  gen_server:start_link(?MODULE, [link,Location,Next], []).


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
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([ID, head, Location, Direction]) ->
  Move = nextMove(Direction,Location),
  {ok, #state{id = ID, role = head, direction = Direction, location = Location, move = Move}};
init([ID, head, Location, Direction, Next]) ->
  Move = nextMove(Direction,Location),
  {ok, #state{id = ID, role = head, direction = Direction, next = Next, location = Location, move = Move}};
init([link, Location]) ->
  {ok, #state{role = link, location = Location}};
init([link, Location, Next]) ->
  {ok, #state{role = link, location = Location, next = Next}}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

%%change direction call (moves)
handle_call({change_dir,Dir}, _From, State) when ((Dir > 317) or (Dir < 314)) ->
  {reply, bad_direction, State};

handle_call({change_dir,Dir}, _From, State = #state{role = head,direction = Dir}) ->
  {reply, same_direction, State};

handle_call({change_dir,NewDir}, _From, State = #state{role = head,direction = Dir, location = Location})->
  Forbidden = forbiddenDirection(Dir),
  if
    NewDir =/= Forbidden->
      NextMove = nextMove(NewDir,Location),
      {reply, {cd,NewDir}, State#state{direction = NewDir, move = NextMove}};
    %  NextLocation = nextMove(NewDir,Location),
    %  NextMove = nextMove(NewDir,NextLocation),
    %  case Next of
    %    none -> ok;
    %    Next -> call(Next, {move,Location})
    %  end,
    %  {reply, {cd,NewDir}, State#state{direction = NewDir, location = NextLocation, move = NextMove}};
    true -> {reply, {forbidden_cd,NewDir}, State}
  end;

handle_call({change_dir,_NewDir}, _From, State = #state{role = link}) ->
  {reply, {cant_cd, link}, State};


%move and grow head
handle_call(move, _From, State = #state{role = head,direction = Dir, move = Move, next = none, location = Loc, grow = true}) ->
  NextMove = nextMove(Dir, Move),
  {ok,Next} = start_link(link, Loc),
  {reply, {head_moved,Loc}, State#state{next = Next, location = Move, move = NextMove, grow = false}};

%move lone head
handle_call(move, _From, State = #state{role = head,direction = Dir, move = Move, next = none}) ->
  NextMove = nextMove(Dir, Move),
  {reply, {head_moved,Move}, State#state{location = Move, move = NextMove}};

%head move
handle_call(move, _From, State = #state{role = head,direction = Dir, move = Move, next = Next, location = Loc}) ->
  NextMove = nextMove(Dir, Move),
  call(Next, {move,Loc}),
  {reply, {head_moved,Loc,Move}, State#state{location = Move, move = NextMove}};

%move and grow link
handle_call({move,NewLoc}, _From, State = #state{role = link, next = none,location = Loc, grow = true}) ->
  {ok,Next} = start_link(link, Loc),
  {reply, ok, State#state{location = NewLoc, next = Next, grow = false}};

%last link move
handle_call({move,NewLoc}, _From, State = #state{role = link, next = none}) ->
  {reply, ok,  State#state{location = NewLoc}};

%link move
handle_call({move,NewLoc}, _From, State = #state{role = link, next = Next,location = Loc}) ->
  call(Next, {move,Loc}),
  {reply, ok, State#state{location = NewLoc}};

%promotion calls
handle_call({promote,ID,Direction}, _From, State = #state{role = link, location = Loc}) ->
  register(ID, self()),
  {reply, {promoted,Loc}, State#state{role = head, id = ID, direction = Direction}};

handle_call({promote,ID}, _From, State = #state{role = link, location = Loc}) ->
  register(ID, self()),
  {reply, {promoted,Loc}, State#state{role = head, id = ID, direction = ?UP}};

handle_call({promote,_Id,_Direction}, _From, State = #state{role = head, id = ID}) ->
  {reply, {cant_promote_head,ID}, State};

%return location list
handle_call(get_snake, _From, State = #state{role = head, id = ID, direction = Dir, next = none,location = Loc}) ->
  {reply, [{ID,Dir},Loc], State};

handle_call(get_snake, _From, State = #state{role = head, id = ID, direction = Dir, next = Next,location = Loc}) ->
  Reply =  [{ID,Dir},Loc | call(Next, get_snake)],
  {reply, Reply, State};

handle_call(get_snake, _From, State = #state{next = none,location = Loc}) ->
  {reply, [Loc], State};

handle_call(get_snake, _From, State = #state{next = Next,location = Loc}) ->
  Reply =  [Loc | call(Next, get_snake)],
  {reply, Reply, State};

handle_call({get_node,Location}, _From, State = #state{location = Location}) ->
  {reply, self(), State};

handle_call({get_node,_Location}, _From, State = #state{next = none}) ->
  {reply, wrong_snake, State};

handle_call({get_node,_Location}, _From, State = #state{next = Next}) ->
  Reply = call(Next,{get_node,_Location}),
  {reply, Reply, State};

%collision calls
handle_call({collision,_Dir}, _From, State = #state{next = none}) ->
  {stop, head_collision, State};

handle_call({collision,_Dir}, _From, State = #state{role = head, next = Next}) ->
  stop(Next,head_collision),
  {stop, head_collision, State};


handle_call({collision,Dir}, _From, State = #state{role = link, next = Next}) ->
  call(Next,{promote,Dir}),
  {stop, {link_collision,promoted,Next}, State};

handle_call(get_direction, _From, State = #state{direction = Dir}) ->
  {reply, Dir, State};


handle_call({set_next,Next}, _From, State = #state{}) ->
  {reply, ok, State#state{next = Next}};
%restore calls
handle_call({restore,[N]}, _From, State) ->
  {ok,Next} = start_link(link, N),
  {reply, ok, State#state{next = Next}};

handle_call({restore,[N|T]}, _From, State) ->
  {ok,Next} = start_link(link, N),
  call(Next,{restore,T}),
  {reply, ok, State#state{next = Next}};

%%debug calls
handle_call(get_state, _From, State) ->
  {reply, State, State};
handle_call(get_state_snake, _From, State = #state{next = none}) ->
  {reply, [State], State};
handle_call(get_state_snake, _From, State = #state{next = Next}) ->
  Reply = [State | call(Next,get_state_snake)],
  {reply, Reply, State};




handle_call(_Request, _From, State) ->
  {reply, unknown_call, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

%%set grow boolean - will grow on next move
handle_cast(grow,  State = #state{next = none}) ->
  {noreply,  State#state{ grow = true}};

handle_cast(grow, State = #state{next = Next}) ->
  cast(Next,grow),
  {noreply,  State#state{ grow = false}};


handle_cast(_Request, State) ->
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
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
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
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).

terminate(head_collision, #state{next = none}) ->
  head_collision;

terminate(head_collision, #state{next = Next}) ->
  stop(Next,head_collision),
  head_collision;

terminate(_Reason, _State) ->
  unknown_reason_terminate.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
forbiddenDirection(?UP) -> ?DOWN;
forbiddenDirection(?DOWN) -> ?UP;
forbiddenDirection(?RIGHT) -> ?LEFT;
forbiddenDirection(?LEFT) -> ?RIGHT.

nextMove(Direction, {X,Y})->
  case Direction of
    ?UP ->    {X,Y+1};
    ?DOWN ->  {X,Y-1};
    ?LEFT ->  {X-1,Y};
    ?RIGHT -> {X+1,Y}
  end.

