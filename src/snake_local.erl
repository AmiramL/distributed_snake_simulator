%%%-------------------------------------------------------------------
%%% @author epsilon
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Jul 2019 11:54 AM
%%%-------------------------------------------------------------------
-module(snake_local).
-author("epsilon").

-behaviour(gen_server).

-include("../include/snake.hrl").

%% API
-export([start_link/0,start/4,call/2,stop/2,cast/2]).

-compile(export_all).
%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  game_state,
  worker_node,
  corners
}).

%%%===================================================================
%%% API
%%%===================================================================

start_remote(MaxX,MaxY,NumOfSnakes,FoodCount,NodeA,NodeB,NodeC,NodeD) ->
  X = MaxX div 2,
  Y = MaxY div 2,
  Food = FoodCount div 4,
  {ok,_Manager} = gen_server:start({local,local_game}, ?MODULE, [0,MaxX,0,MaxY, [{workerA,NodeA},{workerB,NodeB},{workerC,NodeC},{workerD,NodeD}]], []),
  snake_worker:start({NodeA,workerA}, 0,X,0,Y,NumOfSnakes,Food,{local_game, node()}),
  snake_worker:start({NodeB,workerB}, X+1,MaxX,0,Y,NumOfSnakes,Food,{local_game, node()}),
  snake_worker:start({NodeC,workerC}, 0,X,Y+1,MaxY,NumOfSnakes,Food,{local_game, node()}),
  snake_worker:start({NodeD,workerD}, X+1,MaxX,Y+1,MaxY,NumOfSnakes,Food,{local_game, node()}).

start(MaxX,MaxY,NumOfSnakes,FoodCount) ->
  X = MaxX div 2,
  Y = MaxY div 2,
  Food = FoodCount div 4,
  {ok,Manager} = gen_server:start({local,local_game}, ?MODULE, [0,MaxX,0,MaxY], []),
  snake_worker:start(workerA, 0,X,0,Y,NumOfSnakes,Food,Manager),
  snake_worker:start(workerB, X+1,MaxX,0,Y,NumOfSnakes,Food,Manager),
  snake_worker:start(workerC, 0,X,Y+1,MaxY,NumOfSnakes,Food,Manager),
  snake_worker:start(workerD, X+1,MaxX,Y+1,MaxY,NumOfSnakes,Food,Manager).
  %gen_server:start({local,local_game}, ?MODULE, [0,MaxX,0,MaxY], []).


call(ID, Msg) ->
  gen_server:call(ID, Msg,infinity).

stop(ID, Reason) ->
  gen_server:stop(ID, Reason,infinity).

cast(ID, Msg) ->
  gen_server:cast(ID, Msg).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([MinX,MaxX,MinY,MaxY,Nodes]) ->
  process_flag(trap_exit,true),
  {ok, #state{game_state = get_data,worker_node = Nodes, corners = {{MinX,MaxX},{MinY,MaxY}}}};
init([MinX,MaxX,MinY,MaxY]) ->
  process_flag(trap_exit,true),
  Nodes = [workerA,workerB,workerC,workerD],
  {ok, #state{game_state = get_data,worker_node = Nodes, corners = {{MinX,MaxX},{MinY,MaxY}}}}.

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

handle_call(timestep, _From, State = #state{worker_node = Nodes}) ->
  Food = lists:flatten(lists:map(fun(W) -> snake_worker:call(W, get_food) end ,Nodes)),
  SnakeList = lists:foldr(
    fun(W, Acc) ->
      {timestep, S} = snake_worker:call(W,{timestep,Food}),
      S ++ Acc
    end
    ,[]
    ,Nodes),
  detectColissions(SnakeList),
  {reply, SnakeList , State};

handle_call(get_data, _From, State = #state{worker_node = Nodes}) ->
  {reply,
    lists:map(
      fun(X) ->
        [{snakes,Snakes},{food,Food}] = snake_worker:call(X,get_data),
        {X,{snakes,Snakes},{food,Food}}
      end,
    Nodes),
    State};

handle_call(worker, _From, State = #state{worker_node = Node}) ->
  {reply, Node, State};

handle_call({move_snake,[{ID,Dir},H | T]}, _From, State) ->
  Node = findNode(H),
  snake_worker:call(Node,{add_snake,ID,Dir,H,T}),
  {reply, ok, State};

handle_call(get_corners, _From, State = #state{corners = Corners}) ->
  {reply, Corners, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

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

handle_cast({move_snake,Snake}, State = #state{worker_node = Nodes, corners = Corners}) ->
  Head = lists:nth(2,Snake),
  OOB = outOfBounds(Head,Corners),
  if
    OOB -> ok;
    true ->
      lists:foreach(
        fun(S) -> snake_worker:cast(S,{add_snake,Snake}) end,
        Nodes
      )

  end,
  {noreply, State};


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
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
headColissions([], Acc) ->Acc;
headColissions([{{ID,_Dir},H}|T], Acc) ->
  B = lists:keymember(H,2,T),
  if
    B ->
      {{ID2,_Dir2},H} = lists:keyfind(H,2,T),
      headColissions(T,[ID,ID2  | Acc]);
    true ->headColissions(T,Acc)
  end.


detectColissions(SnakeList) ->
  H = headColissions(lists:map(fun(S) -> list_to_tuple(lists:sublist(S,2)) end, SnakeList),[]),
  lists:foreach(
    fun(S) -> stop(S, colission) end,
    H
  ),
  HeadList = lists:map(
    fun(S) ->
      [{ID,_Dir},Head] = lists:sublist(S,2),
      {ID,Head}
    end
    , SnakeList
  ),
  lists:foreach(fun(X) -> castToAll(X,HeadList) end, HeadList),
  H.

castToAll({_ID,_Head},[]) -> ok;

castToAll({ID,Head},[{ID2,_Head2} | T]) ->
  cast(ID2,{cut_node_check,Head}),
  castToAll({ID,Head},T).




findNode(H) ->
  C = snake_worker:call(workerA,get_corners),
  OOB = outOfBounds(H,C),
  if
    OOB -> findNode(H,1);
    true -> workerA
  end.
findNode(H,1) ->
  C = snake_worker:call(workerB,get_corners),
  OOB = outOfBounds(H,C),
  if
    OOB -> findNode(H,2);
    true -> workerB
  end;
findNode(H,2) ->
  C = snake_worker:call(workerC,get_corners),
  OOB = outOfBounds(H,C),
  if
    OOB -> findNode(H,3);
    true -> workerC
  end;
findNode(H,3) ->
  C = snake_worker:call(workerD,get_corners),
  OOB = outOfBounds(H,C),
  if
    OOB -> something_is_wrong;
    true -> workerD
  end.


generateFood(SizeX,SizeY,MinX,MinY,FoodCount,Unavailable) ->
  FoodList_tmp = generateFood(FoodCount,SizeX,SizeY,Unavailable),
  FoodList = lists:map(
    fun({X,Y}) -> {X + MinX, Y + MinY} end,
    FoodList_tmp
  ),
  FoodList.

%generates N random different locations
generateFood(0,_SizeX,_SizeY,Acc) -> Acc;
generateFood(FoodCount,SizeX,SizeY,Acc) ->
  {X,Y} = generateFood(SizeX,SizeY),
  Dup = lists:member({X,Y}, Acc),
  if
    Dup -> generateFood(FoodCount,SizeX,SizeY,Acc);
    true -> generateFood(FoodCount - 1,SizeX,SizeY,[{X,Y} |Acc])
  end.

%generate a random location
generateFood(SizeX,SizeY) ->
  Xpos = rand:uniform(SizeX),
  Ypos = rand:uniform(SizeY),
  {Xpos,Ypos}.



outOfBounds({X,Y}, {{MinX,MaxX},{MinY,MaxY}}) ->
  (X > MaxX) or (X < MinX) or ( Y > MaxY ) or  (Y < MinY).





