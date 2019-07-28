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
  corners,
  food
}).

%%%===================================================================
%%% API
%%%===================================================================
start(MaxX,MaxY,NumOfSnakes,FoodCount) ->
  X = MaxX div 2,
  Y = MaxY div 2,
  Food1 = generateFood(X,Y,0,0,FoodCount,[]),
  Food2 = generateFood(MaxX-X-1,Y,X+1,0,FoodCount,[]),
  Food3 = generateFood(X,MaxY-Y-1,0,Y+1,FoodCount,[]),
  Food4 = generateFood(MaxX-X-1,MaxY-Y-1,X+1,Y+1,FoodCount,[]),
  snake_worker:start(workerA, 0,X,0,Y,NumOfSnakes,Food1,self()),
  snake_worker:start(workerB, X+1,MaxX,0,Y,NumOfSnakes,Food2,self()),
  snake_worker:start(workerC, 0,X,Y+1,MaxY,NumOfSnakes,Food3,self()),
  snake_worker:start(workerD, X+1,MaxX,Y+1,MaxY,NumOfSnakes,Food4,self()),
  gen_server:start({local,local_game}, ?MODULE, [0,MaxX,0,MaxY, Food1 ++ Food2 ++ Food3 ++ Food4], []).


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
init([MinX,MaxX,MinY,MaxY, Food]) ->
  process_flag(trap_exit,true),
  Nodes = [workerA,workerB,workerC,workerD],
  {ok, #state{game_state = get_data,worker_node = Nodes, corners = {MinX,MaxX,MinY,MaxY},food = Food}}.

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
  handle_info(timeout,State),
  lists:foreach(fun(W) -> snake_worker:call(W, move) end ,Nodes),
  M = lists:map(fun(W) -> snake_worker:call(W, get_data) end ,Nodes),
  {reply, M , State};

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

handle_info(timeout, State = #state{game_state = get_data, worker_node = Nodes, food = Food}) ->
  lists:foreach(
    fun(Node) ->
      [{snakes,Snakes},{food,_F}] = snake_worker:call(Node,get_data),
      lists:foreach(fun(S) -> calcMove(S,Food,Node) end ,Snakes)
    end,
    Nodes
  ),
  {noreply, State};



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


calcMove([{ID,Dir} | Locations ],Food,Worker) ->
  {X,Y} = hd(Locations),
  Dist = lists:map(
    fun(F = {X2,Y2}) ->
      D = math:sqrt(math:pow(X2-X,2) + math:pow(Y2-Y,2)),
      {D,F}
    end,
    Food
  ),
  {D,Target} = hd(lists:keysort(1,Dist)),
  NewDir = getDir({X,Y},Target),
  if
    D == 0 -> snake_node:cast(ID,grow), snake_worker:cast(Worker,{remove_food,Target});
    Dir =:= NewDir-> ok;
    true ->
      case snake_node:call(ID,{change_dir,NewDir}) of
        {cd,NewDir} -> ok;
        {forbidden_cd,NewDir} -> snake_node:call(ID,{change_dir,getDir(NewDir,{X,Y},Target)}), ok;
        bad_direction -> ok
      end
  end,
  snake_node:call(ID,move).

getDir({X,_Y},{Xfood,_Yfood}) when Xfood > X-> ?RIGHT;
getDir({X,_Y},{Xfood,_Yfood}) when Xfood < X-> ?LEFT;
getDir({_X,Y},{_Xfood,Yfood}) when Yfood > Y-> ?UP;
getDir({_X,Y},{_Xfood,Yfood}) when Yfood < Y-> ?DOWN;
getDir(_,_) -> 100.

getDir(?UP,{X,_Y},{Xfood,_Yfood}) when Xfood >= X-> ?RIGHT;
getDir(?UP,{X,_Y},{Xfood,_Yfood}) when Xfood =< X-> ?LEFT;
getDir(?DOWN,{X,_Y},{Xfood,_Yfood}) when Xfood >= X-> ?RIGHT;
getDir(?DOWN,{X,_Y},{Xfood,_Yfood}) when Xfood =< X-> ?LEFT;
getDir(?RIGHT,{_X,Y},{_Xfood,Yfood}) when Yfood >= Y-> ?UP;
getDir(?RIGHT,{_X,Y},{_Xfood,Yfood}) when Yfood =< Y-> ?DOWN;
getDir(?LEFT,{_X,Y},{_Xfood,Yfood}) when Yfood >= Y-> ?UP;
getDir(?LEFT,{_X,Y},{_Xfood,Yfood}) when Yfood =< Y-> ?DOWN.


outOfBounds({X,Y}, {{MinX,MaxX},{MinY,MaxY}}) ->
  (X > MaxX) or (X < MinX) or ( Y > MaxY ) or  (Y < MinY).





