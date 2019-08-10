%%%-------------------------------------------------------------------
%%% @author epsilon
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Jul 2019 4:22 PM
%%%-------------------------------------------------------------------
-module(snake_worker).
-author("epsilon").

-behaviour(gen_server).

-include("../include/snake.hrl").


%% API
-export([start_link/6,stop/2,call/2,cast/2,start/8,start/7]).


%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
    {
      id,
      last_snake = 0,
      snakes = [],
      food = [],
      manager,
      corners
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

start({Node,ID},MinX,MaxX,MinY,MaxY,NumOfSnakes,Food, Manager) ->
  spawn(Node,
    fun() ->
    spawn(Node, ?MODULE, start, [ID,MinX,MaxX,MinY,MaxY,NumOfSnakes,Food, Manager])
    end
  );


start(ID,MinX,MaxX,MinY,MaxY,NumOfSnakes,Food, Manager) ->
  gen_server:start({local, ID}, ?MODULE, [ID,MinX,MaxX,MinY,MaxY,NumOfSnakes,Food, Manager], []).

start({Node,ID},MinX,MaxX,MinY,MaxY,{Snakes,Food}, Manager) ->
  spawn(Node,
    fun() ->
      spawn(Node, ?MODULE, start, [ID,MinX,MaxX,MinY,MaxY,{Snakes,Food}, Manager])
    end
  );

start(ID,MinX,MaxX,MinY,MaxY,{Snakes,Food}, Manager) ->
  {ok, _Pid} = gen_server:start({local, ID}, ?MODULE, [ID,MinX,MaxX,MinY,MaxY,{Food}, Manager], []),
  lists:foreach(
    fun(S) -> cast(Manager,{move_snake,S}) end,
    Snakes
  ).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ID::term(),MinX::integer(),MaxX::integer(),MinY::integer(),MaxY::integer(), Num::integer()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).

start_link(ID,MinX,MaxX,MinY,MaxY,NumOfSnakes) ->
  gen_server:start_link({local, ID}, ?MODULE, [ID,MinX,MaxX,MinY,MaxY,NumOfSnakes], []).

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

init([ID,MinX,MaxX,MinY,MaxY,{Food},Manager]) ->
  process_flag(trap_exit,true),
  {ok, #state{id = ID, last_snake = 0 , snakes = [], corners = {{MinX,MaxX},{MinY,MaxY}},food = Food,manager = Manager}};

init([ID,MinX,MaxX,MinY,MaxY,NumOfSnakes,FoodCount,Manager]) ->
  process_flag(trap_exit,true),
  SizeX = MaxX - MinX - 2,
  SizeY = MaxY - MinY - 2,
  Food = lists:map(fun({X,Y}) -> {MinX+X+1,MinY+Y+1} end,
        generateSnakes(FoodCount,SizeX,SizeY,[],[])),
  ID_List = lists:seq(1,NumOfSnakes),
  SnakeSpawnList = generateSnakesInit(NumOfSnakes,SizeX,SizeY,MinX,MinY, Food),
  Snakes = spawnSnake(atom_to_list(ID),combine(ID_List,SnakeSpawnList)),
  {ok, #state{id = ID, last_snake = NumOfSnakes, snakes = Snakes, corners = {{MinX,MaxX},{MinY,MaxY}},food = Food,manager = Manager}}.

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


handle_call(get_data, _From, State = #state{snakes = Snakes, food = Food}) ->
  Reply_tmp = lists:map(
    fun(S) ->
       B = whereis(S),
      case B of
        undefined -> [];
        _ -> snake_node:call(S,get_snake)
      end
    end,
    Snakes
  ),
  Reply = [X || X <- Reply_tmp, X =/= []],
  {reply, [{snakes,Reply},{food,Food}], State};

handle_call({timestep,Food}, _From, State = #state{id = _ID,snakes = Snakes, corners = Corners, manager = Manager}) ->
  %ListOut = lists:map(
   % fun(X) ->
   %   call(X,get_snake)
   % end,
   % Snakes
  %),

  lists:foreach(
    fun(S) ->
      B = whereis(S),
      case B of
        undefined -> ok;
        _ ->
          List = [S | snake_node:call(S,get_head)],
          calcMove(List,Food),
          B2 = whereis(S),
          case B2 of
            undefined -> ok;
            _->
              snake_node:call(S,move),
              OOB = outOfBounds(lists:nth(2,List),Corners),
              if
                OOB ->
                  try snake_node:call(S,get_snake) of
                    Snake ->
                      snake_node:stop(S, moved_to_a_different_node),
                      snake_worker:cast(self(),{remove_snake,S}),
                      snake_local:cast(Manager, {move_snake,Snake})
                  catch
                    _:_ -> ok
                  end;
                  %Snake = snake_node:call(S,get_snake),
                  %snake_local:cast(Manager, {move_snake,Snake});
                true -> ok
              end
          end
      end
    end,
    Snakes),


  {reply, {timestep,self()}, State};





handle_call(get_corners, _From, State = #state{corners = Corners}) ->
  {reply, Corners, State};

handle_call({set_corners,{MinX,MaxX,MinY,MaxY}}, _From, State ) ->
  {reply, new_corners, State#state{corners = {{MinX,MaxX},{MinY,MaxY}}}};



%%%debug calls
handle_call(get_state, _From, State ) ->
  {reply,State, State};

handle_call(get_snakes, _From, State = #state{snakes = Snakes}) ->
  List = lists:map(
    fun(X) ->
      case whereis(X) of
        undefined -> [];
        _ -> call(X,get_snake)
      end
    end,
    Snakes
  ),
  {reply,List, State};

handle_call(get_food, _From, State = #state{food = Snakes}) ->
  {reply,Snakes, State};

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

handle_cast({food,Food}, State) ->
  {noreply, State#state{food = Food}};

handle_cast({remove_food,Loc}, State = #state{food = Food, corners = Corners}) ->
  NewFood = [X || X <- Food, X =/= Loc],
  F = spawnFood(Corners,NewFood),
  {noreply, State#state{food = [F | NewFood]}};

handle_cast({remove_snake,SnakeID},State = #state{snakes = Snakes}) ->
  {noreply, State#state{snakes = [S || S <- Snakes, S =/= SnakeID]}};

handle_cast({add_snake,[{{ID,_},Dir},H |T]},State = #state{last_snake = Last, id = Node, snakes = Snakes, corners = Corners}) ->
  OOB = outOfBounds(H,Corners),
  if
    OOB  -> {noreply,State} ;
    true ->
      Name = ID,
      %Name = list_to_atom(atom_to_list(Node) ++ "_snake" ++ integer_to_list(Last+1)),
      {ok,_P} = snake_node:start(Name,head,H,Dir,Node),
      snake_node:cast(Name,{restore,T}),
      {noreply, State#state{snakes = [Name | Snakes], last_snake = Last + 1}}
  end;

handle_cast(new_snake,State = #state{last_snake = Last, id = Node, snakes = Snakes, corners = {{MinX,MaxX},{MinY,MaxY}},food = Food}) ->
  [{Xrel,Yrel}] = generateSnakes(1,MaxX-MinX-1,MaxY-MinY-1,[], Food),
  [Dir] = generateDirection(1),
  spawnSnake(I = atom_to_list(Node) ++ "New",[{Last+1, {{MinX + Xrel,MinY + Yrel},Dir}}]),
  Name = list_to_atom(I ++"_snake" ++ integer_to_list(Last + 1)),
  {noreply, State#state{snakes = [Name | Snakes], last_snake = Last + 1}};



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

terminate(new, #state{snakes = Snakes}) ->
  lists:foreach(
    fun(S) ->
      T = whereis(S),
      case T of
         undefined -> ok;
        _ ->  stop(S,new)
      end
    end,
    Snakes
  ),
  ok;

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
outOfBounds({X,Y}, {{MinX,MaxX},{MinY,MaxY}}) ->
  (X > MaxX) or (X < MinX) or ( Y > MaxY ) or  (Y < MinY).


spawnSnake(I,L) -> spawnSnake(I,L,[]).
spawnSnake(_I, [], Acc) -> lists:reverse(Acc);
spawnSnake(I,[{ID, {Loc,Dir}} | T ], Acc) ->
  Name = list_to_atom(I ++"_snake" ++ integer_to_list(ID)),
  snake_node:start(Name,head,Loc,Dir,self()),
  spawnSnake(I,T, [Name | Acc]).

spawnFood({{MinX,MaxX},{MinY,MaxY}},Unavailable) ->
  L = {MinX + 1 +  rand:uniform(MaxX-MinX-2),MinY +1+ rand:uniform(MaxY-MinY-2)},
  T = lists:member(L,Unavailable),
  if
    T -> spawnFood({{MinX,MaxX},{MinY,MaxY}},Unavailable);
    true -> L
  end.


%generate N random snakes (location & direction) in the range {[MinX:MinX+SizeX],[MinY:MinY+SizeY]}
generateSnakesInit(NumOfSnakes,SizeX,SizeY, MinX,MinY,Food) ->
  SnakeList_tmp = generateSnakes(NumOfSnakes,SizeX,SizeY,[],Food),
  SnakeList = lists:map(
    fun({X,Y}) -> {X + MinX, Y + MinY} end,
    SnakeList_tmp
  ),
  DirList = generateDirection(NumOfSnakes),
  combine(SnakeList,DirList).


%generates N random different locations
generateSnakes(0,_SizeX,_SizeY,Acc, _) -> Acc;
generateSnakes(NumOfSnakes,SizeX,SizeY,Acc, Unavailable) ->
  {X,Y} = generateSnake(SizeX,SizeY),
  Dup = lists:member({X,Y}, Acc) or lists:member({X,Y}, Unavailable),
  if
    Dup -> generateSnakes(NumOfSnakes,SizeX,SizeY,Acc,Unavailable);
    true -> generateSnakes(NumOfSnakes - 1,SizeX,SizeY,[{X,Y} |Acc],Unavailable)
  end.

%generates N random directions
generateDirection(0) -> [];
generateDirection(Num) ->
  Dir = 313 + rand:uniform(4), %result between 314 and 317
  [Dir | generateDirection(Num-1)].

%generate a random location
generateSnake(SizeX,SizeY) ->
  Xpos = rand:uniform(SizeX),
  Ypos = rand:uniform(SizeY),
  {Xpos,Ypos}.



combine([],[]) -> [];
combine([H1 | T1],[H2 | T2]) ->
  [{H1,H2} | combine(T1,T2)].

calcMove([ID,{X,Y},Dir],Food) ->
  Dist = lists:map(
    fun(F = {X2,Y2}) ->
      D = math:sqrt(math:pow(X2-X,2) + math:pow(Y2-Y,2)),
      {D,F}
    end,
    Food
  ),
  {D,Target} = hd(lists:keysort(1,Dist)),
  R = rand:uniform(4),
  NewDir = getDir(R,{X,Y},Target),
  if
    D == 0 -> snake_node:cast(ID,grow), snake_worker:cast(self(),{remove_food,Target});
    Dir =:= NewDir-> ok;
    true ->
      case snake_node:call(ID,{change_dir,NewDir}) of
        {cd,NewDir} -> ok;
        {forbidden_cd,NewDir} -> snake_node:call(ID,{change_dir,getDir(NewDir,{X,Y},Target)}), ok;
        bad_direction -> ok
      end
  end.



getDir(1,{X,_Y},{Xfood,_Yfood}) when Xfood > X-> ?RIGHT;
getDir(1,{X,_Y},{Xfood,_Yfood}) when Xfood < X-> ?LEFT;
getDir(1,{_X,Y},{_Xfood,Yfood}) when Yfood > Y-> ?UP;
getDir(1,{_X,Y},{_Xfood,Yfood}) when Yfood < Y-> ?DOWN;
getDir(1,_,_) -> 100;


getDir(2,{_X,Y},{_Xfood,Yfood}) when Yfood > Y-> ?UP;
getDir(2,{_X,Y},{_Xfood,Yfood}) when Yfood < Y-> ?DOWN;
getDir(2,{X,_Y},{Xfood,_Yfood}) when Xfood > X-> ?RIGHT;
getDir(2,{X,_Y},{Xfood,_Yfood}) when Xfood < X-> ?LEFT;
getDir(2,_,_) -> 100;

getDir(3,{X,_Y},{Xfood,_Yfood}) when Xfood < X-> ?LEFT;
getDir(3,{_X,Y},{_Xfood,Yfood}) when Yfood > Y-> ?UP;
getDir(3,{X,_Y},{Xfood,_Yfood}) when Xfood > X-> ?RIGHT;
getDir(3,{_X,Y},{_Xfood,Yfood}) when Yfood < Y-> ?DOWN;
getDir(3,_,_) -> 100;

getDir(4,{_X,Y},{_Xfood,Yfood}) when Yfood < Y-> ?DOWN;
getDir(4,{X,_Y},{Xfood,_Yfood}) when Xfood < X-> ?LEFT;
getDir(4,{_X,Y},{_Xfood,Yfood}) when Yfood > Y-> ?UP;
getDir(4,{X,_Y},{Xfood,_Yfood}) when Xfood > X-> ?RIGHT;
getDir(4,_,_) -> 100;


getDir(?UP,{X,_Y},{Xfood,_Yfood}) when Xfood >= X-> ?RIGHT;
getDir(?UP,{X,_Y},{Xfood,_Yfood}) when Xfood =< X-> ?LEFT;
getDir(?DOWN,{X,_Y},{Xfood,_Yfood}) when Xfood >= X-> ?RIGHT;
getDir(?DOWN,{X,_Y},{Xfood,_Yfood}) when Xfood =< X-> ?LEFT;
getDir(?RIGHT,{_X,Y},{_Xfood,Yfood}) when Yfood >= Y-> ?UP;
getDir(?RIGHT,{_X,Y},{_Xfood,Yfood}) when Yfood =< Y-> ?DOWN;
getDir(?LEFT,{_X,Y},{_Xfood,Yfood}) when Yfood >= Y-> ?UP;
getDir(?LEFT,{_X,Y},{_Xfood,Yfood}) when Yfood =< Y-> ?DOWN.
