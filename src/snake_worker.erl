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

-compile(export_all).

%% API
-export([start_link/6,stop/2,call/2,cast/2,start/8]).


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

start(ID,MinX,MaxX,MinY,MaxY,NumOfSnakes,Food, Manager) ->
  gen_server:start({local, ID}, ?MODULE, [ID,MinX,MaxX,MinY,MaxY,NumOfSnakes,Food, Manager], []).
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
init([ID,MinX,MaxX,MinY,MaxY,NumOfSnakes,Food,Manager]) ->
  SizeX = MaxX - MinX,
  SizeY = MaxY - MinY,
  ID_List = lists:seq(1,NumOfSnakes),
  SnakeSpawnList = generateSnakesInit(NumOfSnakes,SizeX,SizeY,MinX,MinY,lists:map(fun({X,Y}) -> {MinX+X,MinY+Y} end,Food)),
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


handle_call(get_data, _From, State = #state{snakes = Snakes, food = Food, corners = Corners, manager = Manager}) ->
  Reply = lists:map(
    fun(S) ->
      Snake = snake_node:call(S,get_snake),
      OOB = outOfBounds(lists:nth(2,Snake),Corners),
      if
        OOB ->
          snake_local:cast(Manager, {move_snake,S}),
          snake_worker:call(self(),{remove_snakes,[S]}), % TODO implement self call
          snake_node:stop(S, moved_to_a_different_node);
        true -> ok
      end,
      Snake
    end,
    Snakes
  ),%% TODO need to remove moved, snakes from state
  {reply, [{snakes,Reply},{food,Food}], State};

handle_call(get_corners, _From, State = #state{corners = Corners}) ->
  {reply, Corners, State};

handle_call({set_corners,{{MinX,MaxX},{MinY,MaxY}}}, _From, State ) ->
  {reply, new_corners, State#state{corners = {{MinX,MaxX},{MinY,MaxY}}}};

handle_call({add_snake,ID,Dir,H,T},_From, State = #state{last_snake = Last, snakes = Snakes}) ->
  snake_node:start(ID,head,H,Dir),
  snake_node:call(ID,{restore,T}),
  {reply, added_snake, State#state{snakes = [[{ID,Dir},H | T]|Snakes]}};

handle_call({add_snakes,ListOfSnakes}, _From, State = #state{last_snake = Last, snakes = Snakes}) ->
  %each snake in the list is a list of locations when hd(S) is the head
  Range = lists:seq(Last, Last + size(ListOfSnakes)),
  NewSnakes = lists:map(
    fun(S) ->
     addSnakes(S)
    end,
    combine(Range,ListOfSnakes)
  ),
  {reply, added_snake, State#state{snakes = Snakes ++ NewSnakes}};



%%%debug calls
handle_call(get_snakes, _From, State = #state{snakes = Snakes}) ->
  {reply,Snakes, State};

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
outOfBounds({X,Y}, {{MinX,MaxX},{MinY,MaxY}}) ->
  (X > MaxX) or (X < MinX) or ( Y > MaxY ) or  (Y < MinY).


spawnSnake(I,L) -> spawnSnake(I,L,[]).
spawnSnake(_I, [], Acc) -> lists:reverse(Acc);
spawnSnake(I,[{ID, {Loc,Dir}} | T ], Acc) ->
  Name = list_to_atom(I ++"_snake" ++ integer_to_list(ID)),
  snake_node:start(Name,head,Loc,Dir),
  spawnSnake(I,T, [Name | Acc]).

spawnFood({{MinX,MaxX},{MinY,MaxY}},Unavailable) ->
  L = {MinX + rand:uniform(MaxX-MinX),MinY + rand:uniform(MaxY-MinY)},
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

%%add snakes in case of fall
addSnakes({ID,_LocationList = [H]}) -> %only head - random direction
  Name = list_to_atom("snake" ++ integer_to_list(ID)),
  Dir = 313 + rand:uniform(4), %result between 314 and 317
  snake_node:start(Name,head,H,Dir),
  Name;

addSnakes({ID,_LocationList = [Head,Link | T]}) ->
  Name = list_to_atom("snake" ++ integer_to_list(ID)),
  Dir = getDir(Head,Link),
  _s = snake_node:start(Name,head,Head,Dir),
  snake_node:call(Name, {restore,[Link | T]}),
  Name.

getDir({X1,Y1},{X2,Y2}) when ((X1+1 =:= X2) and (Y1 =:= Y2))-> ?LEFT;
getDir({X1,Y1},{X2,Y2}) when ((X1-1 =:= X2) and (Y1 =:=Y2))-> ?RIGHT;
getDir({X1,Y1},{X2,Y2}) when ((X1 =:= X2) and (Y1-1 =:=Y2))-> ?UP;
getDir({X1,Y1},{X2,Y2}) when ((X1 =:= X2) and (Y1+1 =:=Y2))-> ?DOWN.

combine([],[]) -> [];
combine([H1 | T1],[H2 | T2]) ->
  [{H1,H2} | combine(T1,T2)].