%%%-------------------------------------------------------------------
%%% @author epsilon
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Aug 2019 11:10 AM
%%%-------------------------------------------------------------------
-module(snake_wx).
-author("epsilon").

-behaviour(wx_object).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/glu.hrl").

-include("../include/snake.hrl").

%% API
-export([start_link/0,start/0]).

%% gen_server callbacks
-export([init/1,
  handle_event/2,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).


-record(state,
  {
    manager = local_game,
    pause = true,

    frame,
		canvas,
    size,
		block_size = {20,20}
  }).


%%%===================================================================
%%% API
%%%===================================================================

start() ->
    wx_object:start_link(?MODULE, [], []).
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
%%% Pause
handle_event(#wx{event = #wxKey{type = key_down, keyCode = 32}},
	     State = #state{pause = false}) ->
  {noreply, State#state{pause = true}};

handle_event(#wx{event = #wxKey{type = key_down, keyCode = 32}},
    State = #state{pause = true}) ->
  erlang:send_after(?TIME_INTERVAL, self(), update),
  {noreply, State#state{pause = false}};

handle_event(#wx{obj = _Frame, event = #wxCommand{type = command_menu_selected},
		 id = Id}, State = #state{manager = Manager}) ->
    %%io:format("Command menu ID: ~p\n", [Id]),
    case Id of
      ?wxID_NEW ->

        snake_local:stop(local_game,new),
        receive
          after 500 -> ok
        end,
        _ = snake_local:start_remote(100,100,2,20,?ServerA,?ServerB,?ServerC,?ServerD),
        receive
        after 100 -> ok
        end,
        draw(State),
        erlang:send_after(?TIME_INTERVAL, self(), update),
        {noreply, State#state{pause = true}};
    	?wxID_ADD ->
        snake_local:cast(Manager,new_snake),
        {noreply, State};
    	?wxID_EXIT ->
        snake_local:stop(local_game,new),
	      {stop, shutdown, State};
	    _ ->
	      {noreply, State}
    end;



handle_event(_Event,State) ->
  {noreply, State}.


init([]) ->
  process_flag(trap_exit,true),
  wx:new(),
  Frame = wxFrame:new(wx:null(),?wxID_ANY,"Snake Simulator by Tom & Amiram", [{size, {750,750}}]),
  MB = wxMenuBar:new(),
  File = wxMenu:new([]),

  wxMenu:append(File, ?wxID_NEW, "&New Game"),
  wxMenu:appendSeparator(File),


  wxMenu:appendSeparator(File),
  wxMenu:append(File, ?wxID_EXIT, "&Quit"),

  Addsnake = wxMenu:new([]),
  wxMenu:append(Addsnake,?wxID_ADD, "Add Snake to the meyham!"),
  wxMenu:connect(Addsnake,command_menu_selected),

  Help = wxMenu:new([]),
  wxMenu:append(Help, ?wxID_HELP, "Help"),
  wxMenu:append(Help, ?wxID_ABOUT, "About"),

  wxMenuBar:append(MB, File, "&Game"),
  wxMenuBar:append(MB, Addsnake, "&Add Snake"),
  wxMenuBar:append(MB, Help, "&Help"),

  wxFrame:setMenuBar(Frame,MB),


  wxFrame:createStatusBar(Frame, [{number, 2}]),

  wxFrame:centreOnScreen(Frame),
  wxFrame:show(Frame),


  GLAttrib = [{attribList, [?WX_GL_RGBA,?WX_GL_DOUBLEBUFFER,0]}],
  Canvas = wxGLCanvas:new(Frame, GLAttrib),

  wxGLCanvas:update(Canvas),

  wxGLCanvas:setCurrent(Canvas),
  wxGLCanvas:setFocus(Canvas),

  init_gl(Canvas),

  wxFrame:connect(Canvas, key_down),
  wxFrame:connect(Frame, close_window, [{skip,true}]),
  wxFrame:connect(Frame, command_menu_selected),

  wxGLCanvas:connect(Canvas, paint),
  wxGLCanvas:connect(Canvas, size),


  wxGLCanvas:setSize(Canvas, 750,750),
  {W,H} = wxGLCanvas:getSize(Canvas),
  gl_resize(W,H),
  {MapWidth, MapHeight} = {100,100},
  BlockWidth	= W div MapWidth,
  BlockHeight = H div MapHeight,




  _ = snake_local:start_remote(MapWidth,MapHeight,2,20,?ServerA,?ServerB,?ServerC,?ServerD),

  receive
    after 100 -> ok
  end,

  State = #state{frame = Frame, canvas = Canvas,block_size = {BlockWidth,BlockHeight}, size = {100,100} },
  draw(State),
  erlang:send_after(?TIME_INTERVAL, self(), update),

  {Frame, State}.

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
handle_info({'_egl_error_',_,no_gl_context}, State) ->
    {stop, shutdown, State#state{}};
handle_info(update, State = #state{pause = P}) ->
  draw(State),
  if
    not P -> erlang:send_after(?TIME_INTERVAL, self(), update);
    true -> ok
  end,
  {noreply, State};
handle_info(Msg, State) ->
    io:format("Unhandled message: ~p\n", [Msg]),
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

%% This initializes the canvas making the coordinate system
%% to have its {0,0} coord in the upper left corner and
%% enables 2D texture and some other stuff
init_gl(Canvas) ->
    {W,H} = wxWindow:getClientSize(Canvas),
    io:format("ClientSize: ~p\n", [{W,H}]),
    gl:clearColor(0.3,1,0.3,0.4),
    gl:enable(?GL_TEXTURE_2D),
    gl:enable(?GL_COLOR_MATERIAL),
    gl:enable(?GL_BLEND),
    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA,?GL_ONE_MINUS_SRC_ALPHA),
    gl_resize(W,H).


%% Resets the ortho view to the given size
gl_resize(W,H) ->
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),
    glu:ortho2D(0, W,H, 0),
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity(),
    gl:clear(?GL_COLOR_BUFFER_BIT),
    ok.

%%%===================================================================
%%% Draw functions
%%%===================================================================

draw(State=#state{manager = Manager,size = _Size , block_size = BlockSize}) ->
    {SnakeList,Food} = snake_local:call(Manager,timestep),
    gl:clear(?GL_COLOR_BUFFER_BIT),
    draw_map(BlockSize, Food),
    lists:foreach(
      fun([_ | [H | T ]]) ->
        draw_snake(T,BlockSize),
        draw_snake_head([H],BlockSize)
      end,
      SnakeList
    ),
     %draw_grid(Size, BlockSize),
    wxGLCanvas:swapBuffers(State#state.canvas).

draw_map({Width, Height}, Food) ->
    FunMap = fun({X,Y}) ->
		     snake_graphics:rectangle(X*Width,Y*Height,Width,Height)
	     end,
    gl:'begin'(?GL_QUADS),
    gl:color4ub(255,51,51,200),
    wx:foreach(FunMap, Food),
    gl:'end'(),
    ok.

draw_snake( [], _ ) ->ok;

draw_snake( T, {Width,Height}) ->
    gl:color4ub(127,0,255,200),
    Fun = fun({X,Y}) ->
		     snake_graphics:rectangle(X*Width,Y*Height,Width,Height)
	  end,
    gl:'begin'(?GL_QUADS),
    wx:foreach(Fun,T),
    gl:'end'(),

    ok.

draw_snake_head(H , {Width,Height}) ->
  gl:color4ub(0,0,102,200),
  Fun =
    fun({X,Y}) ->
      snake_graphics:rectangle(X*Width,Y*Height,Width,Height)
    end,
  gl:'begin'(?GL_QUADS),
  wx:foreach(Fun,H),
  gl:'end'(),
  ok.


%draw_grid({MapWidth, MapHeight}, {Width, Height}) ->
%    gl:color4ub(0,0,0,50),

%    FunX = fun(PosX) -> snake_graphics:line({PosX*Width, 0}, {PosX*Width, MapHeight*Height}) end,
%    FunY = fun(PosY) -> snake_graphics:line({0, PosY*Height}, {MapWidth*Width, PosY*Height}) end,
%    gl:'begin'(?GL_LINES),
%    wx:foreach(FunX, lists:seq(0,MapWidth)),
%    wx:foreach(FunY, lists:seq(0,MapHeight)),
%    gl:'end'(),

%    ok.

