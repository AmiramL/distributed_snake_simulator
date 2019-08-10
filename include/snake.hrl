%%%-------------------------------------------------------------------
%%% @author epsilon
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 22. Jul 2019 5:08 PM
%%%-------------------------------------------------------------------
-author("epsilon").




%% wxKeyCode
-define(DOWN,317).
-define(UP,315).
-define(RIGHT,316).
-define(LEFT,314).
-define(SPACEBAR,32).

-define(TIME_INTERVAL,100).

-define(ServerA,'a@epsilon-LIFEBOOK-UH552').
-define(ServerB,'b@epsilon-LIFEBOOK-UH552').
-define(ServerC,'c@epsilon-LIFEBOOK-UH552').
-define(ServerD,'d@epsilon-LIFEBOOK-UH552').



-record(snake, {
  id = [],
  role = link,
  next = none,
  direction,
  location,
  move = {}
  }).
