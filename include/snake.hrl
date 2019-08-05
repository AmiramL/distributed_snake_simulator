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

-define(TIME_INTERVAL,200).

-define(ServerA,'a@007-lnx-e2').
-define(ServerB,'b@007-lnx-f3').
-define(ServerC,'c@007-lnx-f2').
-define(ServerD,'d@007-lnx-e2').



-record(snake, {
  id = [],
  role = link,
  next = none,
  direction,
  location,
  move = {}
  }).
