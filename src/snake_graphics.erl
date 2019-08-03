%%%-------------------------------------------------------------------
%%% @author  <ollemattss@gmail.com>
%%% @copyright (C) 2013-2014, 
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by  <olle@zubat>
%%%-------------------------------------------------------------------
-module(snake_graphics).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-export([square/2,square/3,rotate/1,
	 line/2,
	 point/2,point/1,
	 scale/2,
	 translate/2,translate/1,
	 polygon/1, triangle/6,
	 quad/8,quad/4,rectangle/4,rectangle/3,
	 circle/4,circle/3]).

square({X,Y},L) ->
    square(X,Y,L).
square(X,Y,L) ->
    Coords = [{X,Y},{X+L,Y},{X+L,Y+L},{X,Y+L}],
    polygon(Coords).



triangle(X1, Y1, X2, Y2, X3, Y3) ->
    Coords = [{X1,Y1}, {X2,Y2}, {X3,Y3}],
    polygon(Coords).

line({X1,Y1}, {X2,Y2}) ->
    %% gl:disable(?GL_TEXTURE_2D),
    %% gl:'begin'(?GL_LINES),
    gl:vertex2f(X1, Y1),
    gl:vertex2f(X2, Y2).
    %% gl:'end'(),
    %% gl:enable(?GL_TEXTURE_2D).



point({X, Y}) ->
    point(X, Y).
point(X, Y) ->
    %%    gl:disable(?GL_TEXTURE_2D),
    %%    gl:'begin'(?GL_POINTS),
    gl:vertex2f(X, Y).
    %% gl:'end'(),
    %% gl:enable(?GL_TEXTURE_2D).

rotate(R) ->
    gl:rotatef(R, 0, 0, 1).

scale(X, Y) ->
    gl:scalef(X, Y, 1).

translate({X, Y}) ->
    translate(X, Y).
translate(X, Y) ->
    gl:translatef(X, Y, 0).

polygon(Coords) when is_list(Coords)->
    gl:disable(?GL_TEXTURE_2D),
    wx:foreach(fun({X,Y}) -> gl:vertex4d(X,Y,0,1) end, Coords),
    gl:enable(?GL_TEXTURE_2D).

quad({X1, Y1}, {X2, Y2}, {X3, Y3}, {X4, Y4} ) ->
    quad(X1, Y1, X2, Y2, X3, Y3, X4, Y4 ).
quad(X1, Y1, X2, Y2, X3, Y3, X4, Y4 ) ->
    Coords = [{X1,Y1}, {X2,Y2}, {X3,Y3}, {X4,Y4} ],
    polygon(Coords).

rectangle({X, Y}, W, H) ->
    rectangle( X, Y, W, H).
rectangle(X, Y, W, H) ->
    Coords = [{X,Y}, {X+W,Y}, {X+W,Y+H}, {X,Y+H} ],
    polygon(Coords).


circle({X, Y}, Radius, Points) ->
    circle(X, Y, Radius, Points).
circle(X, Y, Radius, Points) ->
    TwoPi = math:pi() * 2,
    if 
	Points =< 0 -> Points2 = 1;
	true -> Points2 = Points
    end,
    AngleShift = TwoPi / Points2,
    Phi = 0,
    Coords = coords(Points2, Phi, AngleShift, Radius, X,Y, []),
    polygon(Coords).



coords(Points, Phi, AngleShift, Radius, X,Y, Acc) when Points > 0 ->
    Coord = {X+Radius*math:cos(Phi), Y+Radius*math:sin(Phi)},
    coords(Points -1, Phi+AngleShift, AngleShift, Radius, X,Y, [Coord|Acc]);
coords(0, _Phi, _ShiftAngle, _Radius, _X,_Y, Acc) ->
    lists:flatten(lists:reverse(Acc)).


