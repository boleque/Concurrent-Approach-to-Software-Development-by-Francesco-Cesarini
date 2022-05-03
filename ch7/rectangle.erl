-module(rectangle).
-export([perimeter/1, square/1]).

-record(rectangle, {lenght=0, height=0}).


perimeter(#rectangle{lenght=Lenght, height=Height} = R) when is_record(R, rectangle) ->
    2*(Lenght + Height).

square(#rectangle{lenght=Lenght, height=Height} = R) when is_record(R, rectangle) ->
    Lenght * Height.