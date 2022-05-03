-module(circle).
-export([perimeter/1, square/1]).
-record(circle, {radius=0}).


perimeter(#circle{radius=Radius} = C) when is_record(C, circle) ->
    Pi = math:pi(),
    2*Radius*Pi.

square(#circle{radius=Radius} = C) when is_record(C, circle) ->
    Pi = math:pi(),
    Pi * math:pow(Radius, 2).