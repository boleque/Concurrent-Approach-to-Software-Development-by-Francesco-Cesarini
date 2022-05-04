-module(binary_tree).
-export([sum/1, max_node/1]).
-record(tree_node, {value, left_child=null, right_child=null}).


sum(#tree_node{
    value=Value,
    left_child=Left,
    right_child=Right
} = _Root) ->
    Value + sum(Left) + sum(Right);

sum(null) ->
    0.

max_node(#tree_node{value=Value, right_child=Right} = _Root) ->
    max_node_acc(Right, Value).

max_node_acc(#tree_node{value=Value, right_child=Right} = _Node, _MaxValue) ->
    max_node_acc(Right, Value);

max_node_acc(null, Max) ->
    Max.
