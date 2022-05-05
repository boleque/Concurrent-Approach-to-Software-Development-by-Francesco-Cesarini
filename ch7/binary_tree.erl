-module(binary_tree).
-export([sum/1, max_node/1, is_valid/1]).
-export([is_valid_helper/3]).
-record(tree_node, {value, left_child=null, right_child=null}).


sum(#tree_node{
        value=Value,
        left_child=Left,
        right_child=Right
    } = _Root
) ->
    Value + sum(Left) + sum(Right);

sum(null) ->
    0.

max_node(#tree_node{value=Value, right_child=Right} = _Root) ->
    max_node_acc(Right, Value).

max_node_acc(#tree_node{value=Value, right_child=Right} = _Node, _MaxValue) ->
    max_node_acc(Right, Value);

max_node_acc(null, Max) ->
    Max.

is_valid(Root) ->
    is_valid_helper(Root, -1000000, 1000000).

is_valid_helper(null, _Min_value, _Max_value) ->
    true;

is_valid_helper(#tree_node{value=Value, left_child=Left, right_child=Right}=_Node, Min_value, Max_value) ->
    if
        Value < Min_value ->
            false;
        Value > Max_value ->
            false;
        true ->
            A = is_valid_helper(Right, Value, Max_value),
            B = is_valid_helper(Left, Min_value, Value),
            A and B
    end.
