% In-order
in_order(nil, []).
in_order(tree(Left, Value, Right), Result) :-
    in_order(Left, LeftResult),
    in_order(Right, RightResult),
    append(LeftResult, [Value|RightResult], Result).

% Pre-order
pre_order(nil, []).
pre_order(tree(Left, Value, Right), Result) :-
    pre_order(Left, LeftResult),
    pre_order(Right, RightResult),
    append([Value|LeftResult], RightResult, Result).

% Post-order
post_order(nil, []).
post_order(tree(Left, Value, Right), Result) :-
    post_order(Left, LeftResult),
    post_order(Right, RightResult),
    append(LeftResult, RightResult, Temp),
    append(Temp, [Value], Result).
