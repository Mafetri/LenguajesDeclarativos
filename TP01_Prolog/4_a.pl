% =========  Member  =========
member(X, tree(_, X, _)).
member(X, tree(Left, Root, _)) :-
    X < Root,
    member(X, Left).
member(X, tree(_, Root, Right)) :-
    X > Root,
    member(X, Right).

% =========  Add element  =========
add(X, nil, tree(nil, X, nil)).
add(X, tree(Left, Root, Right), tree(NewLeft, Root, Right)) :-
    X < Root,
    add(X, Left, NewLeft).
add(X, tree(Left, Root, Right), tree(Left, Root, NewRight)) :-
    X > Root,
    add(X, Right, NewRight).

% =========  Delete element  =========
% When the element is a leaf
delete(X, tree(nil, X, nil), nil).

% When the element has only one child
delete(X, tree(Left, X, nil), Left) :- Left \= nil.
delete(X, tree(nil, X, Right), Right) :- Right \= nil.

% When it has two children
delete(X, tree(Left, X, Right), tree(Left, Min, UpdatedRight)) :-
    Left \= nil,
    Right \= nil,
    find_min(Right, Min),               % Find the minimum element in the right subtree (the sucesor of X)
    delete(Min, Right, UpdatedRight),   % Delete the minimum element from the right subtree

% Recursively finding the node to delete.
delete(X, tree(Left, Value, Right), tree(NewLeft, Value, Right)) :-
    X < Value,
    delete(X, Left, NewLeft).

delete(X, tree(Left, Value, Right), tree(Left, Value, NewRight)) :-
    X > Value,
    delete(X, Right, NewRight).

% Helper, finds the minimum element in a tree.
find_min(tree(nil, X, _), X).
find_min(tree(Left, _, _), Min) :-
    find_min(Left, Min).

% Inputs for testing: tree(tree(nil,1,nil), 2, tree(tree(nil,3,nil),4,nil))
% Deletion:
% -  When the item is not in the tree: delete(5,tree(tree(nil,1,nil), 2, tree(tree(nil,3,nil),4,nil)), R).
% -  When the item is in the tree (it has two childrens): delete(2,tree(tree(nil,1,nil), 2, tree(tree(nil,3,nil),4,nil)), R).
% -  When the item is in the tree (it has only one child): delete(4,tree(tree(nil,1,nil), 2, tree(tree(nil,3,nil),4,nil)), R).
% -  When the item is in the tree (it is a leaf): delete(1,tree(tree(nil,1,nil), 2, tree(tree(nil,3,nil),4,nil)), R).