% If both are empty
isomorphic(nil, nil).

% If both are leafs
isomorphic(tree(nil, _, nil), tree(nil, _, nil)).

% If both have a child
isomorphic(tree(Left1, _, Right1), tree(Left2, _, Right2)) :-
    isomorphic(Left1, Left2),  % Check if left subtrees are isomorphic
    isomorphic(Right1, Right2).  % Check if right subtrees are isomorphic

% Testing:
% - Isomorphic: isomorphic(tree(tree(tree(nil, 4, nil), 2, tree(nil, 5, nil)), 1, tree(tree(nil, 3, nil), 2, tree(nil, 4, nil))), tree(tree(tree(nil, 4, nil), 2, tree(nil, 5, nil)), 1, tree(tree(nil, 5, nil), 2, tree(nil, 4, nil)))).
% - Non isomorphic: isomorphic(tree(tree(tree(nil, 4, nil), 2, tree(nil, 5, nil)), 1, tree(tree(nil, 3, nil), 2, tree(nil, 4, nil))), tree(tree(tree(nil, 4, nil), 2, tree(nil, 5, nil)), 1, tree(tree(nil, 5, nil), 3, nil))).
