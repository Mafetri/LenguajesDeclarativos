cartesian(_, [], []).
cartesian([], _, []).
cartesian([X|L1], L2, R) :-
    cartesian_elem(X, L2, R1),
    cartesian(L1, L2, R2),
    append(R1, R2, R).

% Helper pairs the X given with all the elements of the list L2
cartesian_elem(_, [], []).
cartesian_elem(X, [Y|L2], [[X,Y]|R]) :-
    cartesian_elem(X, L2, R).
