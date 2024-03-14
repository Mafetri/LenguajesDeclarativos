% Given two lists, it will return false if the first list is not a subset of the second list or true if it is.
inclusion([], _).
inclusion([X|L1], L2) :- exists(X, L2), inclusion(L1, L2).
exists(_, []) :- false.
exists(X, [X|_]).
exists(X, [Y|L]) :- X \= Y, exists(X, L).