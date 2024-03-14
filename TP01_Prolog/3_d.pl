% Given two lists, it will return a list with the elements that are in both lists.
intersection([], _, []).
intersection([X|L1], L2, [X|R]) :- exists(X, L2), intersection(L1, L2, R).
intersection([X|L1], L2, R) :- not(exists(X, L2)), intersection(L1, L2, R).

exists(_, []) :- false.
exists(X, [X|_]).
exists(X, [Y|L2]) :- X \= Y, exists(X, L2).