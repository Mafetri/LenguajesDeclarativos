cardinality(L, R) :- count_not_repeated(L, [], R).

count_not_repeated([], _, 0).
count_not_repeated([X|L], L2, R) :- not(exists(X, L2)), count_not_repeated(L, [X|L2], Z), R is Z + 1.
count_not_repeated([X|L], L2, R) :- exists(X, L2), count_not_repeated(L, L2, R).

exists(_, []) :- false.
exists(X, [X|_]) :- true.
exists(X, [Y|L2]) :- X \= Y, exists(X, L2).