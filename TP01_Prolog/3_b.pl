% Given two lists, it will return true if they are equal or false if they are not.
equal([], []).
equal([X|L1], [X|L2]) :- equal(L1, L2).