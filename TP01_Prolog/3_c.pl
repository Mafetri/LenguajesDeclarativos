% Given two lists, it will return a list with the elements of the first list that are not in the second list.
union([], L2, L2).
union([X|L1], L2, [X|R]) :- union(L1, L2, R).