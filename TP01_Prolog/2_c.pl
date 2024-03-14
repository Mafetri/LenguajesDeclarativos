delete_ocurrency([], _, []).
delete_ocurrency([X|L], X, R) :- delete_ocurrency(L, X, R).
delete_ocurrency([S|L], X, [S|R]) :- S \= X, delete_ocurrency(L, X, R).