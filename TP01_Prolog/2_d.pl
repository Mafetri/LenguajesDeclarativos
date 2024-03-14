replace_ocurrency([], _, _, []).
replace_ocurrency([X|L], X, Y, [Y|R]) :- replace_ocurrency(L, X, Y, R).
replace_ocurrency([S|L], X, Y, [S|R]) :- S \= X, replace_ocurrency(L, X, Y, R).