double([X], [X, X]).
double([X|L], [X,X|R]) :- double(L, R).