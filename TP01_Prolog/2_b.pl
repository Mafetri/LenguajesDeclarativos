% rotacion a la derecha Ej. entra [1,2,3,4] y sale [4,1,2,3]

right_rotate(L, R) :- get_last(L, X, L2), preappend(X, L2, R).

% Gets the last element of the list and removes it from the list
get_last([X], X, []).
get_last([X|L], R, [X|L2]) :- get_last(L, R, L2).

% Appends X infront of L
preappend(X, L, [X|L]).