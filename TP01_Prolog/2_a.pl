% rotacion a la izquierda. Ej. entra [1,2,3,4] y sale [2,3,4,1]

% 
append([],L,L).
append([H|T],L,[H|Z]) :- append(T,L,Z).

% Appends the first element of the list to the end of the list into Y
left_rotate([X | Xs], Y) :- append(Xs, [X], Y).