% Invert from 2_f
invert(L, R) :- invertion(L,[],R).
invertion([],R,R).
invertion([X|L],Invertido, R) :- invertion(L,[X|Invertido],R).

% append from 2_a
append([],L,L).
append([H|T],L,[H|Z]) :- append(T,L,Z).

palindromo(L, R) :- invert(L, L2), append(L, L2, R).