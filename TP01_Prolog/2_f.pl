invert(L, R) :- invertion(L,[],R).
invertion([],R,R).
invertion([X|L],Invertido, R) :- invertion(L,[X|Invertido],R).