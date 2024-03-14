difference([], L2, L2).
difference(L1, [], L1).
difference([X|L1], [X|L2], R) :- difference(L1,L2,R).
difference([X|L1], [Y|L2], [X,Y|R]) :- X \= Y, difference(L1,L2,R).