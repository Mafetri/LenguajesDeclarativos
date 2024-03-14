lista(0, [1,2,3,4,5,6,7]).					% Target List
lista(1, [1|2,3,4,5,6,7]).					% Error: 2,3,4,5,6,7 is not a list
lista(2, [1, 2, 3, 4, 5, 6, 7|[]]).			% Same as 0, cause is the same list but with an empty list at the end
lista(3, [1|[2, 3, 4, 5, 6, 7]]).			% Same as 0, cause is the same first element with the rest of the list at the end
lista(4, [1, 2, 3|[4|[5, 6, 7]]]).			% Same as 0.
lista(5, [1|[2, 3, [4, 5, 6, 7]]]).			% Not the same: [1, 2, 3, [4, 5, 6, 7]], cause is 1 followed by the elements 2, 3 and another list
lista(6, [1, 2|[3, 4, 5, 6, 7, []]]).		% Not the same: [1, 2, 3, 4, 5, 6, 7, []], cause is 1 and 2 followed by the elements 3, 4, 5, 6, 7 and an empty list
lista(7, [1, 2|[3, 4, 5, 6, 7|[]]]).		% Same as 0, the empty list at the end is not an element of the list, is the end of the list
lista(8, [1, 2, [3, 4,[5, 6, [7]]]]).		% Not the same: [1, 2, [3, 4, [5, 6, [7]]]], cause is 1 and 2 followed by a list, and 3 and 4 followed by another list
lista(9, [1|[2|[3|[4|[5|[6|[7|[]]]]]]]]).	% Same as 0.