pathsEnd(_, [], []).
pathsEnd(S, [[[Si, Sj]|[HH,[Pi, Pj]]]|T], [Path|R]):-
	pathsEnd(S, T, R),
	index(S, Pi, Sj, V),
	atom_number(V, X),
	X > 0,
	Path = [[Si, Sj], HH, [Pi, Pj], [Pi, Sj]].

pathsVert(S, I1, J, I0, L1, L):-
	I0 == I1,
	I2 is I1 + 1,
	pathsVert(S, I2, J, I0, L1, L).
pathsVert(S, I1, J, I0, L1, L):-
	length(S, Len),
	I1 < (Len-1) -> (
		index(S, I1, J, V),
		atom_number(V, X),
		X > 0,
		L2=[I1,J],
		I2 is I1 + 1, pathsVert(S, I2, J, I0, [L2|L1], L)
	); L=L1, !.
pathsVert(S, I1, J, I0, L1, L):-
	length(S, Len),
	I1 < (Len-1),
	I2 is I1 + 1, pathsVert(S, I2, J, I0, L1, L).
	
verticalCheck(_, _, [], []).
verticalCheck(Start, S, [[I,J]|T], [Path|R]):-
	verticalCheck(Start, S, T, R),
	pathsVert(S, 0, J, I, [], P),
	append([Start], [[I,J]], P1), 
	append(P1, P, Path).

pathsHor(S, I, J1, J0, L1, L):-
	J0 == J1,
	J2 is J1 + 1,
	pathsHor(S, I, J2, J0, L1, L).
pathsHor([H|T], I, J1, J0, L1, L):-
	length(H, Len),
	J1 < (Len-1) -> (
		index([H|T], I, J1, V),
		atom_number(V, X),
		X > 0,
		L2=[I,J1],
		J2 is J1 + 1, pathsHor([H|T], I, J2, J0, [L2|L1], L)
	); L=L1, !.
pathsHor([H|T], I, J1, J0, L1, L):-
	length(H, Len),
	J1 < (Len-1),
	J2 is J1 + 1, pathsHor([H|T], I, J2, J0, L1, L).
	
appendPaths(_, [], []).
appendPaths(S, [H|T], [P|R]):-
	appendPaths(S, T, R),
	P = [S|[H]].

removeLT3([], []).
removeLT3([H|T], [H|R]):-
	length(H, Len),
	Len == 3,
	removeLT3(T, R).
removeLT3([H|T], R):-
	removeLT3(T, R).
	
paths(_, _, [], []).
paths(D, S, [[I,J]|T], [Result|R]):-
	pathsHor(S, I, 0, J, [], Paths1),
	verticalCheck([I, J], S, Paths1, Paths2),
	removeLT3(Paths2, Paths3),
	paths(D, S, T, R),
	pathsEnd(S, Paths3, Result).
	
index(Matrix, I, J, Value):-
	nth0(I, Matrix, MatrixRow),
	nth0(J, MatrixRow, Value).
	
isEmpty([], true).
isEmpty(_, false).

% Check if we should go to next list%
empty([H|T], I, J, L1, L):-
	length(H, Len),
	J >= Len,
	I1 is I+1,
	empty([H|T], I1, 0, L1, L).
	
% Check current node%
empty(S, I, J, L1, L):-
	length(S, Len),
	I < Len -> (
	J1 is J+1,
	index(S, I, J, V),
	V == "-",
	L2 = [I, J],
	empty(S, I, J1, [L2|L1], L)); L = [L2|L1], !.
	
% Check next node %
empty(S, I, J, L1, L):-
	length(S, Len),
	I < Len,
	J1 is J+1,
	empty(S, I, J1, L1, L).
	
removeFirst([H|T], T).

pathCost(D, [[I,J], [I1,J1], [I2,J2], [I3, J3]], R):-
	index(D, I, J, V1), index(D, I1, J1, V2), 
	index(D, I2, J2, V3), index(D, I3, J3, V4),
	atom_number(V1, X1), atom_number(V2, X2),
	atom_number(V3, X3), atom_number(V4, X4),
	R is X1 - X2 + X3 - X4.
	
minCost(_, [], 0).
minCost(D, [[H|TT]|T], Min1):-
	minCost(D, T, Min),
	pathCost(D, H, Cost),
	Cost < Min,
	Min1 = Cost.

minimumTransportCost(D, I, Cost):-
	open(D, read, Str1),
    read_file(Str1, Lines1),
    close(Str1),
    extractHeader(Lines1, Header, DATA1),
	extract(DATA1, D1),
	removeLeft(D1, Desc),
	
	open(I, read, Str2),
    read_file(Str2, Lines2),
    close(Str2),
    extractHeader(Lines2, Header, DATA2),
	extract(DATA2, D2),
	removeLeft(D2, Sol),
<<<<<<< HEAD
	write(Desc), nl, write(Sol), nl,
	.
	
emptyCells([H|T]):-
	write(Sol), nl,
=======
>>>>>>> 707f99732fdff2faf8793405c7478a963809ea29
	empty(Sol, 0, 0, [], E),
	removeFirst(E, Empty), !,
	paths(Desc, Sol, Empty, R),
	delete(R, [], Paths),
	minCost(Desc, Paths, C), !,
	Cost is C.
	
extractHeader([H|T], H, T).

removeLeft([], []).
removeLeft([[H|TT]|T], [TT|L]):-
	removeLeft(T, L).
	
extract([], []).
extract([H|T], [Q|L]):-
	split_string(H, "\s", "\s", Q),
	extract(T, L).

read_file(Stream,[]) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream,Codes),
    atom_chars(X, Codes),
    read_file(Stream,L), !.