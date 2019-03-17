pathsLast().

pathsVert(S, I1, J, I0, L1, L):-
	nl,nl, write("I1"+I1),nl,write("J"+J),nl,
	I0 == I1,
	write("true"),
	I2 is I1 + 1,
	pathsVert(S, I2, J, I0, L1, L).
pathsVert(S, I1, J, I0, L1, L):-
	length(S, Len),
	I1 < (Len-1) -> (
		write("I":I1),nl,write("J":J),nl,
		index(S, I1, J, V),
		write("V":V), nl,
		atom_number(V, X),
		X > 0,
		write("true"), L2=[I1,J], write("L2": L2),nl,
		I2 is I1 + 1, pathsVert(S, I2, J, I0, [L2|L1], L)
	); write("END"), L=L1, !.
pathsVert(S, I1, J, I0, L1, L):-
	length(S, Len),
	I1 < (Len-1),
	I2 is I1 + 1, pathsVert(S, I2, J, I0, L1, L).
	
verticalCheck(_, _, [], []).
verticalCheck(Start, S, [[I,J]|T], [Path|R]):-
	write("I is ": I),write("   J is ": J), write("   T is": T),nl,
	verticalCheck(Start, S, T, R),
	pathsVert(S, 0, J, I, [], P),
	not(P = []),
	append([Start], [[I,J]], P1), 
	append([P1], [P], Path),
	nl,nl,write("P": P1).

pathsHor(S, I, J1, J0, L1, L):-
	nl,nl, write("J1"+J1),nl,write("J0"+J0),nl,
	J0 == J1,
	write("true"),
	J2 is J1 + 1,
	pathsHor(S, I, J2, J0, L1, L).
pathsHor([H|T], I, J1, J0, L1, L):-
	length(H, Len),
	J1 < (Len-1) -> (
		write("I":I),nl,write("J1":J1),nl,
		index([H|T], I, J1, V),
		write("V":V), nl,
		atom_number(V, X),
		X > 0,
		write("true"), L2=[I,J1], write("L2": L2),nl,
		J2 is J1 + 1, pathsHor([H|T], I, J2, J0, [L2|L1], L)
	); write("END"), L=L1, !.
pathsHor([H|T], I, J1, J0, L1, L):-
	length(H, Len),
	J1 < (Len-1),
	J2 is J1 + 1, pathsHor([H|T], I, J2, J0, L1, L).
	
appendPaths(_, [], []).
appendPaths(S, [H|T], [P|R]):-
	appendPaths(S, T, R),
	P = [S|[H]].
	
paths(D, S, [[I,J]|T], R):-
	write("I":I),nl,write(" J": J), 
	pathsHor(S, I, 0, J, [], Paths1),
	nl,nl,write("VERT"),nl,
	%pathsVert(S, 0, 1, 2, [], R),
	verticalCheck([I, J], S, Paths1, R),
	%appendPaths([I,J], Paths1, R),
	%nl,nl,write("VERT"),nl,write(J1),nl,
	%append([[I,J]],[[I1,J1]], R1),
	%append(R1, [[I2, J2]], R),
	write("R": R), nl.
	
index(Matrix, I, J, Value):-
	nth0(I, Matrix, MatrixRow),
	nth0(J, MatrixRow, Value).

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
	
=======
	write(Sol), nl,
	empty(Sol, 0, 0, [], E),
	removeFirst(E, Empty),
	write(Empty),
	paths(Desc, Sol, Empty, R),
	write(Empty).
>>>>>>> 55522d36d6d588b404702ca895fcad6d84abae88
	
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