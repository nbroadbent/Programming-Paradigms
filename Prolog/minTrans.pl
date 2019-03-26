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
removeLT3([_|T], R):-
	removeLT3(T, R).
	
paths(_, _, [], []).
paths(D, S, [[I,J]|T], [Result|R]):-
	pathsHor(S, I, 0, J, [], Paths1),
	verticalCheck([I, J], S, Paths1, Paths2),
	removeLT3(Paths2, Paths3),
	paths(D, S, T, R),
	pathsEnd(S, Paths3, Result).
	
% Helper function that allows us to find values in 2d list
index(Matrix, I, J, Value):-
	nth0(I, Matrix, MatrixRow),
	nth0(J, MatrixRow, Value).
	
% Replace item at index J in 1d list with value
insert1D([], _, _, []).
insert1D([H|T], J, Value, [Value|T]):-
	J == 0.
insert1D([H|T], J, Value, [H|R]):-
	J > 0,
	J1 is J - 1,
	insert1D(T, J1, Value, R).
	
% Loop until we find the right row, then insert 1d into row.
insert2D([], _, _, _, []).
insert2D([H|T], I, J, Value, [Row|T]):-
	I == 0,
	insert1D(H, J, Value, Row).
insert2D([H|T], I, J, Value, [H|R]):-
	I > 0,
	I1 is I - 1,
	insert2D(T, I1, J, Value, R).
	
appendFront(Value, List, [Value|List]).
	
% Function to check if a list is empty
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
	
removeFirst([_|T], T).

pathCost(D, S, [[I,J], [I1,J1], [I2,J2], [I3, J3]], R):-
	% Calculate max amount to tranfer for this path, where max is min of subtracted nodes
	index(S, I, J, Vs1), index(S, I1, J1, Vs2), 
	index(S, I2, J2, Vs3), index(S, I3, J3, Vs4),
	convZero(Vs1, Xs1), convZero(Vs2, Xs2),
	convZero(Vs3, Xs3), convZero(Vs4, Xs4),
	min_list([Xs2, Xs4], Min),
	
	% Calculate path cost
	index(D, I, J, V1), index(D, I1, J1, V2), 
	index(D, I2, J2, V3), index(D, I3, J3, V4),
	convZero(V1, X1), convZero(V2, X2),
	convZero(V3, X3), convZero(V4, X4),
	R is (X1 - X2 + X3 - X4) * Min.
	
minCost(_, _, [], _, 0).
minCost(D, S, [[H|_]|T], Path, Min1):-
	minCost(D, S, T, Path, Min),
	pathCost(D, S, H, Cost),
	Cost < Min,
	Min1 = Cost,
	Path = H.
	
rowCost([H|[]], _, C, C).
rowCost([H1|T1], [H2|T2], C, Cost):-
	convZero(H1, N1), convZero(H2, N2),
	C1 is C + (N1 * N2),
	rowCost(T1, T2, C1, Cost).
	
totalCost([H|[]], _, C, C).
totalCost([H1|T1], [H2|T2], C1, Cost):-
	rowCost(H1, H2, 0, C),
	C2 is C1 + C,
	totalCost(T1, T2, C2, Cost), !.

minimumTransportCost(D, I, Cost):-
	open(D, read, Str1),
    read_file(Str1, Lines1),
    close(Str1),
    extractHeader(Lines1, Header1, DATA1),
	extract(DATA1, D1),
	removeLeft(D1, Desc),
	
	open(I, read, Str2),
    read_file(Str2, Lines2),
    close(Str2),
    extractHeader(Lines2, Header2, DATA2),
	extract(DATA2, D2),
	removeLeft(D2, Sol),
	empty(Sol, 0, 0, [], E),
	removeFirst(E, Empty), !,
	paths(Desc, Sol, Empty, R),
	delete(R, [], Paths),
	minCost(Desc, Sol, Paths, Path, C), !,
	transfer(Sol, Path, NewSol),
	writeSolution([Header2|NewSol]),
	totalCost(Desc, NewSol, 0, Cost), !.
	
convZero(V, V1):-
	not(number(V)) -> (
		V == "-" -> (V1 is 0); (atom_number(V, V1))
	); V1 is V.
	
transfer(S, [[I,J], [I1,J1], [I2,J2], [I3, J3]], R):-
	index(S, I, J, V1), index(S, I1, J1, V2), 
	index(S, I2, J2, V3), index(S, I3, J3, V4),
	
	% Convert - to 0
	convZero(V1, X1), convZero(V2, X2),
	convZero(V3, X3), convZero(V4, X4),
	% We will transfer min amount so we don't take too much.
	min_list([X2, X4], Min),
	X11 is X1 + Min, X22 is X2 - Min,
	X33 is X3 + Min, X44 is X4 - Min,
	% Now insert the transfer into our lists
	insert2D(S, I, J, X11, S1), insert2D(S1, I1, J1, X22, S2),
	insert2D(S2, I2, J2, X33, S3), insert2D(S3, I3, J3, X44, R).

formatRow(L, N, M, R):-
	N > 1 -> ( N == M -> (
			appendFront("DEMAND", L, R)
		); (
			% Make source1, source2... string then append to front of row
			atom_concat("Source", N, Source),
			appendFront(Source, L, R)
		)
	); R = L.
	
writeStream([], _, _, _).
writeStream([H|T], N, M, S):-
	flatten(H, F),
	formatRow(F, N, M, Row),
	atomic_list_concat(Row, ' ', String),
	write(S, String), write(S, "\n"),
	N1 is N + 1,
	writeStream(T, N1, M, S).
writeSolution(Data):-
	open("solution.txt", write, S),
	length(Data, M),
	writeStream(Data, 1, M, S),
	close(S).
	
extractHeader([H|T], H, T).

removeLeft([], []).
removeLeft([[_|TT]|T], [TT|L]):-
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