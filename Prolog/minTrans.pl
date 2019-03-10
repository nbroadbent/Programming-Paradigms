minimumTransportCost(D, I, Cost):-
	open('3x3.txt', read, Str1),
    read_file(Str1, Lines1),
    close(Str1),
    extractHeader(Lines1, Header, DATA1),
	extract(DATA1, D1),
	removeLeft(D1, Desc).
	
	open('3x3.txt', read, Str2),
    read_file(Str2, Lines2),
    close(Str2),
    extractHeader(Lines2, Header, DATA2),
	extract(DATA2, D2),
	removeLeft(D2, Sol).
	
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