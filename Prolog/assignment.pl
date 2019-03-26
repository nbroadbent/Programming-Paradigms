city(ottawa,ontario).
city(guelph,ontario).
city(kingston,ontario).
city(gatineau,quebec).
city(montreal,quebec).
team(ravens,ottawa).
team(ggs,ottawa).
team(gryphons,guelph).
team(queens,kingston).
team(torrents,gatineau).
team(stingers,montreal).
sport(annie,lacrosse).
sport(paul,crosscountry).
sport(suzy,ski).
sport(robert,basketball).
sport(tom,lacrosse).
sport(tim,ski).
sport(annie,ski).
sport(joe,basketball).
sport(robert,basketball).
sport(jane,basketball).
sport(marie,crosscountry).
sport(suzy,crosscountry).
sport(jack,ski).
sport(simon,lacrosse).
player(annie,gryphons).
player(tom,torrents).
player(jane,stingers).
player(marie,ggs).
player(joe,ravens).
player(jack,queens).
player(simon,ravens).
player(suzy,torrents).
player(paul,ggs).
player(marie,ggs).
player(simon,gryphons).

%1.a)
bball(X):-
	findall(P, sport(P, basketball), Bag),
	sort(Bag, X).
	
%1.b)
findTeams([], []).
findTeams([City|T], [Teams|R]):-
	findTeams(T, R),
	findall(Team, team(Team, City), Teams).
ontario(X):-
	findall(C, city(C, ontario), Cities),
	findTeams(Cities, R),
	flatten(R, Flat), sort(Flat, X).

%1.c)
duplicates([], []).
duplicates([H1, H2|T], [H1|R]):-
	duplicates(T, R),
	H1 == H2.
duplicates([H|T], R):-
	duplicates(T, R).
sportPlayers([], []).
sportPlayers([Sport|T], [X|R]):-
	sportPlayers(T, R),
	findall(Player, sport(Player, Sport), Bag), sort(Bag, X).
playMore(X):-
	findall(Sports, sport(_, Sports), Sports), sort(Sports, Allsports),
	sportPlayers(Allsports, Players), flatten(Players, Flat), 
	sort(0, @=<, Flat, X1), duplicates(X1, X), !.
	
%1.d)
intersect([], _, []).
intersect([[Player,Sport]|T], OttawaPlayers, [[Player,Sport]|R]):-
	%write("P":Player),nl,
	intersect(T, OttawaPlayers, R),
	member(Player, OttawaPlayers).
intersect([H|T], OttawaPlayers, R):-
	intersect(T, OttawaPlayers, R).
teamsPlayers([], []).
teamsPlayers([Team|T], [X|R]):-
	teamsPlayers(T, R),
	findall(Player, player(Player, Team), X).
listPlayers1(X):-
	findall(Team, team(Team, ottawa), Teams),
	teamsPlayers(Teams, P), flatten(P, Ottawa),
	findall([Player, Sport], sport(Player, Sport), All),
	intersect(All, Ottawa, X).

listPlayers(X):-
	findall((Player, Sport), (sport(Player, Sport), player(Player, Team), team(Team, ottawa)), X).

myAnswer():-
	bball(A), write(A), nl,
	ontario(B), write(B), nl,
	playMore(C), write(C), nl,
	listPlayers(D), write(D), nl,
	setof(sport(Player,Sport), (D), E), write(E).
	
%2.
interest(X):-
	sport(X, ski).
interest(X):-
	sport(X,S),
	S \= ski,
	player(X, T),
	team(T, C),
	city(C, quebec). 
	
%3.
area([[Ax,Ay],[Bx,By],[Cx,Cy]], R):-
	R is 0.5*((Ax-Cx)*(By-Ay) - (Ax-Bx)*(Cy - Ay)).

%4. a)
noSkip([], _, []).
noSkip([H|T], L, [H|R]):-
	member(H, L), 
	skipIt(T, L, R).
noSkip([H|T], L, [H|R]):-
	noSkip(T, L, R).
skipIt([], _, []).
skipIt([H|T], L, R):-
	member(H, L), 
	noSkip(T, L, R).
skipIt([H|T], L, R):-
	skipIt(T, L, R).
skip(L1, L2, R):-
	skipIt(L1, L2, R), !.

%4.  b)
forward([], _, []).
forward([H|T], L, [H|R]):-
	member(H, L),
	rev(T, L, R).
forward([H|T], L, [H|R]):-
	forward(T, L, R).
rev([], _, []).
rev([H|T], L, [R|H]):-
	member(H, L),
	forward(T, L, R).
rev([H|T], L, [R|H]):-
	rev(T, L, R).
switch([[[H|TTT]|TT]|T], R):-
	flatten([TTT, TT, T, H], R).
turn(L1, L2, R):-
	rev(L1, L2, R1),
	switch(R1, R), !.
	
find([], _, _, _, []).
find([H|T], S, L, N, R):-
	not(member(H, S)),
	find(T, S, [H|L], N, R).
find([H|T], S, L, N, [L1|R]):-
	member(H, S),
	N1 is N+1,
	find(T, S, [], N1, R),
	X is N mod 2,
	(X is 0) -> reverse([H|L], L1); L1 = [H|L].
	
turn1(L1, L2, R):-
	find(L1, L2, [], 1, R).
	
%5. a)
treeEx(X):-
	X = t(73,t(31,t(5,nil,nil),nil),t(101,t(83,nil,t(97,nil,nil)),nil)). 

isNil(nil).
single1(nil, []).
single1(t(Key,L,R), [Key|[LL|RR]]):-
	(isNil(L); isNil(R)), not((isNil(L),isNil(R))),
	single1(L, LL), single(R, RR).
single1(t(Key,L,R), [LL|RR]):-
	single1(L, LL), single(R, RR).
single(Tree, R):-
	single1(Tree, R1),
	flatten(R1, R).

%5. b)
singleFill(nil, []).
singleFill(t(Key,L,R), [H|[LL|RR]]):-
	not((isNil(L),isNil(R))), 
	isNil(L) -> H = t(Key,  t(0, nil, nil), R), singleFill(L, LL), singleFill(R, RR); 
		isNil(R) -> H = t(Key, L, t(0, nil, nil));
		singleFill(L, LL), singleFill(R, RR).
singleFill(t(Key,L,R), [[t(Key,L,R)|LL]|RR]):-
	singleFill(L, LL), singleFill(R, RR).




















