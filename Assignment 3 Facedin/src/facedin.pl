
/**
  assignment predicate 1)
*/
buddies(G, X, Y):-
	friendof(G, X, Y),
	friendof(G, Y, X).

/**
  friendof(G, X, Y)
  Predicate which takes a friend graph, G, and the name of a person, X, and a person Y, both from the friend graph.
  True, iff Y is found in person X's friendlist.
*/
friendof([person(X, [Y|_]) | _], X, Y).
friendof([person(X, [_ | L]) | R], X, Y) :-
	friendof([person(X, L) | R], X, Y).
friendof([person(_, _) | L], X, Y) :-
	friendof(L, X, Y).	
	
	
/**
	getperson(G, X, P).
	Predicate which takes a friend graph, G, and the name of a person, X.
	Is true if P is person with name X in the format of person(X, L) in G.
*/
getperson([person(X, L) | _], X, person(X, L)).
getperson([person(_, _) | R], X, person(X, L)) :-
	getperson(R, X, person(X, L)).

/**
	assignment predicate 2)
	Clique(G, L), where G is a friend graph and L is a list of names perhaps forming a clique.
	The predicate first extracts the names of the people in the graph G and checks if L is a subset of G. If L is not a subset of G then L cannot be a clique.
	This ensures that we can run the declaration without a given clique and still get the right possible cliques in the graph G.
	
	The predicate is true if the helper predicate cliquehelp is also true. cliquehelp will be explained below.
*/
clique(G, L) :-
	getpersonlist(G, Persons),
	subsetof(L, Persons),
	cliquehelp(G, L, L).

/**
	cliquehelp(G, L, L) is given a person graph, G, and two identical lists of persons, L.
	The predicate is true if for all persons, person(X, Friends) in L, L\X is a subset of the friendlist, Friends, of X.
	This is done by first finding the friend list from G for the person with the getperson predicate, removing X from L with the myselect predicate,
	and then using subsetof (exxplained below) to see if L\X is indeed a subset of Friends.
	cliquehelp is then called recursively with the graph G as well as the rest of the names in L, as well as the whole of L.
*/	
cliquehelp(_, [], [_,_|_]).	
cliquehelp(G, [X | Tail], L) :-
	getperson(G, X, person(X, Friends)), /** We first get the firstlist of X */
	myselect(X, L, Rest), 				 /** We then remove X from the possible clique, L */
	subsetof(Rest, Friends),			 /** We then see if the list from the step above is indeed a subset of X's friends */
	cliquehelp(G, Tail, L).				 /** Recursively see if the above holds for the rest of the possible clique, L */


	
/*
	the predicate getpersonlist(G, L) is true if L is a list of the names of the persons in the friends graph G.
*/
getpersonlist([], []).
getpersonlist([person(X, _) | Tail], [X|Rest]):-
	getpersonlist(Tail, Rest).
	
/*
	the subsetof(Set1, Set2) is true iff Set1 is a subset of Set2.
*/	
subsetof([], []).
subsetof([], [_|_]).	
subsetof([Head | Tail], L) :-
	myselect(Head, L, R),
	subsetof(Tail, R).
	
myselect(X, [X|Rest], Rest).
myselect(X, [Y, Z | Rest1], [Y | Rest2]) :-
	myselect(X, [Z | Rest1], Rest2).

/*
	assignment predicate 3)
	admirer(G, ken)
*/
	
admirer(G, X) :- 
	getpersonlist(G, L),
	myselect(X, L, Rest),
	pathToAll(G, X, Rest).

pathToAll(_, _, []).	
pathToAll(G, X, [Head | Tail]) :-
	pathToPerson(G, X, Head),
	pathToAll(G, X, Tail).

pathToPerson(G, X, Y) :-
	friendof(G, X, Y).
pathToPerson(G, X, Y) :-
	friendof(G, X, Z),
	getperson(G, X, P),
	myselect(P, G, Rest),
	pathToPerson(Rest, Z, Y).
	
/*
	idol(G, tony).
*/

idol(G, X) :-
	getpersonlist(G, L),
	myselect(X, L, Rest),
	pathFromAll(G, X, Rest).
	
pathFromAll(_, _, []).
pathFromAll(G, X, [Head| Tail]) :-
	pathToPerson(G, Head, X),
	pathFromAll(G, X, Tail).

	
/* 
	path()
*/

ispath(G, X, Y, P) :-
	isfirst(P, X),
	ispathbuilder(G, P),
	islast(P, Y),
	checkforduplicates(G, P).
/*
	checkforduplicates is quite likely redundant, however not enough testing has been done to verify this.
*/
checkforduplicates([],[]).	
checkforduplicates(G, [Head,<- | Tail]):-
	myselect(person(Head, _), G, NewG),
	checkforduplicates(NewG, Tail).
checkforduplicates(G, [Head,-> | Tail]):-
	myselect(person(Head, _), G, NewG),
	checkforduplicates(NewG, Tail).
checkforduplicates(G, [Head]) :-
	myselect(person(Head, _), G, _).

ispathbuilder(G, [X,->,Y]):-
	friendof(G, X, Y).
ispathbuilder(G, [X,<-,Y]):-
	friendof(G, Y, X).
ispathbuilder(G, [X,->,Y|Rest]):-
	friendof(G, X, Y),
	getperson(G, X, P),
	myselect(P, G, NewG), 
	ispathbuilder(NewG, [Y|Rest]).	
ispathbuilder(G, [X,<-,Y|Rest]):-
	friendof(G, Y, X),
	getperson(G, X, P),
	myselect(P, G, NewG),
	ispathbuilder(NewG, [Y|Rest]).
	
isfirst([X|_], X).
isfirst([X|_], X).

islast([X], X).
islast([X], X).
islast([_|Rest], X) :-
	islast(Rest, X).
