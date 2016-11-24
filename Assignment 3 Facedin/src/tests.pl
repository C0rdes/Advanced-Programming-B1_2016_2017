%:- [faceIn].
:- load_files(facedIn).
/*
	Used for testing purposes
*/
g0([person(ralf, [susan, ken]),
 person(ken, [ralf]),
 person(susan, [reed, jessica, jen, ralf]),
 person(reed, [tony, jessica]),
 person(jessica, [jen]),
 person(tony, []),
 person(jen, [susan, jessica, tony])]).

/*
	buddies tests
*/
testbuddiestrue :-
	g0(G),
	buddies(G, susan, jen).
	
testbuddiestrue2(Y) :-
	g0(G),
	buddies(G, jen, Y).
	
testbuddiesfalse :-
	g0(G),
	buddies(G, ken, reed).
	
/*
	clique tests
*/

testcliquetrue :-
	g0(G),
	clique(G, [susan, jen]).
	
testcliquetrue2(L) :-
	g0(G),
	clique(G, L).
	
testcliquefalse :-
	g0(G),
	clique(G, [ralf, susan, ken]).
	
/*
	admirer tests
*/

testadmirertrue :-
	g0(G),
	admirer(G, ralf).
	
testadmirertrue(X) :-
	g0(G),
	admirer(G, X).
	
testadmirerfalse :-
	g0(G),
	admirer(G, tony).
	
/*
	idol tests
*/

testidoltrue :-
	g0(G),
	idol(G, tony).
	
testidoltrue(X) :-
	g0(G),
	idol(G, X).
	
testidolfalse :-
	g0(G),
	idol(G, ken). /* sorry, Ken! */
	
/*
	ispath tests
*/

testispathtrue1 :-
	g0(G),
	ispath(G, ralf, jen, [ralf,->,susan,->,jen]).
	
testispathtrue2 :-
	g0(G),
	ispath(G, ken, jen, [ken, <-, ralf, ->, susan, ->, jessica, ->, jen]).
	
testispathtrue3(P) :-
	g0(G),
	ispath(G, jen, tony, P).
	
testispathtrue4(P) :-
	g0(G),
	ispath(G, tony, ken, P).
	
	