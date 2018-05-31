hl(L X):- d(L, ZP, ZK, JP, JK, DP, DK), ZP=X, ZK=JP, JK=DP, DK=[].
d([0|L], [0|ZP], ZK, JP, JK, DP, DK) :- d(L, ZP, ZK, JP, JK, DP, DK).
d([1|L], ZP, ZK, [1|JP], JK, DP, DK) :- d(L, ZP, ZK, JP, JK, DP, DK).
d([2|L], ZP, ZK, JP, JK, [2|DP], DK) :- d(L, ZP, ZK, JP, JK, DP, DK).
d([], Z, Z, J, J, D, D).

rozplaszcz(L, W) :- rozplaszcz(L, [], [], X), odwroc(X, W).
rozplaszcz([], [], A, A) :- !.
rozplaszcz([], [E|R], A, W) :- !, rozplaszcz(E, R, A, W).
rozplaszcz([E|L], R, A, W) :- !, rozplaszcz(E, [L|R], A, W).
rozplaszcz(X, R, A, W) :- rozplaszcz([], R, [X|A], W).

nielist([]) :- !, fail.
nielist([_|_]) :- !, fail.
nielist(_).

nielist2(X) :- functor(X, N, K), (N, K)\=([], 0), (N, K)\=('.', 2).

list([]).
list([_|_]).

nielist3(X) :- \+(list(X)).

lista([]).
lista([_|L]) :- lista(L).

flatten1([], []).
%flatten1([[]|L], W) :- flatten1(L, W).
flatten1([X|L2], W) :- lista(X), !, flatten1(X, W1), flatten1(L2, W2), append(W1, W2, W).
flatten1([X|L], [X|W]) :- flatten1(L, W).

flatten2(L, F) :- fl(L, [], F).
fl([], A, A).
fl([X|L], A, W) :- lista(X), !, fl(X, A, WX), fl(L, WX, W).
fl([X|L], A, W) :- fl(L, [X|A], W).

cos(X) :- false.
cos([A|B]).


drzewo(nil).
drzewo(tree(L, _, P)) :- drzewo(L), drzewo(P).

insert(nil, Elem, tree(nil, Elem, nil)).
insert(tree(L, W, P), E, tree(NL, W, P)) :- E =< W, !, insert(L, E, NL).
insert(tree(L, W, P), E, tree(L, W, NP)) :- E > W, insert(P, E, NP).


connectT(G, A, B) :- member(kr(A, B), G).
connectT(G, A, C) :- member(kr(A, B), G), connectT(G, B, C).

connectK(A, B) :- edge(A, B).
connectK(A, C) :- edge(A, B), connectK(B, C).

cncT(G, A, B) :- member(A <-> B, G).
cncT(G, A, C) :- member(A <-> B, G), cncT(G, B, C).




wszerz(D, LW) :- wszerz_pom([D], LW).

wszerz_pom([], []).
wszerz_pom([nil | KD], LW) :- wszerz_pom(KD, LW).
wszerz_pom([tree(L, W, P) | KD], [W | LW]) :-
            append(KD, [L, P], NKD),
            wszerz_pom(NKD, LW).


stworzDrzewo(L, D) :-  stworzDrzewo(L, nil, D).

stworzDrzewo([], D, D).
stworzDrzewo([ E | L ], T, D) :-
             insert(T, E, NT),
             stworzDrzewo(L, NT, D).

stworzDrz([], nil).
stworzDrz([ E | L ], T) :-
  insert(D, E, T),
	stworzDrz(L, D).


% DAG - reprezentacja klauzulowa
path(A, B, [A, B]) :- edge(A, B).
path(A, C, [A | P]) :- edge(A, B), path(B, C, P).


% dowolny graf (cykliczny)
pathC(X, Y, P) :- pathC(X, Y, [], P).

pathC(X, Y, _Visited, [X, Y]) :- edge(X, Y).
pathC(X, Y, Visited, [X | P]) :-
              edge(X, Z),
              nonmember(Z, Visited),
              pathC(Z, Y, [X | Visited], P).

% bez pętelek
path2(X, Y, P) :- path2(X, Y, [X], P).

path2(X, Y, _Visited, [X, Y]) :- edge(X, Y).
path2(X, Y, Visited, [X | P]) :-
              edge(X, Z),
              nonmember(Z, Visited),
              path2(Z, Y, [Z | Visited], P).

% inicjalizacja odwiedzonych na [A, B]
path3(X, Y, P) :- path3(X, Y, [X, Y], P).

path3(X, Y, _Visited, [X, Y]) :- edge(X, Y).
path3(X, Y, Visited, [X | P]) :-
              edge(X, Z),
              nonmember(Z, Visited),
              path3(Z, Y, [Z | Visited], P).

% proste ścieżki, co najmniej jedna krawędź, uwzględniając cykle
path4(X, Y, P) :- path4(X, Y, [X], P), P=[_,_|_].

path4(X, Y, _Visited, [X]).
path4(X, Y, Visited, [X | P]) :-
              edge(X, Z),
              nonmember(Z, Visited),
              path4(Z, Y, [Z | Visited], P).


euler([ W1 <-> W2 ], [ W1, W2 ]).
euler([ W1 <-> W2 ], [ W2, W1 ]).
euler(G, [ W1, W2 | Dalej ]) :-
      pobierz(G, W1 <-> W2, G1),
      euler(G1, [ W2 | Dalej ]).

pobierz([ W1 <-> W2 | G ], W1 <-> W2, G).
pobierz([ W1 <-> W2 | G ], W2 <-> W1, G).
pobierz([ E | G ], Kr, [ E | NG ]) :-  pobierz(G, Kr, NG).


% init(Kolejka) - inicjacja kolejki (na pustą)
init(P-P).

% get(Elem, Kolejka, NowaKolejka) - pobranie
%get(Elem, [Elem|P]-K, P-K).
get(Elem, P-K, P1-K) :- \+ empty(P-K), P=[Elem|P1].

% put(Elem, Kolejka, NowaKolejka) - wstawienie
%put(Elem, P-K, P-K1) :- K=[Elem|K1].
put(Elem, P-[Elem|K1], P-K1).

% empty(Kolejka) - czy kolejka pusta
empty(P-K) :- P==K.

% init(Q), put(5,Q,Q1), put(6,Q1,Q2), get(X,Q2,Q3), write(X), put(7,Q3,Q4), put(8,Q4,Q5), get(Y,Q5,Q6), write(Y), get(Z,Q6,Q7), write(Z).


% wszerz( DrzewoBinarne, ListaWierzcholkowWszerz )
wszerz2(D, LW) :- init(K), put(D,K,K1), wszerz2_pom(K1, LW).

wszerz2_pom(K, []) :-
	empty(K), ! .

wszerz2_pom(K, LW) :-
	\+ empty(K),
	get(D,K,K1),
	D=nil,
	!,
	wszerz2_pom(K1, LW).

wszerz2_pom(K, NLW) :-
	\+ empty(K),
	get(D,K,K1),
	D=tree(L,W,P),
	!,
	put(L,K1,K2),
	put(P,K2,K3),
	NLW=[W|LW],
	wszerz2_pom(K3,LW).


wszerz3(D, LW) :- init(K), put(D,K,K1), wszerz3_pom(K1, LW).

wszerz3_pom(K, []) :-
	empty(K), ! .

wszerz3_pom(K, LW) :-
%	\+ empty(K),
	get(nil,K,K1),
	!,
	wszerz3_pom(K1, LW).

wszerz3_pom(K, [W|LW]) :-
%	\+ empty(K),
	get(tree(L,W,P),K,K1),
	!,
	put(L,K1,K2),
	put(P,K2,K3),
	wszerz3_pom(K3,LW).
