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

