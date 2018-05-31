intersect(Z1, Z2) :- element(X, Z1), element(X, Z2).

--podlista(P, L) :- append(_, P, Z), append(Z, _, L)

podlista2(P, L) :- append(Z, _, L), append(_, P, Z)

podlista3([], _).
podlista3([E|P], L) :- append(Z, _, L), append(_, [E|P], Z).

podlista4([], _).
podlista4([E|P], [E|L]) :- append(P, _, L).
podlista4([E|P], [_|L]) :- podlista([E|P], L).

suma([], 0).
suma([E|L], A2) :- suma(L, A1), A2 is A1+E

sumaa(L, W) :- sumaa(L, 0, W).
sumaa([], A, A).
sumaa([E|L], A, W) :- A1 is A+E, suma(L, A1, W).

mini(A, B, A) :- A =< B.
mini(A, B, B) :- A > B.

mini2(A, B, Z) :- A =< B, ! , Z=A.
mini2(A, B, B).

mini3(A, B, A) :- A =< B, !.
mini3(A, B, B) :- A > B.

maxi(A, B, Z) :- A =< B -> Z=B ; Z=A.

zrobListe(0, []) :- !.
zrobListe(A, [A|L]) :- B is A-1, zrobListe(B, L).

zrobliste(0, EL, K, W) :- write(EL), write(' '), write(W), nl, K=W, !.
zrobliste(A, EL, [A|L], W) :- B is A-1, write(EL), write(' '), write(W), nl, zrobliste(B, EL, L, [A|W]).

zrobliste(A, L) :- zrobliste(A, L, L, []).

dlugosc(L, X) :- dlugosc(L, 0, X).
dlugosc([], A, A).
dlugosc([E|L], A, B) :- C is A+1, dlugosc(L, C, B).

min([E|L], M) :- szukaj(L, E, M).
szukaj([], M, M).
szukaj([E1|L], E2, M) :- E1>=E2, !, szukaj(L, E2, M).
szukaj([E1|L], E2, M) :- E1<E2, szukaj(L, E1, M).

odwroc(L, R) :- odwroc(L, [], R).
odwroc([], R, R).
odwroc([E|L], A, R) :- odwroc(L, [E|A], R).

odwroc2(L, R) :- rownaDlugosc(L, R), odwroc(L, [], R).

rownaDlugosc([], []).
rownaDlugosc([_|L1], [_|L2]) :- rownaDlugosc(L1, L2).

palindrom(L) :- odwroc(L, L).

slowo(S) :- slowo(S, []).
slowo([a|S], A) :- slowo(S, [b|A]).
slowo(S, S).

fp5(L, F) :- fp5(L, C, F, C).
fp5([b|L], C, [b|F], R) :- fp5(L, C, F, R).
fp5([c|L], [c|C], F, R) :- fp5(L, C, F, R).


qsort([], []).
qsort([X | L], S) :-
      partition(L, X, M, W),
      qsort(M, SM),
      qsort(W, SW),
      append(SM, [X|SW], S).   % a co gdyby bylo: append(SM, SW, S) ?


partition([], _, [], []).
partition([X | L ], E, [X | Lm], Lw) :-  X =< E, !, partition(L, E, Lm, Lw).
partition([X | L ], E, Lm, [X | Lw]) :-  partition(L, E, Lm, Lw).


qsortA(L, S) :-  qsortA(L, [], S).

qsortA([], S, S).
qsortA([X | L], Akum, Sort) :-
      partition(L, X, L1, L2),
      qsortA(L2, Akum, S2),
      qsortA(L1, [X | S2], Sort).


qsortLog(L, S) :- qsortLog(L, [], S).

qsortLog([], S, S).
qsortLog([H | T], A, S) :-
     partition(T, H, M, W, 0, R),
     qsortLogPom(H, A, M, W, R, S).

qsortLogPom(H, A, Lm, Lw, R, S) :-
    R >= 0, !, qsortLog(Lw, A, SLw), qsortLog(Lm, [H | SLw], S).
qsortLogPom(H, A, Lm, Lw, R, S) :-
    R < 0, qsortLog(Lm, [H | SLw], S), qsortLog(Lw, A, SLw).


partition([], _, [], [], S, S).
partition([A | L], X, [A | M], D, S, W) :-
     A =< X, !, NS is S+1,
     partition(L, X, M, D, NS, W).
partition([A | L], X, M, [A | D], S, W):-
     A > X, NS is S-1,
     partition(L, X, M, D, NS, W).
