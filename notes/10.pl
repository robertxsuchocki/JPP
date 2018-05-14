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
