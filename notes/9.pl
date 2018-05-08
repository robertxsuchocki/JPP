dziecko(jasio, ewa, jan).
dziecko(stasio, ewa, jan).
dziecko(basia, anna, piotr).
dziecko(jan, ela, jakub).

ojciec(X, Y) :- dziecko(X, _, Y).

matka(X, Y) :- dziecko(X, Y, _).

rodzic(X, Y) :- ojciec(X, Y).
rodzic(X, Y) :- matka(X, Y).

dziadek(X, Y) :- rodzic(X, Z), rodzic(Z, Y).

wnuk(X, Y) :- dziadek(Y, X).

%przodek(X, Y) :- rodzic(X, Y).
%przodek(X, Y) :- przodek(X, Z), przodek(Z, Y).

przodek(X, Y) :- rodzic(X, Y).
przodek(X, Y) :- rodzic(X, Z), przodek(Z, Y).


nat(z).
nat(s(X)) :- nat(X).

plus(z, Z, Z).
plus(s(X), Y, s(Z)) :- plus(X, Y, Z).


lista([]).
lista([_|L]) :- lista(L).

pierwszy(E, [E|_]).

ostatni(E, [E]).
ostatni(E, [_|L]) :- ostatni(E, L).

element(E, [E|_]).
element(E, [_|L]) :- element(E, L).

costam(X, L) :- element(X, L), plus(X, s(z), s(s(s(z)))) % X e L, X + 1 = 3

fib(z, s(z)).
fib(s(z), s(z)).
fib(s(s(X)), W) :- fib(s(X), Y), fib(X, Z), plus(Y, Z, W).

scal([], L2, L2).
scal([X|L], L2, [X|W]) :- scal(L, L2, W).

