% Robert Suchocki 361086

:- use_module(library(lists)).


% wyborWierzcholka - przeksztalca wierzcholek AEgrafu na wierzcholek,
% ktory moze nalezec do wyboru
wyborWierzcholka([V, a | Reszta], [V, a | Reszta]).

wyborWierzcholka([V, e], [V, e]).
wyborWierzcholka([V, e, _R | _AEGraf], [V, e, _R]).
wyborWierzcholka([V, e, _R | AEGraf], [V, e, S]) :-
  wyborWierzcholka([V, e | AEGraf], [V, e, S]).

usunWierzcholek([AEWierz | AEGraf], AEGraf, Wierz) :-
  wyborWierzcholka(AEWierz, Wierz).
usunWierzcholek([AEWierz | AEGraf], [AEWierz | AENowy], Wierz) :-
  \+wyborWierzcholka(AEWierz, Wierz), usunWierzcholek(AEGraf, AENowy, Wierz).

jestWyborem([], []).
jestWyborem(AEGraf, [Wierz | Graf]) :-
  usunWierzcholek(AEGraf, AENowy, Wierz), jestWyborem(AENowy, Graf).



wezDowolnaNazwe([Nazwa | Nazwy], Nazwy, Nazwa).
wezDowolnaNazwe([Nazwa1 | Nazwy1], [Nazwa1 | Nazwy2], Nazwa2) :-
  wezDowolnaNazwe(Nazwy1, Nazwy2, Nazwa2).

znajdzWierzcholek([[Nazwa | Reszta] | _Graf], Nazwa, [Nazwa | Reszta]).
znajdzWierzcholek([_G | Graf], Nazwa, Wierz) :-
  znajdzWierzcholek(Graf, Nazwa, Wierz).

check(Name, Visited1, Visited2) :-
  nonmember(Name, Visited1),
  append(Visited1, [Name], Visited2).

walkWithCheck(Graf, [V, T | Reszta], Visited, Visited2) :-
  check(V, Visited, Visited1),
  walk(Graf, [V, T | Reszta], Visited1, Visited2).
walkWithCheck(_Graf, [V, _T], Visited, Visited1) :-
  check(V, Visited, Visited1).
walkWithCheck(_Graf, [V | _], Visited, Visited) :- member(V, Visited).

walk(Graf, [V, T | Reszta], Visited, Visited2) :-
  wezDowolnaNazwe(Reszta, Reszta2, Nazwa),
  znajdzWierzcholek(Graf, Nazwa, Wierz),
  walkWithCheck(Graf, Wierz, Visited, Visited1),
  walk(Graf, [V, T | Reszta2], Visited1, Visited2).
walk(_Graf, [_V, _T], Visited, Visited).

jestDFS([G | Graf], Lista) :-
  walkWithCheck([G | Graf], G, [], Lista).



jestADFS(AEgraf, Lista) :- jestWyborem(AEgraf, Graf), jestDFS(Graf, Lista).



jestADFS1(_AEgraf, _Lista).
