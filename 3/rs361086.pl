% Robert Suchocki 361086

:- use_module(library(lists)).


wyborWierzcholka([V, a | Reszta], [V, a | Reszta]).
wyborWierzcholka([V, e], [V, e]).
wyborWierzcholka([V, e, _R | _AEGraf], [V, e, _R]).
wyborWierzcholka([V, e, _R | AEGraf], [V, e, S]) :-
  wyborWierzcholka([V, e | AEGraf], [V, e, S]).

usunWierzcholek([AEWierz | AEGraf], AEGraf, Wierz) :-
  wyborWierzcholka(AEWierz, Wierz).
usunWierzcholek([AEWierz | AEGraf], [AEWierz | AENowy], Wierz) :-
  \+ wyborWierzcholka(AEWierz, Wierz),
  usunWierzcholek(AEGraf, AENowy, Wierz).

jestWyborem([], []).
jestWyborem(AEGraf, [Wierz | Graf]) :-
  usunWierzcholek(AEGraf, AENowy, Wierz),
  jestWyborem(AENowy, Graf).


pominOdwiedzone([], _Visited, []).
pominOdwiedzone([Nazwa | Reszta], Visited, Wynik) :-
  member(Nazwa, Visited),
  pominOdwiedzone(Reszta, Visited, Wynik).
pominOdwiedzone([Nazwa | Reszta], Visited, [Nazwa | Wynik]) :-
  \+ member(Nazwa, Visited),
  pominOdwiedzone(Reszta, Visited, Wynik).

wezDowolnaNazwe([Nazwa | Nazwy], Nazwy, Nazwa).
wezDowolnaNazwe([Nazwa1 | Nazwy1], [Nazwa1 | Nazwy2], Nazwa2) :-
  wezDowolnaNazwe(Nazwy1, Nazwy2, Nazwa2).

znajdzWierzcholek([[Nazwa | Reszta] | _Graf], Nazwa, [Nazwa | Reszta]).
znajdzWierzcholek([_G | Graf], Nazwa, Wierz) :-
  znajdzWierzcholek(Graf, Nazwa, Wierz).

check(Name, Visited1, Visited2) :-
  \+ member(Name, Visited1),
  append(Visited1, [Name], Visited2).

walkWithCheck(Graf, [V, T, R | Reszta], Visited, Visited2) :-
  check(V, Visited, Visited1),
  walk(Graf, [V, T, R | Reszta], Visited1, Visited2).
walkWithCheck(_Graf, [V, _T], Visited, Visited1) :-
  check(V, Visited, Visited1).
walkWithCheck(_Graf, [V | _Reszta], Visited, Visited) :-
  member(V, Visited).

walk(_Graf, [_V, _T, R | Reszta], Visited, Visited) :-
  pominOdwiedzone([R | Reszta], Visited, []).
walk(Graf, [V, T, R | Reszta], Visited, Visited2) :-
  pominOdwiedzone([R | Reszta], Visited, Reszta1),
  wezDowolnaNazwe(Reszta1, Reszta2, Nazwa),
  znajdzWierzcholek(Graf, Nazwa, Wierz),
  walkWithCheck(Graf, Wierz, Visited, Visited1),
  walk(Graf, [V, T | Reszta2], Visited1, Visited2).
walk(_Graf, [_V, _T], Visited, Visited).

jestDFS([G | Graf], Lista) :-
  walkWithCheck([G | Graf], G, [], Lista).


jestADFS(AEgraf, Lista) :-
  jestWyborem(AEgraf, Graf),
  jestDFS(Graf, Lista).


walkAEWithCheck(Graf, [V, T, R | Reszta], Visited, Visited2) :-
  check(V, Visited, Visited1),
  walkAE(Graf, [V, T, R | Reszta], Visited1, Visited2).
walkAEWithCheck(_Graf, [V, _T], Visited, Visited1) :-
  check(V, Visited, Visited1).
walkAEWithCheck(_Graf, [V | _Reszta], Visited, Visited) :-
  member(V, Visited).

walkAE(_Graf, [_V, _T, R | Reszta], Visited, Visited) :-
  pominOdwiedzone([R | Reszta], Visited, []).
walkAE(Graf, [V, a, R | Reszta], Visited, Visited2) :-
  pominOdwiedzone([R | Reszta], Visited, Reszta1),
  wezDowolnaNazwe(Reszta1, Reszta2, Nazwa),
  znajdzWierzcholek(Graf, Nazwa, Wierz),
  walkAEWithCheck(Graf, Wierz, Visited, Visited1),
  walkAE(Graf, [V, a | Reszta2], Visited1, Visited2).
walkAE(Graf, [_V, e, R | Reszta], Visited, Visited1) :-
  pominOdwiedzone([R | Reszta], Visited, Reszta1),
  wezDowolnaNazwe(Reszta1, _Reszta2, Nazwa),
  znajdzWierzcholek(Graf, Nazwa, Wierz),
  walkAEWithCheck(Graf, Wierz, Visited, Visited1).
walkAE(_Graf, [_V, _T], Visited, Visited).

jestADFS1([AE | AEgraf], Lista) :-
  walkAEWithCheck([AE | AEgraf], AE, [], Lista).
