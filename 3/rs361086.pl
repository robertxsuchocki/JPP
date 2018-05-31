% Robert Suchocki 361086

:- use_module(library(lists)).

wyborWierzcholka([V, T], [V, T]).

wyborWierzcholka([V, a | Rest], [V, a | Rest]).

wyborWierzcholka([V, e, R | RestAE], [V, e, R]).
wyborWierzcholka([V, e, R | RestAE], [V, e, S]) :-
  wyborWierzcholka([V, e | RestAE], [V, e, S]).

jestWyborem([], []).
jestWyborem(AEgraf, Graf).

jestDFS(Graf, Lista).

jestADFS(AEgraf, Lista) :- jestWyborem(AEgraf, Graf), jestDFS(Graf, Lista).

jestADFS1(AEgraf, Lista).
