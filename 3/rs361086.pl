% Robert Suchocki 361086

:- use_module(library(lists)).


choiseOfNode([Name, a | Rest], [Name, a | Rest]).
choiseOfNode([Name, e], [Name, e]).
choiseOfNode([Name, e, Next | AEGraph], [Name, e, Next]).
choiseOfNode([Name, e, Next | AEGraph], [Name, e, Node]) :-
  choiseOfNode([Name, e | AEGraph], [Name, e, Node]).

matchNodes([AENode | AEGraph], AEGraph, Node) :-
  choiseOfNode(AENode, Node).
matchNodes([AENode | AEGraph], [AENode | AENew], Node) :-
  \+ choiseOfNode(AENode, Node),
  matchNodes(AEGraph, AENew, Node).

jestWyborem([], []).
jestWyborem(AEGraph, [Node | Graph]) :-
  matchNodes(AEGraph, AENew, Node),
  jestWyborem(AENew, Graph).


skipVisited([], Visited, []).
skipVisited([Name | Rest], Visited, Result) :-
  member(Name, Visited),
  skipVisited(Rest, Visited, Result).
skipVisited([Name | Rest], Visited, [Name | Result]) :-
  \+ member(Name, Visited),
  skipVisited(Rest, Visited, Result).

takeAnyName([Name | Names], Names, Name).
takeAnyName([OtherName | Names], [OtherName | NewNames], Name) :-
  takeAnyName(Names, NewNames, Name).

findNode([[Name | Rest] | Graph], Name, [Name | Rest]).
findNode([OtherNode | Graph], Name, Node) :-
  findNode(Graph, Name, Node).

markVisited(Name, Visited1, Visited2) :-
  \+ member(Name, Visited1),
  append(Visited1, [Name], Visited2).

walkWithCheck(Graph, [Name, Type, Next | Rest], Visited1, Visited3) :-
  markVisited(Name, Visited1, Visited2),
  walk(Graph, [Name, Type, Next | Rest], Visited2, Visited3).
walkWithCheck(Graph, [Name, Type], Visited1, Visited2) :-
  markVisited(Name, Visited1, Visited2).
walkWithCheck(Graph, [Name | Rest], Visited, Visited) :-
  member(Name, Visited).

walk(Graph, [Name, Type, Next | Rest1], Visited, Visited) :-
  skipVisited([Next | Rest1], Visited, []).
walk(Graph, [CurrName, Type, Next | Rest1], Visited1, Visited3) :-
  skipVisited([Next | Rest1], Visited1, Rest2),
  takeAnyName(Rest2, Rest3, NextName),
  findNode(Graph, NextName, Node),
  walkWithCheck(Graph, Node, Visited1, Visited2),
  walk(Graph, [CurrName, Type | Rest3], Visited2, Visited3).
walk(Graph, [Name, Type], Visited, Visited).

jestDFS([FirstNode | Graph], List) :-
  walkWithCheck([FirstNode | Graph], FirstNode, [], List).


jestADFS(AEGraph, List) :-
  jestWyborem(AEGraph, Graph),
  jestDFS(Graph, List).


walkAEWithCheck(Graph, [Name, Type, Next | Rest], Visited, Visited2) :-
  markVisited(Name, Visited, Visited1),
  walkAE(Graph, [Name, Type, Next | Rest], Visited1, Visited2).
walkAEWithCheck(Graph, [Name, Type], Visited, Visited1) :-
  markVisited(Name, Visited, Visited1).
walkAEWithCheck(Graph, [Name | Rest], Visited, Visited) :-
  member(Name, Visited).

walkAE(Graph, [Name, Type, Next | Rest], Visited, Visited) :-
  skipVisited([Next | Rest], Visited, []).
walkAE(Graph, [CurrName, a, Next | Rest], Visited, Visited2) :-
  skipVisited([Next | Rest], Visited, Rest1),
  takeAnyName(Rest1, Rest2, NextName),
  findNode(Graph, NextName, Node),
  walkAEWithCheck(Graph, Node, Visited, Visited1),
  walkAE(Graph, [CurrName, a | Rest2], Visited1, Visited2).
walkAE(Graph, [CurrName, e, Next | Rest], Visited, Visited1) :-
  skipVisited([Next | Rest], Visited, Rest1),
  takeAnyName(Rest1, Rest2, NextName),
  findNode(Graph, NextName, Node),
  walkAEWithCheck(Graph, Node, Visited, Visited1).
walkAE(Graph, [Name, Type], Visited, Visited).

jestADFS1([FirstNode | AEGraph], List) :-
  walkAEWithCheck([FirstNode | AEGraph], FirstNode, [], List).
