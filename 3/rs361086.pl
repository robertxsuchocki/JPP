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

walkWithCheck(Case, Graph, [Name, Type, Next | Rest], Visited1, Visited3) :-
  markVisited(Name, Visited1, Visited2),
  walk(Case, Graph, [Name, Type, Next | Rest], Visited2, Visited3).
walkWithCheck(Case, Graph, [Name, Type], Visited1, Visited2) :-
  markVisited(Name, Visited1, Visited2).
walkWithCheck(Case, Graph, [Name | Rest], Visited, Visited) :-
  member(Name, Visited).

walk(Case, Graph, [Name, Type], Visited, Visited).
walk(Case, Graph, [Name, Type, Next | Rest], Visited, Visited) :-
  skipVisited([Next | Rest], Visited, []).
walk(Case, Graph, [CurrName, Type, Next | Rest1], Visited1, Visited3) :-
  (Case == g; Type == a), !,
  skipVisited([Next | Rest1], Visited1, Rest2),
  takeAnyName(Rest2, Rest3, NextName),
  findNode(Graph, NextName, Node),
  walkWithCheck(Case, Graph, Node, Visited1, Visited2),
  walk(Case, Graph, [CurrName, Type | Rest3], Visited2, Visited3).
walk(ae, Graph, [CurrName, e, Next | Rest1], Visited, Visited) :-
  takeAnyName([Next | Rest1], Rest2, NextName),
  member(NextName, Visited).
walk(ae, Graph, [CurrName, e, Next | Rest1], Visited1, Visited2) :-
  takeAnyName([Next | Rest1], Rest2, NextName),
  \+ member(NextName, Visited1),
  skipVisited(Rest2, Visited1, Rest3),
  findNode(Graph, NextName, Node),
  walkWithCheck(ae, Graph, Node, Visited1, Visited2).


jestDFS([FirstNode | Graph], List) :-
  walkWithCheck(g, [FirstNode | Graph], FirstNode, [], List).


jestADFS(AEGraph, List) :-
  jestWyborem(AEGraph, Graph),
  jestDFS(Graph, List).


jestADFS1([FirstNode | AEGraph], List) :-
  walkWithCheck(ae, [FirstNode | AEGraph], FirstNode, [], List).
