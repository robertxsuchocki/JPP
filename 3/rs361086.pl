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


skipVisited([], List, []).
skipVisited([Name | Rest], List, Result) :-
  member(Name, List),
  skipVisited(Rest, List, Result).
skipVisited([Name | Rest], List, [Name | Result]) :-
  \+ member(Name, List),
  skipVisited(Rest, List, Result).

takeFirstNew([], [New | News], New).
takeFirstNew([Old | Olds], [Old | News], New) :-
  takeFirstNew(Olds, News, New).

takeNextName([Name | Names], Names, Name).
takeNextName([OtherName | Names], [OtherName | NewNames], Name) :-
  takeNextName(Names, NewNames, Name).

findNode([[Name | Rest] | Graph], Name, [Name | Rest]).
findNode([OtherNode | Graph], Name, Node) :-
  findNode(Graph, Name, Node).

markVisited(Name, List1, List2) :-
  \+ member(Name, List1),
  append(List1, [Name], List2).

walkWithCheck(Case, Graph, [Name, Type, Next | Rest], List1, List3, ListL) :-
  markVisited(Name, List1, List2),
  walk(Case, Graph, [Name, Type, Next | Rest], List2, List3, ListL).
walkWithCheck(Case, Graph, [Name, Type], List1, List2, ListL) :-
  markVisited(Name, List1, List2).
walkWithCheck(Case, Graph, [Name | Rest], List, List, ListL) :-
  member(Name, List).

walk(Case, Graph, [Name, Type], List, List, ListL).
walk(Case, Graph, [Name, Type, Next | Rest], List, List, ListL) :-
  skipVisited([Next | Rest], List, []).
walk(Case, Graph, [CurrName, Type, Next | Rest1], List1, List3, ListL) :-
  \+ (Case == ae, Type == e),
  skipVisited([Next | Rest1], List1, Rest2),
  takeFirstNew(List1, ListL, NextName),
  takeNextName(Rest2, Rest3, NextName),
  findNode(Graph, NextName, Node),
  walkWithCheck(Case, Graph, Node, List1, List2, ListL),
  walk(Case, Graph, [CurrName, Type | Rest3], List2, List3, ListL).
walk(ae, Graph, [CurrName, e, Next | Rest1], List, List, ListL) :-
  takeNextName([Next | Rest1], Rest2, NextName),
  member(NextName, List).
walk(ae, Graph, [CurrName, e, Next | Rest1], List1, List2, ListL) :-
  takeFirstNew(List1, ListL, NextName),
  takeNextName([Next | Rest1], Rest2, NextName),
  \+ member(NextName, List1),
  skipVisited(Rest2, List1, Rest3),
  findNode(Graph, NextName, Node),
  walkWithCheck(ae, Graph, Node, List1, List2, ListL).


jestDFS([FirstNode | Graph], List) :-
  walkWithCheck(g, [FirstNode | Graph], FirstNode, [], List, List).


jestADFS(AEGraph, List) :-
  jestWyborem(AEGraph, Graph),
  jestDFS(Graph, List).


jestADFS1([FirstNode | AEGraph], List) :-
  walkWithCheck(ae, [FirstNode | AEGraph], FirstNode, [], List, List).
