% Robert Suchocki 361086

:- use_module(library(lists)).


% choiceOfNode(A, B) - checks if B can be in a choice of graph containing A,
% nodes with label a must be identical, nodes with label e are accepted
% if B contains one from all of node names in A and 0 if A has no edges
choiseOfNode([Name, a | Rest], [Name, a | Rest]).
choiseOfNode([Name, e], [Name, e]).
choiseOfNode([Name, e, Next | _AEGraph], [Name, e, Next]).
choiseOfNode([Name, e, _Next | AEGraph], [Name, e, Node]) :-
  choiseOfNode([Name, e | AEGraph], [Name, e, Node]).

% matchNodes(O, N, V) - checks whether V can be a choice of any node in O
% and whether N is a graph O minus a node that V is a choice of
matchNodes([AENode | AEGraph], AEGraph, Node) :-
  choiseOfNode(AENode, Node).
matchNodes([AENode | AEGraph], [AENode | AENew], Node) :-
  \+ choiseOfNode(AENode, Node),
  matchNodes(AEGraph, AENew, Node).

% jestWyborem(AE, G) - checks if graph G is a choice of AEGraph,
% it is done by matching the choices of nodes from G to original nodes in AE
% and removing both after successful match until both lists are empty
jestWyborem([], []).
jestWyborem(AEGraph, [Node | Graph]) :-
  matchNodes(AEGraph, AENew, Node),
  jestWyborem(AENew, Graph).


% skipVisited(N, V, R) - checks if array N without elements from V equals to R
% is used to remove names of visited nodes from list of neighbourhoods of node
skipVisited([], _Visited, []).
skipVisited([Name | Rest], Visited, Result) :-
  member(Name, Visited),
  skipVisited(Rest, Visited, Result).
skipVisited([Name | Rest], Visited, [Name | Result]) :-
  \+ member(Name, Visited),
  skipVisited(Rest, Visited, Result).

% takeFirstNew(O, N, F) - checks if O is a prefix of N and if F is the first
% element of N that is not in the O,
% used for optimization of working with DFSs on given lists, algorithm won't
% generate all of the answers by checking every possibility for next node,
% but take the first node from list given to check that is not already
% in a produced list and use that node instead
takeFirstNew([], [New | _News], New).
takeFirstNew([Old | Olds], [Old | News], New) :-
  takeFirstNew(Olds, News, New).

% takeNextName(O, N, V) - checks if O contains V and whether N is equal to O
% without element V,
% method very similar to matchNodes, but doesn't require additional predicates
% in matching elements and just uses pattern match of strings,
% in algorithm with given list this predicate checks if name is in neighbourhood
% list of node and subtracts it from it into N,
% if list is not given then this predicate is responsible for extracting every
% possible node name from list for further moves and thus generates every path
takeNextName([Name | Names], Names, Name).
takeNextName([OtherName | OldNames], [OtherName | NewNames], Name) :-
  takeNextName(OldNames, NewNames, Name).

% findNode(G, N, V) - checks if G contains node V with name N,
% used for receiving full node after choosing name with 2 functions above
findNode([[Name | Rest] | _Graph], Name, [Name | Rest]).
findNode([_OtherNode | Graph], Name, Node) :-
  findNode(Graph, Name, Node).

% markVisited(O, N, V) - checks that V is not a member of O and that N is equal
% to O with V appended at the end of it
markVisited(List1, List2, Name) :-
  \+ member(Name, List1),
  append(List1, [Name], List2).

walkWithCheck(Case, Graph, [Name, Type, Next | Rest], List1, List3, ListR) :-
  markVisited(List1, List2, Name),
  walk(Case, Graph, [Name, Type, Next | Rest], List2, List3, ListR).
walkWithCheck(_Case, _Graph, [Name, _Type], List1, List2, _ListR) :-
  markVisited(List1, List2, Name).
walkWithCheck(_Case, _Graph, [Name | _Rest], List, List, _ListR) :-
  member(Name, List).

walk(_Case, _Graph, [_Name, _Type], List, List, _ListR).
walk(_Case, _Graph, [_Name, _Type, Next | Rest], List, List, _ListR) :-
  skipVisited([Next | Rest], List, []).
walk(Case, Graph, [CurrName, Type, Next | Rest1], List1, List3, ListR) :-
  \+ (Case == ae, Type == e),
  skipVisited([Next | Rest1], List1, Rest2),
  takeFirstNew(List1, ListR, NextName),
  takeNextName(Rest2, Rest3, NextName),
  findNode(Graph, NextName, Node),
  walkWithCheck(Case, Graph, Node, List1, List2, ListR),
  walk(Case, Graph, [CurrName, Type | Rest3], List2, List3, ListR).
walk(ae, _Graph, [_CurrName, e, Next | Rest1], List, List, _ListR) :-
  takeNextName([Next | Rest1], _Rest2, NextName),
  member(NextName, List).
walk(ae, Graph, [_CurrName, e, Next | Rest1], List1, List2, ListR) :-
  takeFirstNew(List1, ListR, NextName),
  takeNextName([Next | Rest1], Rest2, NextName),
  \+ member(NextName, List1),
  skipVisited(Rest2, List1, _Rest3),
  findNode(Graph, NextName, Node),
  walkWithCheck(ae, Graph, Node, List1, List2, ListR).


jestDFS([FirstNode | Graph], List) :-
  walkWithCheck(g, [FirstNode | Graph], FirstNode, [], List, List).


jestADFS(AEGraph, List) :-
  jestWyborem(AEGraph, Graph),
  jestDFS(Graph, List).


jestADFS1([FirstNode | AEGraph], List) :-
  walkWithCheck(ae, [FirstNode | AEGraph], FirstNode, [], List, List).
