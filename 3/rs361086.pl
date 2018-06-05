% Robert Suchocki 361086

% Note: due to programming habits I've decided to use english names and comments
% and I've left polish names only for predicates that had been explicitly named
% in a task description, I hope it won't be taken as a flaw on code style
% This solution includes all requested predicates with all additional rules
% mentioned in task evaluation section

% choiceOfNode(A, B) - checks if B can be in a choice of graph containing A,
% nodes with label a must be identical, nodes with label e are accepted
% if B contains one from all of node names in A and 0 if A has no edges
choiseOfNode([Name, a | Rest], [Name, a | Rest]).
choiseOfNode([Name, e], [Name, e]).
choiseOfNode([Name, e, Next | _AEGraph], [Name, e, Next]).
choiseOfNode([Name, e, _Next | AEGraph], [Name, e, Node]) :-
  choiseOfNode([Name, e | AEGraph], [Name, e, Node]).

% matchNodes(O, N, V) - checks whether V can be a choice of any node in O
% and whether N is a graph O minus a node that V is a choice of,
% when building a graph choice with this predicate first rule will be successful
% for all choices of first found node and then negation in a second rule
% will stop computation, which at the end stops generating every permutation
% of this choice, which means that every graph choice permutation will succeed,
% but while building, every graph will be built, but only his first permutation
matchNodes([AENode | AEGraph], AEGraph, Node) :-
  choiseOfNode(AENode, Node).
matchNodes([AENode | AEGraph], [AENode | AENew], Node) :-
  \+ choiseOfNode(AENode, Node),
  matchNodes(AEGraph, AENew, Node).

% makeAChoice(AE, G) - checks if graph G is a choice of AEGraph w/o first nodes,
% it is done by matching the choices of nodes from G to original nodes in AE
% and removing both after successful match until both lists are empty,
% this predicate matches first node in G with any matching node in AE
makeAChoice([], []).
makeAChoice(AEGraph, [Node | Graph]) :-
  matchNodes(AEGraph, AENew, Node),
  makeAChoice(AENew, Graph).

% jestWyborem(AE, G) - checks if graph G is a choice of AEGraph,
% firstly checks that first nodes match (w.r.t. answer on forum) and after that
% tries matching the rest of pairs, disregarding nodes' order, with makeAChoise
jestWyborem([], []).
jestWyborem([AENode | AEGraph], [Node | Graph]) :-
  choiseOfNode(AENode, Node),
  makeAChoice(AEGraph, Graph).


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
% but take the first node from result list given to check that is not already
% in a produced list and use that node instead
takeFirstNew([], [New | _News], New).
takeFirstNew([Old | Olds], [Old | News], New) :-
  takeFirstNew(Olds, News, New).

% takeNextName(O, N, V) - checks if O contains V and whether N is equal to O
% without element V,
% method very similar to matchNodes, but doesn't require additional predicates
% in matching elements and just uses pattern match of strings,
% in algorithm with given list this predicate checks if name is in neighbourhood
% list of node and subtracts this name from list into N,
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


% walk(C, G, V, L1, L2, LR) - does the DFS exploration (or walkthrough)
% C is the "case" of DFS - ae means AE graph, g means normal graph
% G stands for graph, V stands for node, both with standard list structure
% L1, L2 are initial and final node lists, which means L1 has the list of nodes
% visited before current rule computation and after chained changes to this list
% as seen below L2 is the final list for this computation
% LR is final result list of entire DFS, it's there for performance reasons only
% as it is used in takeFirstNew predicate described above and requires full list
% to do it's work

% we just accept if there's nowhere to go
walk(_Case, _Graph, [_Name, _Type], List, List, _ListR).
% we also accept if every node on list has already been visited
walk(_Case, _Graph, [_Name, _Type, Next | Rest], List, List, _ListR) :-
  skipVisited([Next | Rest], List, []).
% here, for AE graphs, we accept if node choice leads to visited node
walk(ae, _Graph, [_CurrName, e, Next | Rest1], List, List, _ListR) :-
  takeNextName([Next | Rest1], _Rest2, NextName),
  member(NextName, List).
% this rule makes the move in a graph
walk(Case, Graph, [CurrName, Type, Next | Rest1], List1, List4, ListR) :-
  % at start, we reduce neighbourhood list by already visited nodes
  skipVisited([Next | Rest1], List1, Rest2),
  % choosing name, firstly we try to get next node from result list
  takeFirstNew(List1, ListR, NextName),
  % now we either check if node from result list is valid or choose next name
  takeNextName(Rest2, Rest3, NextName),
  % after retrieving name, let's get entire node
  findNode(Graph, NextName, Node),
  % now we can append neighbourhood list with next node
  append(List1, [NextName], List2),
  % and make a DFS walk for our node
  walk(Case, Graph, Node, List2, List3, ListR),
  % if we're not making a AE graph walk for "exists" node
  (   \+ (Case == ae, Type == e)
      % we can go to next nodes in list after coming back from just chosen one
  ->  walk(Case, Graph, [CurrName, Type | Rest3], List3, List4, ListR)
      % otherwise we stop here, as we went the only way possible, and do nothing
  ;   append(List3, [], List4)
  ).


% jestDFS(G, L) - checks if L is a DFS exploration list for graph G by
% computing standard case of walk
jestDFS([[Name | Rest] | Graph], List) :-
  walk(g, [[Name | Rest] | Graph], [Name | Rest], [Name], List, List).

% jestADFS(AEG, L) - checks if L is a DFS exploration list for some graph G,
% that is a choice from AE graph AEG, done by explicitly making a choice of G
% and then computing a standard DFS exploration, equivalent to jestADFS1
jestADFS(AEGraph, List) :-
  jestWyborem(AEGraph, Graph),
  jestDFS(Graph, List).

% jestADFS1(AEG, L) - checks if L is a DFS exploration list for some graph G,
% that is a choice from AE graph AEG, done by computing unusual DFS exploration
% which implicitly chooses one edge for every e node on the fly and then
% goes through it, ignoring all the others, equivalent to jestADFS
jestADFS1([[Name | Rest] | AEGraph], List) :-
  walk(ae, [[Name | Rest] | AEGraph], [Name | Rest], [Name], List, List).
