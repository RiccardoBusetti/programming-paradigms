%%%%% Graph 1 %%%%
/*
edge(s,n1,6).
edge(s,n2,4).
edge(s,n3,3.1).
edge(n1,n3,2.4).
edge(n1,t,7).
edge(n2,n3,3.3).
edge(n2,n4,5).
edge(n3,n4,5.2).
edge(n3,n5,3.6).
edge(n4,n5,1).
edge(n4,t,2.2).
edge(n5,t,2).
*/


%%%% Graph 2 %%%%
/*
edge(a,b,4.2).
edge(a,c,1.2).
edge(a,d,3.0).
edge(a,e,2.2).
edge(b,c,3.5).
edge(b,d,2.0).
edge(b,e,1.7).
edge(c,d,3.4).
edge(c,e,2.2).
edge(d,e,3.2).
*/

%%%% Custom Graph %%%%
edge(a,b,5).
edge(b,c,6).
edge(c,a,10).


%%%% Rules declaration %%%%
connection(N1,N2,Cost) :-
	edge(N1,N2,Cost),\+(N1=N2).
connection(N1,N2,Cost) :- 
	edge(N2,N1,Cost),\+(N1=N2).

incomplete_node_list(L,N):-
	connection(N,_,_),
	\+(member(N,L)).

graph_nodes(Acc,L):-
	incomplete_node_list(Acc,N),!,
	graph_nodes([N|Acc],L).

graph_nodes(L,L).

graph_nodes(L) :- 
	graph_nodes([],L),!.

graph_nodes(L1):-
	graph_nodes([],L2),
	sort(L1,S),
	sort(L2,S).


%%%% 1 %%%%
complete() :-
	graph_nodes(Nodes),
	complete(Nodes,Nodes).
	
complete([],_).	
complete([CurrentNode|NextNodes],Nodes) :-
	directly_connected_to_all(CurrentNode,Nodes),
	complete(NextNodes,Nodes).

directly_connected_to_all(_,[]).
directly_connected_to_all(Node,[OtherNode|Nodes]) :-
	Node=OtherNode,
	directly_connected_to_all(Node, Nodes),
	!.
directly_connected_to_all(Node,[OtherNode|Nodes]) :-
	\+(Node=OtherNode),
	connection(Node,OtherNode,_),
	directly_connected_to_all(Node,Nodes),
	!.

	
%%%% 2 %%%%
path(Source,Destination,Path,Cost) :-
	dfs(Source,Destination,Path,[Source],Cost).

dfs(Destination,Destination,Visited,Visited,0).
dfs(Current,Destination,Path,Visited,Cost) :-
	connection(Current,Next,ToNextCost),
	\+(member(Next,Visited)),
	append(Visited,[Next],NewVisited),
	dfs(Next,Destination,Path,NewVisited,PartialCost),
	Cost is ToNextCost + PartialCost.

	
%%%% 3 %%%%
min_traversal(Source,Destination,Path,Cost,NodesNumber) :-
	path(Source,Destination,Path,Cost),
	length(Path,NodesNumber),
    \+(shorter_path(Source,Destination,NodesNumber)).
	
shorter_path(Source,Destination,NodesNumber) :-
	path(Source,Destination,OtherPath,_),
	length(OtherPath,OtherNodesNumber),
	OtherNodesNumber < NodesNumber.
	
	
%%%% 4 %%%%
connected() :-
	graph_nodes(Nodes),
	connected(Nodes,Nodes).

connected([],_).	
connected([CurrentNode|NextNodes],Nodes) :-
	path_to_all(CurrentNode,Nodes),
	connected(NextNodes,Nodes).
	
path_to_all(_,[]).
path_to_all(Node,[OtherNode|Nodes]) :-
	Node=OtherNode,
	path_to_all(Node,Nodes),
	!.
path_to_all(Node,[OtherNode|Nodes]) :-
	\+(Node=OtherNode),
	path(Node,OtherNode,_,_),
	path_to_all(Node,Nodes),
	!.
	
	
%%%% 5 %%%%
hcycle(Cycle,Cost) :-
	graph_nodes(Nodes),
	length(Nodes,NodesNumber),
	first(Nodes,FirstNode),
	hamiltonian_cycle(NodesNumber,FirstNode,[FirstNode],Cycle,Cost).
	
hamiltonian_cycle(1,Current,[Destination|OtherVisited],[Destination|OtherVisited],ToNextCost) :-
	connection(Current,Destination,ToNextCost).
hamiltonian_cycle(Remaining,Current,Visited,Cycle,Cost) :-
	connection(Current,Next,ToNextCost),
	\+(member(Next,Visited)),
	append(Visited,[Next],NewVisited),
	NewRemaining is Remaining - 1,
	hamiltonian_cycle(NewRemaining,Next,NewVisited,Cycle,PartialCost),
	Cost is ToNextCost + PartialCost.


%%%% 6 %%%%
shcycle(Cycle,Cost) :-
	hcycle(Cycle,Cost),
	\+(shorter_hcycle(Cost)).

shorter_hcycle(Cost) :-
	hcycle(_,OtherCost),
	OtherCost < Cost.	
	
%%%% Utilities %%%%
first([Head|_],Head).