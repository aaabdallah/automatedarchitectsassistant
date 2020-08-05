/*
:- compile(library(basics)).
:- compile(library(sets)).
*/
/*******************************************************************/
/* Topological relations (graph constraints). */
/*
- undirect or directed
- *tree, *connected graph, *strongly connected directed graph,
	forest, unconnected graph, star, ring
(NodeList, EdgeList)
[a,b,c,d][[a,b],[a,c],[c,d]]
*/

/* Checks for a fully connected undirected graph */
ufullyconnected([],[]).
ufullyconnected([_],[]).
ufullyconnected(NodeList,EdgeList) :-
	length(NodeList, NumNodes), NumNodes >= 2,
	is_set(NodeList),
	length(EdgeList, NumEdges),
	NumEdges is (NumNodes * (NumNodes - 1)) // 2,
	checkufc(NodeList,EdgeList,NumNodes).

checkufc([], _, _).
checkufc([HN|RestNodes], EdgeList, NumNodes) :-
	findall(X, (member([X,HN],EdgeList);member([HN,X],EdgeList)), AllX),
	length(AllX, ConnectedNodes), NumNodes is ConnectedNodes + 1,
	checkufc(RestNodes, EdgeList, NumNodes).

/* Checks for an undirected ring */
uring([], []).
uring([_], []).
uring([A,B],[[A,B]]).
uring([A,B],[[B,A]]).
uring([HN|RestNodes],EdgeList) :-
	length([HN|RestNodes], NumNodes), NumNodes >= 3,
	is_set([HN|RestNodes]),
	length(EdgeList, NumEdges), NumNodes is NumEdges,
	checkring([HN|RestNodes], EdgeList).

checkring([], _).
checkring([HN|RestNodes], EdgeList) :-
	findall(X, (member([X,HN],EdgeList);member([HN,X],EdgeList)), AllX),
	length(AllX, 2),
	checkring(RestNodes, EdgeList).

/* Checks for an undirected star */
ustar([], []).
ustar([_], []).
ustar(NodeList,EdgeList) :-
	length(NodeList, NumNodes), NumNodes >= 2,
	utree(NodeList,EdgeList),
	member(Center,NodeList),
	subtract(NodeList, [Center], RestNodes),
	findall(X,(member(X,RestNodes),
	(member([Center,X],EdgeList);member([X,Center],EdgeList))), OuterNodes),
	seteq(RestNodes,OuterNodes).

/* Checks for a directed star */
dstar([], []).
dstar([_], []).
dstar([Center|RestNodes],EdgeList) :-
	length([Center|RestNodes], NumNodes), NumNodes >= 2,
	dtree([Center|RestNodes],EdgeList),
	findall(X, member([Center,X],EdgeList), OuterNodes),
	seteq(RestNodes,OuterNodes).

/* Checks for an undirected (free) tree */
utree([], []).
utree([_], []).
utree([HN|RestNodes],EdgeList) :-
	length([HN|RestNodes], NumNodes), NumNodes >= 2,
	is_set([HN|RestNodes]),
	findall(X, uconnected(HN,X,EdgeList), ConnectedNodes),
	seteq([HN|RestNodes],ConnectedNodes),
	length(EdgeList, NumEdges),
	NumEdges is NumNodes - 1.

/* Checks for a directed tree (assumes known root) */
dtree([], []).
dtree([_], []).
dtree([Root|RestNodes],EdgeList) :-
	length([Root|RestNodes], NumNodes), NumNodes >= 2,
	is_set([Root|RestNodes]),
	findall(X, dconnected(Root,X,EdgeList), ConnectedNodes),
	seteq([Root|RestNodes],ConnectedNodes),
	length(EdgeList, NumEdges),
	NumEdges is NumNodes - 1.

/* Checks for a connected undirected graph (may contain cycles) */
cugraph([], []).
cugraph([_], []).
cugraph([HN|RestNodes],EdgeList) :-
	length([HN|RestNodes], NumNodes), NumNodes >= 2,
	is_set([HN|RestNodes]),
	findall(X, uconnected(HN,X,EdgeList), ConnectedNodes),
	seteq([HN|RestNodes],ConnectedNodes).

/* Checks for a all-connected-to-a-single-node ('root') directed graph
   (assumes known root, may contain cycles) */
cdgraph([], []).
cdgraph([_], []).
cdgraph([Root|RestNodes],EdgeList) :-
	length([Root|RestNodes], NumNodes), NumNodes >= 2,
	is_set([Root|RestNodes]),
	findall(X, dconnected(Root,X,EdgeList), ConnectedNodes),
	seteq([Root|RestNodes],ConnectedNodes).

/* Check if two nodes are connected in an undirected graph */
uconnected(X,Y,EdgeList) :- uconnected(X,Y,EdgeList,[X]).

uconnected(X,X,_,_).
uconnected(X,Y,EdgeList,Visited) :-
	(member([X,N], EdgeList);member([N,X], EdgeList)),
	nonmember(N, Visited), uconnected(N, Y, EdgeList, [N | Visited]).

/* Check if two nodes are connected in a directed graph */
dconnected(X,Y,EdgeList) :- dconnected(X,Y,EdgeList,[X]).

dconnected(X,X,_,_).
dconnected(X,Y,EdgeList,Visited) :-
	member([X,N], EdgeList),
	nonmember(N, Visited), dconnected(N, Y, EdgeList, [N | Visited]).

/* Check if a list of edges meets the constraints of a list of layers */
layered([],_).
layered([[A,B]|OtherEdges], Layers) :-
	inlayers([A,B], Layers),
	layered(OtherEdges, Layers).
layered([[A,B]|OtherEdges], Layers) :-
	unaffectedbylayers([A,B], Layers),
	layered(OtherEdges, Layers).

/* Check if an edge is in at least one of a list of layers */
inlayers(_, []) :- !, fail.
inlayers(Edge, [Layer | _]) :-
	inlayer(Edge, Layer), !.
inlayers(Edge, [_ | OtherLayers]) :-
	inlayers(Edge, OtherLayers), !.

/* Check if an edge is in a layer */
inlayer([A,B], [TopList, BottomList]) :-
	member(A, TopList), member(B, BottomList), !.
inlayer([A,B], [TopList, BottomList]) :-
	member(A, BottomList), member(B, TopList), !.

/* Check if an edge is not affected by all layers in a list */
unaffectedbylayers(_, []) :- !.
unaffectedbylayers(Edge, [Layer | OtherLayers]) :-
	unaffectedbylayer(Edge, Layer), !,
	unaffectedbylayers(Edge, OtherLayers), !.

/* Check if an edge is not affected by a layer */
unaffectedbylayer(_,[]) :- !.
unaffectedbylayer([A,B], [_,BottomList]) :-
	nonmember(A,BottomList), nonmember(B,BottomList), !.
