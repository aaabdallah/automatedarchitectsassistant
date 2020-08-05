/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(distributedprocesses, 'Distributed Processes').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(datastructure(distributedprocesses), Name) :-
	indatabase([Name]),
	validentity(datacomponent(base), Name).

validentity(socket(distributedprocesses), Name) :-
	indatabase([Name]),
	validentity(port(base), Name).

validentity(process(distributedprocesses), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name),

	getvalues(process, _, Name, [ports, locals], [Ports, Locals]),

	checkfortypemismatch(Name, Ports, socket(distributedprocesses),
	'Process ports include a non-socket in Distributed Processes style'),
	checkfortypemismatch(Name, Locals, datastructure(distributedprocesses),
	'Process locals include a non-datastructure in Distributed Processes style').

validentity(circuit(distributedprocesses), Name) :-
	indatabase([Name]),
	validentity(dataconnector(base), Name),

	getvalues(circuit, _, Name,
	[c1, c2, p1, p2], [[C1], [C2], [P1], [P2]]),

	checkfortypemismatch(Name, C1, process(distributedprocesses),
	'Circuit has c1 which is a non-process in Distributed Processes style'),
	checkfortypemismatch(Name, C2, process(distributedprocesses),
	'Circuit has c2 which is a non-process in Distributed Processes style'),
	checkfortypemismatch(Name, P1, socket(distributedprocesses),
	'Circuit has p1 which is a non-socket in Distributed Processes style'),
	checkfortypemismatch(Name, P2, socket(distributedprocesses),
	'Circuit has p2 which is a non-socket in Distributed Processes style').

validentity(processcall(distributedprocesses), Name) :-
	indatabase([Name]),
	validentity(controlconnector(base), Name),

	getvalues(processcall, _, Name, [c1, c2], [[C1], [C2]]),

	checkfortypemismatch(Name, C1, process(distributedprocesses),
	'Process call has c1 which is a non-process in Distributed Processes style'),
	checkfortypemismatch(Name, C2, process(distributedprocesses),
	'Process call has c2 which is a non-process in Distributed Processes style').

validentity(processspawn(distributedprocesses), Name) :-
	indatabase([Name]),
	validentity(controlconnector(base), Name),

	getvalues(processspawn, _, Name, [c1, c2], [[C1], [C2]]),

	checkfortypemismatch(Name, C1, process(distributedprocesses),
	'Process spawn has c1 which is a non-process in Distributed Processes style'),
	checkfortypemismatch(Name, C2, process(distributedprocesses),
	'Process spawn has c2 which is a non-process in Distributed Processes style').

validentity(system(distributedprocesses), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),

	getvalues(system, _, Name,
	[controlcomponents, dataconnectors, calls, spawns],
	[Controlcomponents, Dataconnectors, Calls, Spawns]),

	((Dataconnectors == [unspecified] ;
		Controlcomponents == [unspecified]) ->
		true;
		(fetch(circuit, _, Dataconnectors, p1, CircuitP1s),
		fetch(circuit, _, Dataconnectors, p2, CircuitP2s),
		fetch(process, _, Controlcomponents, ports, ProcessPorts),
		union([CircuitP1s, CircuitP2s], CircuitPorts),
		(subset(CircuitPorts, ProcessPorts) ->
			true;
			zz('Distributed Processes system has circuit socket not found in process sockets',
			Name, (CircuitPorts, ProcessPorts))
		))
	),
	(Dataconnectors == [unspecified] ->
		true;
		(findall([P1,P2], (member(C,Dataconnectors),getvalue(circuit,_,C,p1,[P1]),
		P1 \== unspecified,getvalue(circuit,_,C,p2,[P2]),P2 \== unspecified), P12s),
		(is_set(P12s),findall([X,Y], (member([X,Y],P12s),member([Y,X],P12s)), DoubledReversed),
		DoubledReversed == []) ->
			true;
			zz('Distributed Processes system has two circuits connecting same pair of sockets',
			Name, Dataconnectors)
		)
	),
	fetch(process, _, Controlcomponents, nodes, CCnodes),
	fetch(processcall, _, Calls, nodes, Cnodes),
	fetch(processspawn, _, Spawns, nodes, Snodes),
	fetch(circuit, _, Dataconnectors, nodes, DCnodes),
	union([CCnodes,Cnodes,Snodes,DCnodes], Allnodes),
	length(Allnodes, LN),
	(LN > 1 ->
		true;
		zz('Distributed Processes system does not span multiple nodes',
		Name, Allnodes)
	).

/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(datastructure(distributedprocesses),
	[],
	[],
	[],
	[],
	datacomponent(base)).
typeschema(socket(distributedprocesses),
	[],
	[],
	[iotype],
	[[ioinout]],
	port(base)).
typeschema(process(distributedprocesses),
	[],
	[],
	[localobjects],
	[[]],
	controlcomponent(base)).
typeschema(circuit(distributedprocesses),
	[],
	[],
	[ccobj, directionality],
	[[ctype], [twoway]],
	dataconnector(base)).
typeschema(processcall(distributedprocesses),
	[],
	[],
	[queuesize, blocking],
	[[0], [true]],
	controlconnector(base)).
typeschema(processspawn(distributedprocesses),
	[],
	[],
	[queuesize, blocking],
	[[0], [false]],
	controlconnector(base)).
typeschema(system(distributedprocesses),
	[],
	[],
	[globalobjects, classes, shareddata, triggers],
	[[], [], [], []],
	system(base)).

informalnames(system(distributedprocesses),
	['Initial Processes    ', 
	'Initial Circuits     ',
	'Global Objects       ', 
	'Processes            ', 
	'Classes              ', 
	'Shared Data          ',
	'Circuits             ', 
	'Process Calls        ', 
	'Process Spawns       ',
	'Recognized Messages  ', 
	'Triggers             ', 
	'Call Layers          ',
	'Spawn Layers         ', 
	'Data Connector Layers', 
	'Nodes                ',
	'Resources            ', 
	'Platform             ']).

