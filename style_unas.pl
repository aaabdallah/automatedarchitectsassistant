/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(unas, 'UNAS').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(message(unas), Name) :-
	indatabase([Name]),
	validentity(datastructure(base), Name).

validentity(socket(unas), Name) :-
	indatabase([Name]),
	validentity(port(base), Name).

validentity(procedure(unas), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name).

validentity(task(unas), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name),

	getvalues(task, _, Name, [ports], [Ports]),

	checkfortypemismatch(Name, Ports, socket(unas),
	'Task has ports which include a non-socket in UNAS style').

validentity(callback(unas), Name) :-
	indatabase([Name]),
	validentity(controlconnector(base), Name),

	getvalues(callback, _, Name, [c1, c2], [[C1], [C2]]),

	checkfortypemismatch(Name, C1, task(unas),
	'Callback has c1 which is a non-task in UNAS style'),
	checkfortypemismatch(Name, C2, procedure(unas),
	'Callback has c2 which is a non-procedure in UNAS style').

validentity(circuit(unas), Name) :-
	indatabase([Name]),
	validentity(dataconnector(base), Name),

	getvalues(circuit, _, Name, [p1, p2, c1, c2], [[P1], [P2], [C1], [C2]]),

	checkfortypemismatch(Name, P1, socket(unas),
	'Circuit has p1 which is a non-socket in UNAS style'),
	checkfortypemismatch(Name, P2, socket(unas),
	'Circuit has p2 which is a non-socket in UNAS style'),
	checkfortypemismatch(Name, C1, task(unas),
	'Circuit has c1 which is a non-task in UNAS style'),
	checkfortypemismatch(Name, C2, task(unas),
	'Circuit has c2 which is a non-task in UNAS style').

validentity(trigger(unas), Name) :-
	indatabase([Name]),
	validentity(trigger(base), Name),

	getvalues(trigger, _, Name,
	[controlcomponent, inmessage, outmessageset, outcalls],
	[Controlcomponent, Inmessage, Outmessageset, Outcalls]),

	checkfortypemismatch(Name, Controlcomponent, procedure(unas),
	'UNAS trigger controlcomponent is a non-procedure'),
	checkfortypemismatch(Name, Inmessage, message(unas),
	'UNAS trigger inmessage is a non-message'),
	checkfortypemismatch(Name, Outmessageset, message(unas),
	'UNAS trigger outmessageset includes a non-message'),
	checkfortypemismatch(Name, Outcalls, callback(unas),
	'UNAS trigger outcalls includes a non-callback').

validentity(system(unas), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),

	getvalues(system, _, Name,
	[controlcomponents, dataconnectors, recognizedmessages, calls,
	initialcontrolcomponents, initialdataconnectors, triggers],
	[Controlcomponents, Dataconnectors, Recognizedmessages, Calls,
	Initialcontrolcomponents, Initialdataconnectors, Triggers]),

	(validunascontrolcomponents(Controlcomponents) ->
		true;
		zz('UNAS system controlcomponents includes more than tasks & procedures',
		Name, Controlcomponents)
	),
	checkfortypemismatch(Name, Calls, callback(unas),
	'UNAS system controlconnectors include a non-callback'),
	checkfortypemismatch(Name, Dataconnectors, circuit(unas),
	'UNAS system dataconnectors include a non-circuit'),
	checkfortypemismatch(Name, Recognizedmessages, message(unas),
	'UNAS system recognized messages include a non-message'),
	checkfortypemismatch(Name, Triggers, trigger(unas),
	'UNAS system triggers include a non-trigger'),
	checkfortypemismatch(Name, Initialcontrolcomponents, task(unas),
	'UNAS system initialcontrolcomponents should only have tasks'),
	findall(T, (member(T,Controlcomponents),confirmtype(task(unas),T)), Tasks),
	(Initialcontrolcomponents == [unspecified] ->
		true;
		(seteq(Initialcontrolcomponents, Tasks) ->
			true;
			zz('UNAS system initial and allowed tasks are not the same',
			Name, (Initialcontrolcomponents, Tasks))
		)
	),
	((Dataconnectors == [unspecified] ;
		Initialdataconnectors == [unspecified]) ->
		true;
		(seteq(Dataconnectors, Initialdataconnectors) ->
			true;
			zz('UNAS system initial and allowed circuits are not the same',
			Name, (Initialdataconnectors, Dataconnectors))
		)
	),
	((Dataconnectors == [unspecified] ;
		Controlcomponents == [unspecified]) ->
		true;
		(fetch(circuit, _, Dataconnectors, p1, CircuitP1s),
		fetch(circuit, _, Dataconnectors, p2, CircuitP2s),
		fetch(task, _, Tasks, ports, TaskPorts),
		union([CircuitP1s, CircuitP2s], CircuitPorts),
		(subset(CircuitPorts, TaskPorts) ->
			true;
			zz('UNAS system has circuit socket not found in task sockets',
			Name, (CircuitPorts, TaskPorts))
		))
	),
	(Dataconnectors == [unspecified] ->
		true;
		(findall([P1,P2], (member(C,Dataconnectors),getvalue(circuit,_,C,p1,[P1]),
		P1 \== unspecified,getvalue(circuit,_,C,p2,[P2]),P2 \== unspecified), P12s),
		(is_set(P12s),findall([X,Y], (member([X,Y],P12s),member([Y,X],P12s)), DoubledReversed),
		DoubledReversed == []) ->
			true;
			zz('UNAS system has two circuits connecting same pair of sockets',
			Name, Dataconnectors)
		)
	),
	((Triggers == [unspecified] ; Triggers == []) ->
		zz('UNAS system needs a set of triggers', Name, Triggers);
		true
	).


validunascontrolcomponents([unspecified]) :- !.
validunascontrolcomponents([]) :- !.
validunascontrolcomponents([HC | TC]) :-
	confirmtype(task(unas), HC), validunascontrolcomponents(TC).
validunascontrolcomponents([HC | TC]) :-
	confirmtype(procedure(unas), HC), validunascontrolcomponents(TC).



/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(message(unas),
	[],
	[],
	[],
	[],
	datastructure(base)).
typeschema(socket(unas),
	[],
	[],
	[streaming, blocking],
	[[false], [false]],
	port(base)).
typeschema(procedure(unas),
	[],
	[],
	[ports],
	[[]],
	controlcomponent(base)).
typeschema(task(unas),
	[],
	[],
	[],
	[],
	controlcomponent(base)).
typeschema(callback(unas),
	[],
	[],
	[blocking, queuesize],
	[[true], [0]],
	controlconnector(base)).
typeschema(circuit(unas),
	[],
	[],
	[ccobj, reliable, directionality],
	[[ctype], [true], [twoway]],
	dataconnector(base)).
typeschema(trigger(unas),
	[],
	[],
	[subtype, outspawns],
	[[ctype], []],
	trigger(base)).
typeschema(system(unas),
	[],
	[],
	[globalobjects, classes, spawns, shareddata, call_layers, spawn_layers, dataconnector_layers],
	[[], [], [], [], [], [], []],
	system(base)).

informalnames(system(unas),
	['Initial Tasks        ', 
	'Initial Circuits     ',
	'Global Objects       ', 
	'Tasks and Procedures ', 
	'Classes              ', 
	'Shared Data          ',
	'Circuits             ', 
	'Calls                ', 
	'Spawns               ',
	'Recognized Messages  ', 
	'Triggers             ', 
	'Call Layers          ',
	'Spawn Layers         ', 
	'Data Connector Layers', 
	'Nodes                ',
	'Resources            ', 
	'Platform             ']).
