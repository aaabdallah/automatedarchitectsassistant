/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(pipefilter, 'Pipe & Filter').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(socket(pipefilter), Name) :-
	indatabase([Name]),
	validentity(port(base), Name),

	getvalues(socket, _, Name, [iotype], [Iotype]),

	(Iotype == [ioinout] ->
		zz('Pipe sockets cannot be in/out', Name, Iotype);
		true
	).

validentity(pipe(pipefilter), Name) :-
	indatabase([Name]),
	validentity(dataconnector(base), Name),

	getvalues(pipe, _, Name, [c1, c2, p1, p2], [[C1], [C2], [P1], [P2]]),

	checkfortypemismatch(Name, C1, filter(pipefilter),
	'Pipe socket c1 not a filter in Pipe/Filter style'),
	checkfortypemismatch(Name, C2, filter(pipefilter),
	'Pipe socket c2 not a filter in Pipe/Filter style'),
	checkfortypemismatch(Name, P1, socket(pipefilter),
	'Pipe socket p1 not a socket in Pipe/Filter style'),
	checkfortypemismatch(Name, P2, socket(pipefilter),
	'Pipe socket p2 not a socket in Pipe/Filter style'),
	((P1 == unspecified ; P2 == unspecified) ->
		true;
		(getvalue(socket, _, P1, iotype, [IO1]),
		getvalue(socket, _, P2, iotype, [IO2]),
		((IO1 == unspecified ; IO2 == unspecified) ->
			true;
			((IO1 == ioout ->
				true;
				zz('Pipe socket p1 must be an out socket', Name, (P1,IO1))
			),
			(IO2 == ioin ->
				true;
				zz('Pipe socket p2 must be an in socket', Name, (P2,IO2))
			))
		),
		(P1 \== P2 ->
			true;
			zz('Pipe sockets p1 and p2 must be different', Name, (P1, P2))
		))
	).

validentity(filter(pipefilter), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name),

	getvalues(filter, _, Name, [ports], [Ports]),

	checkfortypemismatch(Name, Ports, socket(pipefilter),
	'Filter ports include a non-socket in Pipe/Filter style').

validentity(system(pipefilter), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),

	getvalues(system, _, Name,
	[controlcomponents, dataconnectors, initialcontrolcomponents,
	initialdataconnectors],
	[Controlcomponents, Dataconnectors, Initialcontrolcomponents,
	Initialdataconnectors]),

	checkfortypemismatch(Name, Controlcomponents, filter(pipefilter),
	'Pipe/filter system controlcomponents include a non-filter'),
	(Dataconnectors == [unspecified] ->
		true;
		((mapfixtovar(confirmtype, pipe(pipefilter), Dataconnectors) ->
			true;
			zz('Pipe/filter system dataconnectors include a non-pipe',
			Name, Dataconnectors)
		),
		findall([Pipe, P1, P2], (member(Pipe,Dataconnectors),getvalue(pipe,_,Pipe,p1,[P1]),
		P1 \== unspecified,getvalue(pipe,_,Pipe,p2,[P2]),P2 \== unspecified), PP12s),
		findall([X, Y], (member([X,A,B],PP12s),member([Y,A,B],PP12s),X \== Y), DoubledPipes),
		length(DoubledPipes, ShouldbeZero),
		(ShouldbeZero == 0 ->
			true;
			zz('Two pipes connect same sender and receiver in pipe/filter system',
			Name, DoubledPipes)
		))
	),
	((Controlcomponents == [unspecified] ;
		Initialcontrolcomponents == [unspecified]) ->
		true;
		(seteq(Controlcomponents, Initialcontrolcomponents) ->
			true;
			zz('Pipe/filter system controlcomponents and initial controlcomponents differ',
			Name, (Controlcomponents, Initialcontrolcomponents))
		)
	),
	((Dataconnectors == [unspecified] ;
		Initialdataconnectors == [unspecified]) ->
		true;
		(seteq(Dataconnectors, Initialdataconnectors) ->
			true;
			zz('Pipe/filter system dataconnectors and initial dataconnectors differ',
			Name, (Dataconnectors, Initialdataconnectors))
		)
	),
	((Dataconnectors == [unspecified] ;
		Controlcomponents == [unspecified]) ->
		true;
		(fetch(pipe, _, Dataconnectors, p1, PipeP1s),
		fetch(pipe, _, Dataconnectors, p2, PipeP2s),
		fetch(filter, _, Controlcomponents, ports, FilterPorts),
		union([PipeP1s, PipeP2s], PipePorts),
		(subset(PipePorts, FilterPorts) ->
			true;
			zz('Pipe socket not found in filter sockets',
			Name, (PipePorts, FilterPorts))
		))
	).


/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(socket(pipefilter),
	[],
	[],
	[streaming],
	[[true]],
	port(base)).
typeschema(pipe(pipefilter),
	[],
	[],
	[ccobj, directionality],
	[[ctype], [forward]],
	dataconnector(base)).
typeschema(filter(pipefilter),
	[],
	[],
	[localobjects],
	[[]],
	controlcomponent(base)).
typeschema(system(pipefilter),
	[],
	[],
	[globalobjects, classes, calls, spawns, shareddata, call_layers, spawn_layers, triggers],
	[[], [], [], [], [], [], [], []],
	system(base)).

informalnames(system(pipefilter),
	['Initial Filters      ', 
	'Initial Pipes        ',
	'Global Objects       ', 
	'Filters              ', 
	'Classes              ', 
	'Shared Data          ',
	'Pipes                ', 
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

