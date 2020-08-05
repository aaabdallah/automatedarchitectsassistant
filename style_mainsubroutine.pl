/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(mainsubroutine, 'Main/Subroutine').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(datastructure(mainsubroutine), Name) :-
	indatabase([Name]),
	validentity(datacomponent(base), Name).

validentity(procedure(mainsubroutine), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name),

	getvalues(procedure, _, Name, [locals], [Locals]),

	checkfortypemismatch(Name, Locals, datastructure(mainsubroutine),
	'Main/Subroutine procedure locals include a non-datastructure').

validentity(procedurecall(mainsubroutine), Name) :-
	indatabase([Name]),
	validentity(controlconnector(base), Name),

	getvalues(procedurecall, _, Name, [c1, c2], [[C1], [C2]]),

	checkfortypemismatch(Name, C1, procedure(mainsubroutine),
	'Procedure call c1 not a procedure in Main/Subroutine style'),
	checkfortypemismatch(Name, C2, procedure(mainsubroutine),
	'Procedure call c2 not a procedure in Main/Subroutine style').

validentity(system(mainsubroutine), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),

	getvalues(system, _, Name,
	[shareddata, controlcomponents, initialcontrolcomponents, calls],
	[Shareddata, Controlcomponents, Initialcontrolcomponents, Calls]),

	(Shareddata == [unspecified] ->
		true;
		(findall(D, (member([D,_CC,_OBJ],Shareddata)), AllData),
		(mapfixtovar(confirmtype, datastructure(mainsubroutine), AllData) ->
			true;
			zz('Main/subroutine system shareddata includes a non-datastructure',
			Name, AllData)
		))
	),
	checkfortypemismatch(Name, Calls, procedurecall(mainsubroutine),
	'Main/Subroutine system calls include a non-procedure call'),
	checkfortypemismatch(Name, Controlcomponents, procedure(mainsubroutine),
	'Main/Subroutine system controlcomponents include a non-procedure'),
	(Initialcontrolcomponents == [unspecified] ->
		true;
		(length(Initialcontrolcomponents, LI),
		(LI == 1 ->
			true;
			zz('Main/subroutine system initialcontrolcomponents must be one',
			Name, Initialcontrolcomponents)
		),
		(Calls == [unspecified] ->
			true;
			(Initialcontrolcomponents = [Main],
			findall(X,(member(C,Calls),getvalue(controlconnector,_,C,c2,[X]),
			X \== unspecified),CallC2s),
			(member(Main, CallC2s) ->
				zz('Main routine called in Main/Subroutine system', Name,
				(Main,Calls));
				true
			),
			(Controlcomponents == [unspecified] ->
				true;
				(subtract(Controlcomponents, [Main], Subroutines),
				findall([X,Y],(member(C,Calls),getvalue(controlconnector,_,C,c1,[X]),
				X \== unspecified,getvalue(controlconnector,_,C,c2,[Y]),Y \== unspecified),
				CallC12s),
				append([Main], Subroutines, AllProcedures),
				(cdgraph(AllProcedures,CallC12s) ->
					true;
					zz('Main/subroutine system requires directed, connected call graph from main',
					Name, (AllProcedures, CallC12s))
				))
			))
		))
	),
	(Initialcontrolcomponents == [unspecified] ->
		getmaxthreads([], [], [], Calls, Threads);
		getmaxthreads(Initialcontrolcomponents, [], [], Calls, Threads)
	),
	length(Threads, LT),
	(LT == 1 ->
		true;
		zz('Main/subroutine system must have one thread only',
		Name, Threads)
	),
	fetch(procedure, _, Controlcomponents, nodes, CCnodes),
	fetch(procedurecall, _, Calls, nodes, Cnodes),
	union([CCnodes,Cnodes], Allnodes),
	length(Allnodes, LN),
	(LN == 1 ->
		true;
		zz('Main/subroutine system should have a single node only',
		Name, Allnodes)
	).



/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(datastructure(mainsubroutine),
	[],
	[],
	[],
	[],
	datacomponent(base)).
typeschema(procedure(mainsubroutine),
	[],
	[],
	[ports, localobjects],
	[[], []],
	controlcomponent(base)).
typeschema(procedurecall(mainsubroutine),
	[],
	[],
	[queuesize, blocking],
	[[0], [true]],
	controlconnector(base)).
typeschema(system(mainsubroutine),
	[],
	[],
	[globalobjects, classes, spawns, dataconnectors, spawn_layers, dataconnector_layers,
	triggers],
	[[], [], [], [], [], [], []],
	system(base)).

informalnames(system(mainsubroutine),
	['Initial Procedures     ', 
	'Initial Data Connectors',
	'Global Objects         ', 
	'Procedures             ', 
	'Classes                ', 
	'Shared Data            ',
	'Data Connectors        ', 
	'Procedure Calls        ', 
	'Spawns                 ',
	'Recognized Messages    ', 
	'Triggers               ', 
	'Call Layers            ',
	'Spawn Layers           ', 
	'Data Connector Layers  ', 
	'Nodes                  ',
	'Resources              ', 
	'Platform               ']).

