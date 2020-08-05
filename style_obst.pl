/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(obst, 'OBST Database').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(datacomponent(obst), Name) :-
	indatabase([Name]),
	validentity(datacomponent(base), Name).

validentity(controlcomponent(obst), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name).

validentity(system(obst), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),

	getvalues(system, _, Name,
	[triggers, spawns, controlcomponents, initialcontrolcomponents, calls],
	[Triggers, Spawns, Controlcomponents, Initialcontrolcomponents, Calls]),

	checkfortypemismatch(Name, Controlcomponents, controlcomponent(obst),
	'OBST system control components include a non-OBST component'),

	(Initialcontrolcomponents == [unspecified] ->
		getmaxthreads([], Triggers, Spawns, Calls, Threads);
		getmaxthreads(Initialcontrolcomponents, Triggers, Spawns, Calls, Threads)
	),
	length(Threads, LT),
	(LT == 1 ->
		true;
		zz('OBST system must have one thread only', Name, Threads)
	).


/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(datacomponent(obst),
	[],
	[],
	[datasize],
	[[400]],
	datacomponent(base)).
typeschema(controlcomponent(obst),
	[],
	[],
	[],
	[],
	controlcomponent(base)).
typeschema(system(obst),
	[],
	[],
	[],
	[],
	system(base)).

informalnames(system(obst),
	['Initial Control Components', 
	'Initial Data Connectors   ',
	'Global Objects            ', 
	'Control Components        ', 
	'Classes                   ', 
	'Shared Data               ',
	'Data Connectors           ', 
	'Calls                     ', 
	'Spawns                    ',
	'Recognized Messages       ', 
	'Triggers                  ', 
	'Call Layers               ',
	'Spawn Layers              ', 
	'Data Connector Layers     ', 
	'Nodes                     ',
	'Resources                 ', 
	'Platform                  ']).

