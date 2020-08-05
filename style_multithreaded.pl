/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(multithreaded, 'Multithreaded').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(system(multithreaded), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),
	getvalues(system, _, Name,
	[initialcontrolcomponents, triggers, spawns, calls],
	[Initialcontrolcomponents, Triggers, Spawns, Calls]),

	(Initialcontrolcomponents == [unspecified] ->
		getmaxthreads([], Triggers, Spawns, Calls, Threads);
		getmaxthreads(Initialcontrolcomponents, Triggers, Spawns, Calls, Threads)
	),
	length(Threads, LT),
	(LT > 1 ->
		true;
		zz('Multithreaded system contains less than 2 threads',
		Name, Threads)
	).

/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(system(multithreaded),
	[],
	[],
	[],
	[],
	system(base)).

informalnames(system(multithreaded),
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

