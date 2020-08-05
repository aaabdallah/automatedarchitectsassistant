/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(distributed, 'Distributed').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(system(distributed), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),
	getvalues(system, _, Name,
	[controlcomponents, dataconnectors, calls, spawns],
	[Controlcomponents, Dataconnectors, Calls, Spawns]),

	fetch(controlcomponent, _, Controlcomponents, nodes, CCnodes),
	fetch(controlconnector, _, Calls, nodes, Cnodes),
	fetch(controlconnector, _, Spawns, nodes, Snodes),
	fetch(dataconnector, _, Dataconnectors, nodes, DCnodes),
	union([CCnodes,Cnodes,Snodes,DCnodes], Allnodes),
	length(Allnodes, LN),
	(LN > 1 ->
		true;
		zz('Distributed system does not span multiple nodes',
		Name, Allnodes)
	).

/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(system(distributed),
	[],
	[],
	[],
	[],
	system(base)).

informalnames(system(distributed),
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

