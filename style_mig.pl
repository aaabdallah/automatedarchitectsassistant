/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(mig, 'Mach RPC Interface').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/




/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(event(mig),
	[],
	[],
	[],
	[],
	datacomponent(base)).
typeschema(controlcomponent(mig),
	[],
	[],
	[],
	[],
	controlcomponent(base)).
typeschema(trigger(mig),
	[],
	[],
	[subtype, outspawns],
	[[ctype], []],
	trigger(base)).
typeschema(system(mig),
	[],
	[],
	[globalobjects, classes, calls, spawns, shareddata, call_layers, spawn_layers,
	dataconnector_layers],
	[[], [], [], [], [], [], [], []],
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
