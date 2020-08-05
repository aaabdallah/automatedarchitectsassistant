/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(softbench, 'HP SoftBench').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(event(softbench), Name) :-
	indatabase([Name]),
	validentity(datacomponent(base), Name).

validentity(controlcomponent(softbench), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name).

validentity(trigger(softbench), Name) :-
	indatabase([Name]),
	validentity(trigger(base), Name),

	getvalues(trigger, _, Name,
	[controlcomponent, inmessage, outmessageset, outcalls, outspawns],
	[Controlcomponent, Inmessage, Outmessageset, Outcalls, Outspawns]),

	checkfortypemismatch(Name, Controlcomponent, controlcomponent(softbench),
	'SoftBench trigger controlcomponent lacks proper type'),
	checkfortypemismatch(Name, Inmessage, event(softbench),
	'SoftBench trigger inmessage lacks proper type'),
	checkfortypemismatch(Name, Outmessageset, event(softbench),
	'SoftBench trigger outmessageset includes entity lacking proper type'),
	checkfortypemismatch(Name, Outcalls, controlcomponent(softbench),
	'SoftBench trigger outcalls includes entity lacking proper type'),
	checkfortypemismatch(Name, Outspawns, controlcomponent(softbench),
	'SoftBench trigger outspawns includes entity lacking proper type').

validentity(system(softbench), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),

	getvalues(system, _, Name,
	[controlcomponents, recognizedmessages, triggers],
	[Controlcomponents, Recognizedmessages, Triggers]),

	checkfortypemismatch(Name, Controlcomponents, controlcomponent(softbench),
	'SoftBench system controlcomponents includes entity lacking proper type'),
/*	checkfortypemismatch(Name, Recognizedmessages, event(softbench),
	'SoftBench system recognizedmessages includes entity lacking proper type'),*/
	checkfortypemismatch(Name, Triggers, trigger(softbench),
	'SoftBench system triggers includes entity lacking proper type'),
	((Triggers == [unspecified] ; Triggers == []) ->
		zz('SoftBench system needs a set of triggers', Name, Triggers);
		true
	).


/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(event(softbench),
	[],
	[],
	[type],
	[[string]],
	datacomponent(base)).
typeschema(controlcomponent(softbench),
	[],
	[],
	[codesize],
	[[1200]],
	controlcomponent(base)).
typeschema(trigger(softbench),
	[],
	[],
	[],
	[],
	trigger(base)).
typeschema(system(softbench),
	[],
	[],
	[recognizedmessages],
	[[xevent_keypress, xevent_keyrelease, xevent_buttonpress]],
	system(base)).

informalnames(system(softbench),
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

