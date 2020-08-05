/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(interviews, 'Stanford InterViews').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(event(interviews), Name) :-
	indatabase([Name]),
	validentity(datacomponent(base), Name),

	(member(Name, [xevent_keypress, xevent_keyrelease, xevent_buttonpress]) ->
		true;
		zz('Unrecognized OBST event', Name, Name)
	).

validentity(controlcomponent(interviews), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name).

validentity(object(interviews), Name) :-
	indatabase([Name]),
	validentity(object(base), Name).

validentity(trigger(interviews), Name) :-
	indatabase([Name]),
	validentity(trigger(base), Name),

	getvalues(trigger, _, Name,
	[object, inmessage, outmessageset, outcalls, outspawns],
	[Object, Inmessage, Outmessageset, Outcalls, Outspawns]),

	checkfortypemismatch(Name, Object, object(interviews),
	'InterViews trigger object lacks proper type'),
	checkfortypemismatch(Name, Inmessage, event(interviews),
	'InterViews trigger inmessage lacks proper type'),
	checkfortypemismatch(Name, Outmessageset, event(interviews),
	'InterViews trigger outmessageset includes entity lacking proper type'),
	checkfortypemismatch(Name, Outcalls, controlcomponent(interviews),
	'InterViews trigger outcalls includes entity lacking proper type'),
	checkfortypemismatch(Name, Outspawns, controlcomponent(interviews),
	'InterViews trigger outspawns includes entity lacking proper type').

validentity(system(interviews), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),

	getvalues(system, _, Name,
	[controlcomponents, globalobjects, recognizedmessages, triggers],
	[Controlcomponents, Globalobjects, Recognizedmessages, Triggers]),

	checkfortypemismatch(Name, Controlcomponents, controlcomponent(interviews),
	'InterViews system controlcomponents includes entity lacking proper type'),
	checkfortypemismatch(Name, Globalobjects, object(interviews),
	'InterViews system globalobjects includes entity lacking proper type'),
/*	checkfortypemismatch(Name, Recognizedmessages, event(interviews),
	'InterViews system recognizedmessages includes entity lacking proper type'),*/
	checkfortypemismatch(Name, Triggers, trigger(interviews),
	'InterViews system triggers includes entity lacking proper type'),
	((Triggers == [unspecified] ; Triggers == []) ->
		zz('InterViews system needs a set of triggers', Name, Triggers);
		true
	).


/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(event(interviews),
	[],
	[],
	[type],
	[[string]],
	datacomponent(base)).
typeschema(controlcomponent(interviews),
	[],
	[],
	[],
	[],
	controlcomponent(base)).
typeschema(object(interviews),
	[],
	[],
	[],
	[],
	object(base)).
typeschema(trigger(interviews),
	[],
	[],
	[subtype],
	[[ctype]],
	trigger(base)).
typeschema(system(interviews),
	[],
	[],
	[recognizedmessages],
	[[xevent_keypress, xevent_keyrelease, xevent_buttonpress]],
	system(base)).

informalnames(system(interviews),
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

