/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(eventbased, 'Event-Based').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(event(eventbased), Name) :-
	indatabase([Name]),
	validentity(datacomponent(base), Name).

validentity(datastructure(eventbased), Name) :-
	indatabase([Name]),
	validentity(datacomponent(base), Name).

validentity(procedure(eventbased), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name),

	getvalues(procedure, _, Name, [locals], [Locals]),

	checkfortypemismatch(Name, Locals, datastructure(eventbased),
	'Event-based procedure locals include a non-datastructure').

validentity(procedurecall(eventbased), Name) :-
	indatabase([Name]),
	validentity(controlconnector(base), Name),

	getvalues(procedurecall, _, Name, [c1, c2], [[C1], [C2]]),

	checkfortypemismatch(Name, C1, procedure(eventbased),
	'Procedure call c1 not a procedure in Event-based style'),
	checkfortypemismatch(Name, C2, procedure(eventbased),
	'Procedure call c2 not a procedure in Event-based style').

validentity(object(eventbased), Name) :-
	indatabase([Name]),
	validentity(object(base), Name),

	getvalues(object, _, Name, [data, methods], [Data, Methods]),

	checkfortypemismatch(Name, Data, datastructure(eventbased),
	'Event-based object data includes a non-datastructure'),
	checkfortypemismatch(Name, Methods, procedure(eventbased),
	'Event-based object methods include a non-procedure').

validentity(trigger(eventbased), Name) :-
	indatabase([Name]),
	validentity(trigger(base), Name),

	getvalues(trigger, _, Name,
	[object, inmessage, outmessageset, outcalls],
	[Object, Inmessage, Outmessageset, Outcalls]),

	checkfortypemismatch(Name, Object, object(eventbased),
	'Event-based trigger object is a non-object'),
	checkfortypemismatch(Name, Inmessage, event(eventbased),
	'Event-based trigger inmessage is a non-event'),
	checkfortypemismatch(Name, Outmessageset, event(eventbased),
	'Event-based trigger outmessageset includes a non-event'),
	checkfortypemismatch(Name, Outcalls, procedurecall(eventbased),
	'Event-based trigger outcalls includes a non-procedure call').

validentity(system(eventbased), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),

	getvalues(system, _, Name,
	[controlcomponents, globalobjects, calls, recognizedmessages, triggers],
	[Controlcomponents, Globalobjects, Calls, RecognizedMessages, Triggers]),

	checkfortypemismatch(Name, Controlcomponents, procedure(eventbased),
	'Event-based system controlcomponents include a non-procedure'),
	checkfortypemismatch(Name, Globalobjects, object(eventbased),
	'Event-based system globalobjects include a non-object'),
	checkfortypemismatch(Name, Calls, procedurecall(eventbased),
	'Event-based system calls include a non-procedure call'),
	checkfortypemismatch(Name, RecognizedMessages, event(eventbased),
	'Event-based system recognized messages include a non-event'),
	checkfortypemismatch(Name, Triggers, trigger(eventbased),
	'Event-based system triggers include a non-trigger'),
	((Triggers == [unspecified] ; Triggers == []) ->
		zz('Event-based system needs a set of triggers', Name, Triggers);
		true
	).

/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(event(eventbased),
	[],
	[],
	[],
	[],
	datacomponent(base)).
typeschema(datastructure(eventbased),
	[],
	[],
	[],
	[],
	datacomponent(base)).
typeschema(procedure(eventbased),
	[],
	[],
	[ports],
	[[]],
	controlcomponent(base)).
typeschema(procedurecall(eventbased),
	[],
	[],
	[queuesize, blocking],
	[[0], [true]],
	controlconnector(base)).
typeschema(object(eventbased),
	[],
	[],
	[],
	[],
	object(base)).
typeschema(trigger(eventbased),
	[],
	[],
	[subtype, outspawns],
	[[otype], []],
	trigger(base)).
typeschema(system(eventbased),
	[],
	[],
	[spawns, shareddata],
	[[], []],
	system(base)).

informalnames(system(eventbased),
	['Initial Procedures     ', 
	'Initial Data Connectors',
	'Global Objects         ', 
	'Procedures             ', 
	'Classes                ', 
	'Shared Data            ',
	'Data Connectors        ', 
	'Procedure Calls        ', 
	'Procedure Spawns       ',
	'Recognized Messages    ', 
	'Triggers               ', 
	'Call Layers            ',
	'Spawn Layers           ', 
	'Data Connector Layers  ', 
	'Nodes                  ',
	'Resources              ', 
	'Platform               ']).

