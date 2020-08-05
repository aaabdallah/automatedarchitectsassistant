/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(mach, 'Mach RPC Interface').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(event(mach), Name) :-
	indatabase([Name]),
	validentity(datacomponent(base), Name),

	getvalues(event, _, Name, type, [Type]),

	((Type == unspecified ; Type == structure ; Type == array) ->
		true;
		zz('Mach event must have structure or array as data type',
		Name, Type)
	).

validentity(controlcomponent(mach), Name) :-
	indatabase([Name]),
	validentity(controlcomponent(base), Name).

validentity(trigger(mach), Name) :-
	indatabase([Name]),
	validentity(trigger(base), Name),

	getvalues(trigger, _, Name,
	[controlcomponent, inmessage, outmessageset, outcalls],
	[Controlcomponent, Inmessage, Outmessageset, Outcalls]),

	checkfortypemismatch(Name, Controlcomponent, controlcomponent(mach),
	'Mach trigger controlcomponent lacks proper type'),
	checkfortypemismatch(Name, Inmessage, event(mach),
	'Mach trigger inmessage lacks proper type'),
	checkfortypemismatch(Name, Outmessageset, event(mach),
	'Mach trigger outmessageset includes entity lacking proper type'),
	checkfortypemismatch(Name, Outcalls, controlcomponent(mach),
	'Mach trigger outcalls includes entity lacking proper type').

validentity(system(mach), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),

	getvalues(system, _, Name,
	[controlcomponents, recognizedmessages, triggers, initialcontrolcomponents],
	[Controlcomponents, Recognizedmessages, Triggers, Initialcontrolcomponents]),

	checkfortypemismatch(Name, Controlcomponents, controlcomponent(mach),
	'Mach system controlcomponents includes entity lacking proper type'),
	checkfortypemismatch(Name, Initialcontrolcomponents, controlcomponent(mach),
	'Mach system initialcontrolcomponents includes entity lacking proper type'),
	checkfortypemismatch(Name, Recognizedmessages, event(mach),
	'Mach system recognizedmessages includes entity lacking proper type'),
	checkfortypemismatch(Name, Triggers, trigger(mach),
	'Mach system triggers includes entity lacking proper type'),

	((Controlcomponents == [unspecified] ;
		Initialcontrolcomponents == [unspecified]) ->
		true;
		(seteq(Controlcomponents, Initialcontrolcomponents) ->
			true;
			zz('Mach system controlcomponents and initial controlcomponents differ',
			Name, (Controlcomponents, Initialcontrolcomponents))
		)
	),
/*	(Controlcomponents == [unspecified] ->
		true;
		(length(Controlcomponents, LC),
		(LC == 1 ->
			true;
			zz('Mach system controlcomponents must be one',
			Name, Controlcomponents)
		))
	),*/
	((Triggers == [unspecified] ; Triggers == []) ->
		zz('Mach system missing a set of triggers', Name, Triggers);
		true
	).

/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(event(mach),
	[],
	[],
	[type],
	[[structure]],
	datacomponent(base)).
typeschema(controlcomponent(mach),
	[],
	[],
	[],
	[],
	controlcomponent(base)).
typeschema(trigger(mach),
	[],
	[],
	[subtype, outspawns],
	[[ctype], []],
	trigger(base)).
typeschema(system(mach),
	[],
	[],
	[globalobjects, classes, calls, spawns, shareddata, call_layers, spawn_layers,
	dataconnector_layers, recognizedmessages],
	[[], [], [], [], [], [], [], [],
	[machmsgtype_rpc, machmsgtype_encrypted, machmsgtype_normal]],
	system(base)).

informalnames(system(mach),
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

