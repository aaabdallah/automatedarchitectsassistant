/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(softwarebus, 'Software Bus').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

validentity(system(softwarebus), Name) :-
	indatabase([Name]),
	validentity(system(base), Name),
	getvalues(system, _, Name,
	[controlcomponents, dataconnectors],
	[Controlcomponents, Dataconnectors]),

	((Controlcomponents == [unspecified] ; Dataconnectors == [unspecified]) ->
		true;
		(findall([X,Y],(member(C,Dataconnectors),getvalue(dataconnector,_,C,c1,[X]),
		X \== unspecified,getvalue(dataconnector,_,C,c2,[Y]),Y \== unspecified),
		DataconnectorC12s),
		(ustar(Controlcomponents,DataconnectorC12s) ->
			true;
			zz('Bus not connected to all communicants in software bus system',
			Name, (Controlcomponents, DataconnectorC12s))
		),
		length(Controlcomponents, LCC),
		length(Dataconnectors, LDC),
		(LDC is LCC - 1 ->
			true;
			zz('Too many or too few dataconnectors for software bus system',
			Name, (Dataconnectors, Controlcomponents))
		))
	).

/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(system(softwarebus),
	[],
	[],
	[],
	[],
	system(base)).

informalnames(system(softwarebus),
	['Initial Communicants   ', 
	'Initial Data Connectors',
	'Global Objects         ', 
	'Communicants           ', 
	'Classes                ', 
	'Shared Data            ',
	'Data Connectors        ', 
	'Calls                  ', 
	'Spawns                 ',
	'Recognized Messages    ', 
	'Triggers               ', 
	'Call Layers            ',
	'Spawn Layers           ', 
	'Data Connector Layers  ', 
	'Nodes                  ',
	'Resources              ', 
	'Platform               ']).
