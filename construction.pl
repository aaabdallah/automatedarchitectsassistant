/*************************************************************************/
/* Rules for supporting analysis of constructions */
/* May have to customize this per style (?) */
/* :- compile(library(lists)). */

/* Recursively get values */
recgetvalues(Name, Attribute, SystemNames, ValueLists) :-
	gathervalues([], [[Name]], Attribute, SystemNames, ValueLists).

/*recgetvalues(Name, Attribute, SystemNames, ValueLists) :-
	construction(_, _, name, [Name]),
	gathervalues([], [S1, S2], Attribute, SystemNames, ValueLists).*/

/*************************************************************************/
/* Rule to extract attribute values for attribute A out of a
   supplied system or construction */
/* gathervalues(systemname, stackofsystems, attribute, syslist, valuelist) */
/* the syslist and valuelist are output lists matching systems with their
   values for that attribute */

/* No name supplied to test, nothing on stack */
gathervalues([], [], _, [], []) :- !.
/* An unspecified system supplied, nothing on stack */
gathervalues([unspecified], [], _, [], []) :- !.
/* No name, but something on stack */
gathervalues([], [Top | Stack], Attribute, SystemNames, ValueLists) :-
	gathervalues(Top, Stack, Attribute, SystemNames, ValueLists).
/* An unspecified system, and something on stack */
gathervalues([unspecified], [Top|Stack], Attribute, SystemNames, ValueLists) :-
	gathervalues(Top, Stack, Attribute, SystemNames, ValueLists).
/* A supplied name for a system, but attribute value unspecified */
gathervalues([Name], Stack, Attribute, SystemNames, ValueLists) :-
	system(_, _, name, [Name]),
    getvalue(system, _, Name, Attribute, [unspecified]), !,
	gathervalues([], Stack, Attribute, SystemNames, ValueLists).
/* A supplied name for a system, and nonempty attribute value */
gathervalues([Name], Stack, Attribute, [Name | NRest], [VList | VRest]) :-
	system(_, _, name, [Name]),
    getvalue(system, _, Name, Attribute, VList),
	gathervalues([], Stack, Attribute, NRest, VRest).
/* A supplied name for a construction, attribute unspecified */
gathervalues([Name], Stack, Attribute, SystemNames, ValueLists) :-
	construction(_, _, name, [Name]),
	getvalue(construction, _, Name, Attribute, [unspecified]), !,
    getvalue(construction, _, Name, subsystem1, S1),
    getvalue(construction, _, Name, subsystem2, S2),
    gathervalues([], [S1, S2 | Stack], Attribute, SystemNames, ValueLists).
/* A supplied name for a construction, attribute has value */
gathervalues([Name], Stack, Attribute, [Name | NRest], [VList | VRest]) :-
	construction(_, _, name, [Name]),
	getvalue(construction, _, Name, Attribute, VList),
    getvalue(construction, _, Name, subsystem1, S1),
    getvalue(construction, _, Name, subsystem2, S2),
    gathervalues([], [S1, S2 | Stack], Attribute, NRest, VRest).
/* eventually have case for abstractions? */

/*************************************************************************/

