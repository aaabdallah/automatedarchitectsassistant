/* Rules for:
1. recognizing attributes, checking their values
2. interacting with the database of attributes

The types of entities are stored as:
typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))

attributes stored as: entity(status, name, attribute, attributevaluelist)
example1: filter(added, f1, inputsockets, [s1, s2])
example2: socket(added, s1, buffersize, [10])
NOTE THAT LAST ELEMENT IS A LIST!!!!
*/

/* :- compile(library(sets)). */
/* :- compile(library(lists)). */

/*************************************************************************/
/* Entities that may be found in the database */
:- dynamic callback/4.
:- dynamic circuit/4.
:- dynamic controlcomponent/4.
:- dynamic controlconnector/4.
:- dynamic datacomponent/4.
:- dynamic dataconnector/4.
:- dynamic datastructure/4.
:- dynamic event/4.
:- dynamic filter/4.
:- dynamic group/4.
:- dynamic message/4.
:- dynamic object/4.
:- dynamic pipe/4.
:- dynamic port/4.
:- dynamic procedure/4.
:- dynamic procedurecall/4.
:- dynamic process/4.
:- dynamic processcall/4.
:- dynamic processspawn/4.
:- dynamic socket/4.
:- dynamic system/4.
:- dynamic task/4.
:- dynamic trigger/4.

alltypes([callback(unas), circuit(distributedprocesses), circuit(unas),
	controlcomponent(base), controlcomponent(interviews),
	controlcomponent(mach), controlcomponent(obst), controlcomponent(softbench),
	controlconnector(base),
	datacomponent(base), datacomponent(obst),
	dataconnector(base), datastructure(distributedprocesses), datastructure(eventbased), 
	datastructure(mainsubroutine),
	event(eventbased), event(interviews), event(mach), event(softbench),
	filter(pipefilter), group(base), message(unas),
	object(base), object(eventbased), object(interviews),
	pipe(pipefilter),
	port(base), procedure(eventbased), procedure(mainsubroutine), procedure(unas),
	procedurecall(eventbased),
	procedurecall(mainsubroutine),
	process(distributedprocesses), processcall(distributedprocesses),
	processspawn(distributedprocesses), 
	socket(distributedprocesses), socket(pipefilter), socket(unas), 
	system(base), system(distributed), 
	system(distributedprocesses), system(eventbased), system(interviews),
	system(mach), system(mainsubroutine),
	system(multithreaded), system(obst),
	system(pipefilter), system(softbench), system(softwarebus), system(unas), task(unas),
	trigger(base), trigger(eventbased), trigger(interviews),
	trigger(mach), trigger(softbench),
	trigger(unas)]).

/*************************************************************************/
/* Section for processing an entity when first encountered */

/*recognizedtype(type)*/
/*type is given as entity(style)*/
recognizedtype(Type, Type) :-
	typeschema(Type,_,_,_,_,_), !.
recognizedtype(Type,RealBaseType) :-
	Type =.. [Entity, Style], Style \== base, style(Style,_),
	RealBaseType =.. [Entity, base], typeschema(RealBaseType,_,_,_,_,_), !.

/*processentity(name of entity, its type, attributes, values of former)*/
/*type is given as entity(style)*/
/*After parsing an entity, analyze its attributes, add it to database*/
processentity(SystemName, GroupType, Attributes, ValueLists) :-
	GroupType == group(base),
	getgroupname(SystemName, GroupName),
	validattributes(GroupName, GroupType, Attributes),
	validattributevalues(GroupType, Attributes, ValueLists),
	isnewentity(SystemName, system(base)),
	isnewentity(GroupName, GroupType),
/* Check that all subsystems are not part of any grouping input already */

	nth1(Place, Attributes, subsystems),
	nth1(Place, ValueLists, Subsystems),
	(systemsnotgrouped(Subsystems) ->
		true;
		(write('ERROR: Group includes already grouped subsystems.'), nl,
		write('Group: '), write(GroupName), nl,
		write('Subsystems: '), write(Subsystems), nl, nl,
		!, fail)
	),
	removefromdatabase(SystemName, system(base)),
	removefromdatabase(GroupName, GroupType),
	addtodatabase(GroupName, GroupType, Attributes, ValueLists),
	assignchildren(GroupName, GroupType, Attributes, ValueLists),
	processgroup(GroupName, GroupType, SystemName), !.
processentity(Name, Type, Attributes, ValueLists) :-
	Type =.. [Entity, _],
	Entity \== group,
	validattributes(Name, Type, Attributes),
	validattributevalues(Type, Attributes, ValueLists),
	isnewentity(Name, Type),
	removefromdatabase(Name, Type),
	addtodatabase(Name, Type, Attributes, ValueLists),
	assignchildren(Name, Type, Attributes, ValueLists), !.

isnewentity(Name, Type) :-
	Type =.. [Entity, _],
	indatabase([Name], Entity),
	write('ERROR: Declaration of previously declared entity.'), nl,
	write('Name: '), write(Name), nl,
	write('Type: '), write(Type), nl, nl,
	!, fail.
isnewentity(_, _).

/*validattributes(name, type, entered variable attributes)*/
/*type is given as entity(style)*/
validattributes(Name, Type, Attrs) :-
	gettypeschemaattributes(Type, _, _, FixedAttrs),
	intersect(Attrs, FixedAttrs),
	write('ERROR: Declaration of a fixed attribute.'), nl,
	write('Name: '), write(Name), nl,
	write('Type: '), write(Type), nl,
	write('Fixed Attributes: '), write(FixedAttrs), nl,
	write('Attributes: '), write(Attrs), nl, nl,
	!, fail.
validattributes(_, Type, Attrs) :-
	is_set(Attrs),
	gettypeschemaattributes(Type, AllAttrs, _, _),
	subset(Attrs, AllAttrs), !.
validattributes(Name, Type, Attrs) :-
	write('ERROR: Unrecognized or duplicated attributes.'), nl,
	write('Name: '), write(Name), nl,
	write('Type: '), write(Type), nl,
	write('Attributes: '), write(Attrs), nl, nl,
	!, fail.

/*gettypeschemaattributes(Type, AllAttributes, AttrTypes, FixedAttributes) */
/* Recursively retrieve all the attributes of an entity, derived and otherwise.*/
/* Also returns the types of the attributes, as well as the fixed attributes. */
gettypeschemaattributes(Type, AllAttributes, AttrTypes, FixedAttributes) :-
	gettypeschemaattributes(Type, [], All1, [], AT1, [], Fixed1),
	flatten(All1, AllAttributes),
	flatten(AT1, AttrTypes),
	flatten(Fixed1, Fixed2),
	remove_dups(Fixed2, FixedAttributes).

gettypeschemaattributes(Type, AccAttrs, [Attrs | AccAttrs], AccATs, [ATs | AccATs],
	AccFixed, [Fixed | AccFixed]) :-
	typeschema(Type, Attrs, ATs, Fixed, _, []).
gettypeschemaattributes(Type, AccAttrs, TotalAttrs, AccATs, TotalATs, AccFixed, TotalFixed) :-
	typeschema(Type, Attrs, ATs, Fixed, _, ParentType), ParentType \== [],
	gettypeschemaattributes(ParentType, [Attrs | AccAttrs], TotalAttrs,
	[ATs | AccATs], TotalATs, [Fixed | AccFixed], TotalFixed).

getbaseancestor(BaseType, BaseType) :-
	BaseType =.. [_, base], !.
getbaseancestor(Type, BaseType) :-
	getancestortypes(Type, TypeChain),
	member(BaseType, TypeChain),
	BaseType =.. [_, base].

/*getancestortypes(inputtype, chain of ancestors - list)*/
/* Assumes single ancestor per type (i.e. only single inheritance) */
getancestortypes(Type, TypeChain) :-
	getancestortypes(Type, [], TC1),
	flatten(TC1, TypeChain).

getancestortypes(Type, TypeChain, TypeChain) :-
	typeschema(Type, _, _, _, _, []).
getancestortypes(Type, AccChain, TotalChain) :-
	typeschema(Type, _, _, _, _, ParentType), ParentType \== [],
	getancestortypes(ParentType, [ParentType | AccChain], TotalChain).

/*getdescendanttypes(inputtype, list of all descendant types)*/
/*input type may be a list of types */
getdescendanttypes(Type, TypeChain) :-
	getdescendanttypes(Type, [], TC1),
	flatten(TC1, TypeChain).

getdescendanttypes([], AllDTypes, AllDTypes).
getdescendanttypes(Type, AllDTypes, AllDTypes) :-
	not_is_list(Type),
	findall(X, (typeschema(X, _, _, _, _, Type)), []).
getdescendanttypes(Type, AccDTypes, AllDTypes) :-
	not_is_list(Type),
	findall(D, (typeschema(D, _, _, _, _, Type)), DTypes),
	DTypes \== [],
	getdescendanttypes(DTypes, [DTypes | AccDTypes], AllDTypes).
getdescendanttypes(TypeList, AllDTypes, AllDTypes) :-
	is_list(TypeList), TypeList \== [],
	findall(D, (member(T, TypeList),typeschema(D,_,_,_,_,T)), []).
getdescendanttypes(TypeList, AccDTypes, AllDTypes) :-
	is_list(TypeList), TypeList \== [],
	findall(D, (member(T, TypeList),typeschema(D,_,_,_,_,T)), DTypes),
	DTypes \== [],
	getdescendanttypes(DTypes, [DTypes | AccDTypes], AllDTypes).

	
/*************************************************************************/
/* Determine whether a particular attribute value is valid. */
validattributevalues(_, [], []) :- !.
validattributevalues(Type, [Attr | ARest], [Val | VRest]) :-
	gettypeschemaattributes(Type, AllAttrs, AllAttrTypes, _),
	nth1(Place, AllAttrs, Attr),
	nth1(Place, AllAttrTypes, AttrType),
	validattributevalue(Attr, AttrType, Val), !,
	validattributevalues(Type, ARest, VRest).

/*validattributevalue(name, type, value - LIST! )*/
validattributevalue(_, atom, [Value]) :-
	atom(Value).
validattributevalue(_, atom(_), [Value]) :-
	atom(Value).
validattributevalue(_, atomset, Value) :-
	is_set(Value), atomlist(Value).
validattributevalue(_, atomset(_), Value) :-
	is_set(Value), atomlist(Value).
validattributevalue(_, atomlist, Value) :-
	atomlist(Value).
validattributevalue(_, atomlist(_), Value) :-
	atomlist(Value).
validattributevalue(_, tupleset(Length), Value) :-
	is_set(Value), tuplelist(Length, Value).
validattributevalue(_, tuplelist(Length), Value) :-
	tuplelist(Length, Value).
validattributevalue(_, nonnegative, [Value]) :-
	integer(Value), Value >= 0.
validattributevalue(_, positive, [Value]) :-
	integer(Value), Value > 0.
validattributevalue(_, percentage, [Value]) :-
	integer(Value), Value >= 0, Value =< 100.
validattributevalue(_, boolean, [Value]) :-
	Value == 'true' ; Value == 'false'.
validattributevalue(_, enum(EType), [Value]) :-
	enum(EType, Enumeration), memberchk(Value, Enumeration).
validattributevalue(Name, Type, Value) :-
	write('ERROR: Invalid attribute value.'), nl,
	write('Attribute: '), write(Name), nl,
	write('Type: '), write(Type), nl,
	write('Value: '), write(Value), nl, nl,
	!, fail.

atomlist([]) :- !.
atomlist([H|T]) :- atom(H), atomlist(T).

tuplelist(_, []) :- !.
tuplelist(Length, [H | T]) :-
	is_list(H), length(H, Length), tuplelist(Length, T).

/* Distributed communications mechanism */
enum(discommmech, [messagepassing, remoteprocedurecall, distributedsharedmemory, workqueue]).
/* Type of CPU */
enum(cputype, [sparc, '80x86', '680x0', powerpc]).
/* Implementation language */
enum(implemlang, [c, cplusplus, ada, pascal, prolog, lisp, fortran, 
	cobol, smalltalk]).
/* Operating system */
enum(os, [sunos, solaris, aix, mswindows, dos, os2, system75]).
/* Graphical User Interface */
enum(gui, [xwindowsystem, mswindows, macintosh, os2]).
/* Port input/output type */
enum(iotype, [ioin, ioout, ioinout]).
/* General purpose indicator for controlcomponent or object */
enum(cosubtype, [ctype, otype]).
/* Directionality for data transfers */
enum(directionality, [forward, reverse, twoway]).
/* Datatypes */
enum(datatype, [integer, float, structure, array, string]).


/*************************************************************************/
/* Assign parents to substructures */
/* (ParentName, ParentType, Listof Parent Attrs, ValueLists of former) */
/* Algorithm:
- For each attribute A:
	- Find position of A in variable attributes list,
	- Use position to get the type of A,
	- If it is a substructure, then
		1. determine what kind of substructure it is
		2. get the value of the attribute A (i.e. the children)
		3. make the children links
	- Else go on to next attribute
*/
assignchildren(_, _, [], []) :- !.
assignchildren(PName, PType, [PAttr | RestPAttrs], [PVList | RestPVLists]) :-
	gettypeschemaattributes(PType, PAllVarAttrs, PAllVarAttrTypes, _),
	nth1(Place, PAllVarAttrs, PAttr),
	nth1(Place, PAllVarAttrTypes, PAttrType),
	substructural(PAttrType, PossibleChildTypes),
	makechildren(PName, PossibleChildTypes, PVList), !,
	assignchildren(PName, PType, RestPAttrs, RestPVLists).
assignchildren(PName, PType, [_ | RestPAttrs], [_ | RestPVLists]) :-
	assignchildren(PName, PType, RestPAttrs, RestPVLists).
assignchildren(PName, PType, PAttrs, PVLists) :-
	write('ERROR: Unable to assign children links'), nl,
	write('Parent: '), write(PName), nl,
	write('Parent Type: '), write(PType), nl,
	write('Attributes: '), write(PAttrs), nl,
	write('ValueLists: '), write(PVLists), nl, nl,
	!, fail.

substructural(atom(substructure(Types)), Types).
substructural(atomlist(substructure(Types)), Types).
substructural(atomset(substructure(Types)), Types).

/* Establish 'links' from unparented children to parents */
/* Note that if the children already have parents, they are left alone */
makechildren(_, _, []) :- !.
makechildren(ParentName, PossibleChildTypes, [ChildName | OtherChildren]) :-
	makechild(ParentName, PossibleChildTypes, ChildName), !,
	makechildren(ParentName, PossibleChildTypes, OtherChildren).

/* If the child has a parent, it is left alone.  Thus the program should
ensure that the most direct possible parents for a child are used */
makechild(ParentName, PossibleTypes, ChildName) :-
	/* Below, we get the descendant types to allow for derived type declarations */
	getdescendanttypes(PossibleTypes, PossibleDescendantTypes),
	union([PossibleTypes, PossibleDescendantTypes], AllPossibleTypes),
	/*msg(16, 'AllPossibleTypes', AllPossibleTypes),*/
	getactualtype(ChildName, AllPossibleTypes, ChildType), /* also ensures existence */
	/*msg(16, 'ChildType', ChildType),*/
	ChildType =.. [ChildEntity, _],
	CheckForParent =.. [ChildEntity, _, ChildName, parent, _],
	(call(CheckForParent) ->
		true;
		updateattributes(ChildEntity, added, ChildName, [parent], [[ParentName]])
	), !.
makechild(ParentName, PossibleChildTypes, ChildName) :-
	write('ERROR: Reference to an entity which is either undeclared or of unknown type.'), nl,
	write('Parent: '), write(ParentName), nl,
	write('Possible Child Types: '), write(PossibleChildTypes), nl,
	write('Child Name: '), write(ChildName), nl, nl,
	!, fail.

getactualtype(Name, [Type | _], Type) :-
	Type =.. [Entity, Style],
	NameFact =.. [Entity, _, _, name, [Name]],
	call(NameFact),
	StyleFact =.. [Entity, _, Name, style, [Style]],
	call(StyleFact),
	!.
getactualtype(Name, [_ | OtherTypes], Type) :-
	getactualtype(Name, OtherTypes, Type), !.

/*************************************************************************/
/* Determine if a list of entities are owned or not by a common parent */
/* TEMPORARILY DISABLED */
owned(_,_,_).
/*owned([], _, _).
owned([Name | T], Entity, Parents) :-
	member(Parent, Parents),
	Fact =.. [Entity, _, Name, parent, [Parent]],
	call(Fact), !, owned(T, Entity, Parents).*/

/*************************************************************************/
/* Check if a list of entities is declared and owned */
declaredandowned(EntityList, EntityType, Parent) :-
	declaredandowned(EntityList, EntityType, Parent,
	('Entity not declared', EntityType),
	('Entity has incorrect system parent', EntityType)).

declaredandowned([], _, _, _, _) :- !.
declaredandowned([unspecified], _, _, _, _) :- !.
declaredandowned([HE | TE], Entity, Parent, Msg1, Msg2) :-
	alltypes(AllTypes),
	(getactualtype(HE, AllTypes, ActualType) -> /* also ensures existence */
		(ActualType =.. [ActualEntity, _],
		(owned([HE], ActualEntity, [Parent]) ->
			true;
			zz(Msg2, Parent, HE)
		))
	;
		zz(Msg1, Parent, HE)
	),
	declaredandowned(TE, Entity, Parent, Msg1, Msg2).

/*declaredandowned(EntityList, EntityType, Parent, Msg1, Msg2) :-
	(indatabase(EntityList, EntityType) ->
		true;
		zz(Msg1, Parent, EntityList)
	),
	(owned(EntityList, EntityType, [Parent]) ->
		true;
		zz(Msg2, Parent, EntityList)
	).*/

/*************************************************************************/
/* checkfortypemismatch(system, attribute, expected attribute type, error)
Will check that the attribute(s) are of the expected type. Will report
error if the type(s) are different from expected. */
checkfortypemismatch(_, unspecified, _, _) :- !.
checkfortypemismatch(_, [unspecified], _, _) :- !.
checkfortypemismatch(SystemName, AttributeName, Type, Msg) :-
	not_is_list(AttributeName),
	(confirmtype(Type, AttributeName) ->
		true;
		zz(Msg, SystemName, AttributeName)
	), !.
checkfortypemismatch(SystemName, AttributeNames, Type, Msg) :-
	is_list(AttributeNames),
	(mapfixtovar(confirmtype, Type, AttributeNames) ->
		true;
		zz(Msg, SystemName, AttributeNames)
	), !.

/*************************************************************************/
/* Database rules */

fetch(_, _, [unspecified], _, []) :- !.
fetch(Entity, Status, EntityList, Attribute, FlatSet) :-
	findall(X, (member(E,EntityList),getvalue(Entity,Status,E,Attribute,X),
	X \== [unspecified]), O1), flatten(O1, O2),
	remove_dups(O2, FlatSet).

/* Retrieves a set of attribute values */
getvalues(_, _, _, [], []).
getvalues(Entity, Status, Name, [HA | TA], [HV | TV]) :-
	getvalue(Entity, Status, Name, HA, HV),
	getvalues(Entity, Status, Name, TA, TV).

/* Checks for presence of a record, if not then it returns unspecified */
getvalue(Entity, Status, Name, Attribute, ValueList) :-
	nonvar(Entity), nonvar(Name), nonvar(Attribute),
	ValueList == [unspecified],
	Fact =.. [Entity, Status, Name, Attribute, _],
	call(Fact), !, fail.
getvalue(Entity, Status, Name, Attribute, ValueList) :-
	nonvar(Entity), nonvar(Name), nonvar(Attribute),
	Fact =.. [Entity, Status, Name, Attribute, ValueList],
	call(Fact), !.
getvalue(Entity, _, Name, Attribute, [unspecified]) :-
	nonvar(Entity), nonvar(Name), nonvar(Attribute),
	indatabase([Name], Entity), !.
/*next three rules deal with an entity which is actually derived*/
getvalue(Entity, Status, Name, Attribute, ValueList) :-
	nonvar(Entity), nonvar(Name), nonvar(Attribute),
	ValueList == [unspecified],
	alltypes(AllTypes),
	getactualtype(Name, AllTypes, ActualType), /* also ensures existence */
	ActualType =.. [ActualEntity, _],
	Fact =.. [ActualEntity, Status, Name, Attribute, _],
	call(Fact), !, fail.
getvalue(Entity, Status, Name, Attribute, ValueList) :-
	nonvar(Entity), nonvar(Name), nonvar(Attribute),
	alltypes(AllTypes),
	getactualtype(Name, AllTypes, ActualType), /* also ensures existence */
	ActualType =.. [ActualEntity, _],
	Fact =.. [ActualEntity, Status, Name, Attribute, ValueList],
	call(Fact), !.
getvalue(Entity, _, Name, Attribute, [unspecified]) :-
	nonvar(Entity), nonvar(Name), nonvar(Attribute).
/*	alltypes(AllTypes),
	getactualtype(Name, AllTypes, _).  also ensures existence */

/*
1. pass it unspecified - fact is unspec
2. pass it variable - fact is unspec
3. pass it unspec - fact is in there
4. pass it variable - fact is not there
*/

getinformalname(Type, Attribute, InformalName) :-
	informalnames(Type, InformalNames),
	gettypeschemaattributes(Type, Attributes, _, _),
	nth1(Place, Attributes, Attribute),
	nth1(Place, InformalNames, InformalName).

removefromdatabase(Name, Type) :-
	Type =.. [Entity, _],
	retractlist([Entity, _, Name, _, _]), !.

indatabase([]) :- !.
indatabase([Name | T]) :-
	typeschema(Type, _, _, _, _, _),
	Type =.. [Entity, _],
	Fact =.. [Entity, _, Name, _, _],
	call(Fact), !,
	indatabase(T).

indatabase([], _) :- !.
indatabase([Name | T], Entity) :-
	Fact =.. [Entity, _, Name, _, _],
	call(Fact), !,
	indatabase(T, Entity).

confirmtype(Type, Name) :-
	Type =.. [Entity, Style],
	NameFact =.. [Entity, _, _, name, [Name]],
	call(NameFact),
	StyleFact =.. [Entity, _, Name, style, [Style]],
	call(StyleFact).

getallentitiesderived(Type, Entities) :-
	getdescendanttypes(Type, DescendantTypes),
	append([Type], DescendantTypes, AllTypes),
	findall(EList, (member(T, AllTypes), getallentities(T, EList)), EntityLists),
	flatten(EntityLists, Entities).

getallentities(Type, Entities) :-
	Type =.. [Entity, Style],
	findall(Name, (Fact =.. [Entity,_,Name,style,[Style]],call(Fact)), Entities).

validtype(Type) :-
	typeschema(Type, _, _, _, _, _).

/*addtodatabase(name, type, entered variable attributes, values of former)*/
/* assumes variable attributes have already been recognized, values checked */
/* Examples: name, style, OWNER, variable attributes, fixed attributes */
addtodatabase(Name, Type, VarAttrs, VarValues) :-
	typeschema(Type, _, _, FixedAttrs, FixedValues, _),
	Type =.. [Entity, Style],
	updateattributes(Entity, added, Name, [name, style], [[Name], [Style]]),
	updateattributes(Entity, added, Name, VarAttrs, VarValues),
	updateattributes(Entity, added, Name, FixedAttrs, FixedValues).

purge :-
	typeschema(Type, _, _, _, _, _), Type =.. [Entity, _],
	retractlist([Entity, _, _, _, _]), fail.
purge.

/* updateattributes(entity, status, name, attributes, values of former) */
updateattributes(_, _, _, [], []) :- !.
updateattributes(Entity, Status, Name, [Attr | RestAttr], [Val | RestVal]) :-
	retractlist([Entity, _, Name, Attr, _]),
	assertlist([Entity, Status, Name, Attr, Val]),
	updateattributes(Entity, Status, Name, RestAttr, RestVal).

retractlist([Entity, Status, Name, Attribute, ValueList]) :-
	Fact =.. [Entity, Status, Name, Attribute, ValueList],
	retractall(Fact), !.

assertlist([Entity, Status, Name, Attribute, ValueList]) :-
	Fact =.. [Entity, Status, Name, Attribute, ValueList],
	assert(Fact), !.

