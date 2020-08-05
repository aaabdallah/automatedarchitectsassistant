/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

/*
Check that all subsystems are not part of any grouping input already
Add new group to database under different name?
Create new system
Mark all subsystems as part of new grouping
*/

validentity(group(base), GroupName) :-
	atom_chars(GroupName, GroupNameList),
	append([103,114,111,117,112,95,102,111,114,95], SystemNameList, GroupNameList),
	atom_chars(SystemName, SystemNameList),

	removefromdatabase(SystemName, system(base)),
	processgroup(GroupName, group(base), SystemName).

processgroup(GroupName, group(base), Name) :-
	getvalues(group, _, GroupName,
	[subsystems, newcalls, newspawns, extendedshareddata,
	newdataconnectors,
	newcall_layers, newspawn_layers, newdataconnector_layers],
	[Subsystems, Newcalls, Newspawns, Extendedshareddata,
	Newdataconnectors,
	Newcall_layers, Newspawn_layers, Newdataconnector_layers]),

	fetch(system, _, Subsystems, initialcontrolcomponents, S1),
	delete(S1, [], S_Initialcontrolcomponents),
	fetch(system, _, Subsystems, controlcomponents, S2),
	delete(S2, [], S_Controlcomponents),
	fetch(system, _, Subsystems, globalobjects, S3),
	delete(S3, [], S_Globalobjects),
	fetch(system, _, Subsystems, calls, S4),
	delete(S4, [], S_Calls),
	fetch(system, _, Subsystems, spawns, S5),
	delete(S5, [], S_Spawns),
	fetch(system, _, Subsystems, dataconnectors, S6),
	delete(S6, [], S_Dataconnectors),
	findall(X, (member(S,Subsystems),getvalue(system,_,S,shareddata,X),
	X \== [unspecified],X \== []), A1), union(A1, A1u),
	remove_dups(A1u, S_Shareddata),
	findall(X, (member(S,Subsystems),getvalue(system,_,S,call_layers,X),
	X \== [unspecified],X \== []), A2), union(A2, A2u),
	remove_dups(A2u, S_Call_layers),
	(Newcall_layers == [unspecified] ->
		Newsystemcall_layers = S_Call_layers;
		union([S_Call_layers, Newcall_layers], Newsystemcall_layers)
	),
	findall(X, (member(S,Subsystems),getvalue(system,_,S,spawn_layers,X),
	X \== [unspecified],X \== []), A3), union(A3, A3u),
	remove_dups(A3u, S_Spawn_layers),
	(Newspawn_layers == [unspecified] ->
		Newsystemspawn_layers = S_Spawn_layers;
		union([S_Spawn_layers, Newspawn_layers], Newsystemspawn_layers)
	),
	findall(X, (member(S,Subsystems),getvalue(system,_,S,dataconnector_layers,X),
	X \== [unspecified],X \== []), A4), union(A4, A4u),
	remove_dups(A4u, S_Dataconnector_layers),
	(Newdataconnector_layers == [unspecified] ->
		Newsystemdataconnector_layers = S_Dataconnector_layers;
		union([S_Dataconnector_layers, Newdataconnector_layers], Newsystemdataconnector_layers)
	),
	fetch(system, _, Subsystems, recognizedmessages, S7),
	delete(S7, [], S_Recognizedmessages),
	fetch(system, _, Subsystems, triggers, S8),
	delete(S8, [], S_Triggers),
	fetch(system, _, Subsystems, nodes, S9),
	delete(S9, [], S_Nodes),
	getallcontrolcomponents(S_Controlcomponents, S_Globalobjects, AllCCs),
	getallobjects(S_Controlcomponents, S_Globalobjects, AllOBs),

	((Newcalls == [unspecified] ; S_Calls == []) ->
		true;
		(intersect(Newcalls,S_Calls) ->
			zz('New calls and subsystem calls must be different', Name,
			(Newcalls,S_Calls));
			true
		)
	),
	((Newspawns == [unspecified] ; S_Spawns == []) ->
		true;
		(intersect(Newspawns,S_Spawns) ->
			zz('New spawns and subsystem spawns must be different', Name,
			(Newspawns,S_Spawns));
			true
		)
	),
	((Newdataconnectors == [unspecified] ; S_Dataconnectors == []) ->
		true;
		(intersect(Newdataconnectors,S_Dataconnectors) ->
			zz('New dataconnectors and subsystem dataconnectors must be different', Name,
			(Newdataconnectors,S_Dataconnectors));
			true
		)
	),
	(Newcalls == [unspecified] ->
		true;
		(findall(X,(member(C,Newcalls),getvalue(controlconnector,_,C,blocking,X),
		X \== [unspecified]),CB),
		flatten(CB,CB2), remove_dups(CB2, CB3),
		(subset(CB3, [true]) ->
			true;
			zz('Bridging call is non-blocking', Name, Newcalls))
		)
	),
	(Newspawns == [unspecified] ->
		true;
		(findall(X,(member(C,Newspawns),getvalue(controlconnector,_,C,blocking,X),
		X \== [unspecified]),SB),
		flatten(SB,SB2), remove_dups(SB2, SB3),
		(subset(SB3, [false]) ->
			true;
			zz('Bridging spawn is blocking', Name, Newspawns))
		)
	),
	(Newcalls == [unspecified] ->
		true;
		checkifbridging(Name, controlconnector, Newcalls, Subsystems)
	),
	(Newspawns == [unspecified] ->
		true;
		checkifbridging(Name, controlconnector, Newspawns, Subsystems)
	),
	(Newdataconnectors == [unspecified] ->
		true;
		checkifbridging(Name, dataconnector, Newdataconnectors, Subsystems)
	),
	(Extendedshareddata == [unspecified] ->
		true;
		(findall(X, (member(X,Extendedshareddata),
		validextendedshareddata(Subsystems,X,AllCCs,AllOBs)),ValidatedESDs),
		(seteq(Extendedshareddata, ValidatedESDs) ->
			true;
			zz('Group extended shared data not consistent with subsystems',
			Name, (Extendedshareddata, ValidatedESDs, Subsystems))
		))
	),
	(Newcalls == [unspecified] ->
		Newsystem_calls = S_Calls;
		union([S_Calls, Newcalls], Newsystem_calls)
	),
	(Newspawns == [unspecified] ->
		Newsystem_spawns = S_Spawns;
		union([S_Spawns, Newspawns], Newsystem_spawns)
	),
	(Newdataconnectors == [unspecified] ->
		Newsystem_dataconnectors = S_Dataconnectors;
		union([S_Dataconnectors, Newdataconnectors], Newsystem_dataconnectors)
	),
	(Extendedshareddata == [unspecified] ->
		Newsystem_shareddata = S_Shareddata;
		(findall([D,CCs,OBs], (member([D,CCs,OBs],S_Shareddata),
		nonmember([D,_,_],Extendedshareddata)), StrippedSharedData),
		append(StrippedSharedData, Extendedshareddata, Newsystem_shareddata))
	),
	getmaxthreads(S_Initialcontrolcomponents, [], Newsystem_spawns, Newsystem_calls, Threads),
	(invalidmultidctothread(Threads, Newsystem_dataconnectors, TL1, TL2, TL3, TD1, TD2) ->
		zz('Two threads communicating with a third at multiple conflicting points',
		Name, (TL1, TL2, TL3, TD1, TD2));
		true
	),
	(threadssharingdata(Threads, Newsystem_shareddata,
	[TSDD, TSDCCs, TSDOBs], TSDC1, TSDL1, TSDC2, TSDL2) ->
		zz('Two independent threads sharing data', Name,
		([TSDD, TSDCCs, TSDOBs], TSDC1, TSDL1, TSDC2, TSDL2));
		true
	),
	(Newcall_layers == [unspecified] ->
		(Newcall_layers_tops = [], Newcall_layers_bottoms = []);
		(findall(X, member([X,_],Newcall_layers), Clt),
		flatten(Clt, Clt1), remove_dups(Clt1, Newcall_layers_tops),
		findall(X, member([_,X],Newcall_layers), Clb),
		flatten(Clb, Clb1), remove_dups(Clb1, Newcall_layers_bottoms))
	),
	union([Newcall_layers_tops, Newcall_layers_bottoms], Newcall_layers_ccs),
	(Newspawn_layers == [unspecified] ->
		(Newspawn_layers_tops = [], Newspawn_layers_bottoms = []);
		(findall(X, member([X,_],Newspawn_layers), Slt),
		flatten(Slt, Slt1), remove_dups(Slt1, Newspawn_layers_tops),
		findall(X, member([_,X],Newspawn_layers), Slb),
		flatten(Slb, Slb1), remove_dups(Slb1, Newspawn_layers_bottoms))
	),
	union([Newspawn_layers_tops, Newspawn_layers_bottoms], Newspawn_layers_ccs),
	(Newdataconnector_layers == [unspecified] ->
		(Newdataconnector_layers_tops = [], Newdataconnector_layers_bottoms = []);
		(findall(X, member([X,_],Newdataconnector_layers), Dlt),
		flatten(Dlt, Dlt1), remove_dups(Dlt1, Newdataconnector_layers_tops),
		findall(X, member([_,X],Newdataconnector_layers), Dlb),
		flatten(Dlb, Dlb1), remove_dups(Dlb1, Newdataconnector_layers_bottoms))
	),
	union([Newdataconnector_layers_tops, Newdataconnector_layers_bottoms],
	Newdataconnector_layers_ccs),
	(subset(Newcall_layers_ccs, AllCCs) ->
		true;
		zz('Call layers refer to component outside of group', Name,
		(Newcall_layers_ccs, AllCCs))
	),
	(subset(Newspawn_layers_ccs, AllCCs) ->
		true;
		zz('Spawn layers refer to component outside of group', Name,
		(Newspawn_layers_ccs, AllCCs))
	),
	(subset(Newdataconnector_layers_ccs, AllCCs) ->
		true;
		zz('Dataconnector layers refer to component outside of group', Name,
		(Newdataconnector_layers_ccs, AllCCs))
	),
	(Newcalls == [unspecified] ->
		true;
		(findall([X,Y],(member(C,Newcalls),getvalue(controlconnector,_,C,c1,[X]),
		X \== unspecified,getvalue(controlconnector,_,C,c2,[Y]),Y \== unspecified),CallC12s),
		(layered(CallC12s, Newsystemcall_layers) ->
			true;
			zz('Bridging call breaks layering constraint', Name, (Newcalls,Newsystemcall_layers))
		))
	),
	(Newspawns == [unspecified] ->
		true;
		(findall([X,Y],(member(C,Newspawns),getvalue(controlconnector,_,C,c1,[X]),
		X \== unspecified,getvalue(controlconnector,_,C,c2,[Y]),Y \== unspecified),SpawnC12s),
		(layered(SpawnC12s, Newsystemspawn_layers) ->
			true;
			zz('Bridging spawn breaks layering constraint', Name, (Newspawns,Newsystemspawn_layers))
		))
	),
	(Newdataconnectors == [unspecified] ->
		true;
		(findall([X,Y],(member(C,Newdataconnectors),getvalue(dataconnector,_,C,c1,[X]),
		X \== unspecified,getvalue(dataconnector,_,C,c2,[Y]),Y \== unspecified),DataconnectorC12s),
		(layered(DataconnectorC12s, Newsystemdataconnector_layers) ->
			true;
			zz('Bridging dataconnector breaks layering constraint', 
			Name, (Newdataconnectors,Newsystemdataconnector_layers))
		))
	),
	(identicalsets(S_Recognizedmessages) ->
		true;
		zz('Group subsystems have different sets of recognized messages',
		Name, (Subsystems,S_Recognizedmessages))
	),
	fetch(event, _, S_Recognizedmessages, type, Recognizedmessagetypes),
	length(Recognizedmessagetypes, RmtL),
	(RmtL =< 1 ->
		true;
		zz('Group messages use different formats for passing data',
		Name, (S_Recognizedmessages, Recognizedmessagetypes))
	),

	PossibleAttributes = [initialcontrolcomponents, controlcomponents, globalobjects,
	calls, spawns, dataconnectors, shareddata,
	call_layers, spawn_layers, dataconnector_layers,
	recognizedmessages, triggers, nodes],

	PossibleValues = [S_Initialcontrolcomponents, S_Controlcomponents, S_Globalobjects,
	Newsystem_calls, Newsystem_spawns, Newsystem_dataconnectors, Newsystem_shareddata,
	Newsystemcall_layers, Newsystemspawn_layers, Newsystemdataconnector_layers,
	S_Recognizedmessages, S_Triggers, S_Nodes],

	collectnonemptyvalues(PossibleAttributes, PossibleValues, FinalAttributes, NonemptyValues),
	addtodatabase(Name, system(base), FinalAttributes, NonemptyValues).

/* validextendedshareddata checks if the extended shared data is valid */
validextendedshareddata(Subsystems, [D, CCList, OBList], AllCCs, AllOBs) :-
	member(S,Subsystems),
	getvalue(system, _, S, shareddata, Oldsd),
	member([D, OldCCList, OldOBList], Oldsd), !,
	subset(OldCCList, CCList),
	subset(OldOBList, OBList),
	subset(CCList, AllCCs),
	subset(OBList, AllOBs).
validextendedshareddata(_, _, _, _).

checkifbridging(_, _, [], _) :- !.
checkifbridging(Name, ConnectorEntity, [E | OtherEs], Subsystems) :-
	getvalue(ConnectorEntity, _, E, c1, [C1]),
	getvalue(ConnectorEntity, _, E, c2, [C2]),
	C1 \== unspecified, C2 \== unspecified,
	findall(X, (member(X,Subsystems),getallcontrolcomponents(X,CCs),
	member(C1,CCs)), C1SystemList),
	findall(X, (member(X,Subsystems),getallcontrolcomponents(X,CCs),
	member(C2,CCs)), C2SystemList),
	length(C1SystemList, L1),
	length(C2SystemList, L2),
	((L1 == 1, L2 == 1) ->
		((C1SystemList \== C2SystemList) ->
			true;
			zz('Bridging connector must traverse two distinct subsystems', Name,
			(E, C1SystemList, Subsystems))
		);
		((L1 == 1 ->
			true;
			zz('Connector has c1 which is not found or found in multiple subsystems',
			Name, (E, C1, C1SystemList, Subsystems))
		),
		(L2 == 1 ->
			true;
			zz('Connector has c2 which is not found or found in multiple subsystems',
			Name, (E, C2, C2SystemList, Subsystems))
		))
	),
	checkifbridging(Name, ConnectorEntity, OtherEs, Subsystems), !.
checkifbridging(Name, ConnectorEntity, [_ | OtherEs], Subsystems) :-
	checkifbridging(Name, ConnectorEntity, OtherEs, Subsystems), !.

/* invalidmultidctothread(Threads, Dataconnectors) checks if a set of threads
with dataconnectors going between threads has a mismatch where two
different threads have dataconnectors going to two different controlcomponents
in a third different thread */
invalidmultidctothread(Threads, Dataconnectors, L1, L2, L3, D1, D2) :-
	member(D1, Dataconnectors), member(D2, Dataconnectors), D1 \== D2,
	getvalue(dataconnector, _, D1, c1, [D1C1]), D1C1 \== unspecified,
	getvalue(dataconnector, _, D1, c2, [D1C2]), D1C2 \== unspecified,
	getvalue(dataconnector, _, D2, c1, [D2C1]), D2C1 \== unspecified,
	getvalue(dataconnector, _, D2, c2, [D2C2]), D2C2 \== unspecified,
	member(L1, Threads), member(L2, Threads), member(L3, Threads),
	L1 \== L2, L2 \== L3, L3 \== L1,
	member(C1, L1), member(C2a, L2), member(C2b, L2), C2a \== C2b, member(C3, L3),
	((D1C1 == C1, D1C2 == C2a);(D1C1 == C2a, D1C2 == C1)),
	((D2C1 == C3, D2C2 == C2b);(D2C1 == C2b, D2C2 == C3)).

threadssharingdata(Threads, Shareddata, [D, CCs, OBs], C1, L1, C2, L2) :-
	member([D, CCs, OBs], Shareddata),
	member(C1, CCs), member(C2, CCs), C1 \== C2,
	member(L1, Threads), member(L2, Threads), L1 \== L2,
	member(C1, L1), member(C2, L2).

collectnonemptyvalues(PossibleAttributes, PossibleValues, FinalAttributes, NonemptyValues) :-
	collectnonemptyvalues(PossibleAttributes, PossibleValues, [], FinalAttributes,
	[], NonemptyValues).

collectnonemptyvalues([], [], AList, AList, VList, VList) :- !.
collectnonemptyvalues([_ | RestAttributes], [[] | RestValues], 
	AAccum, ATotal, VAccum, VTotal) :-
	collectnonemptyvalues(RestAttributes, RestValues, AAccum, ATotal, VAccum, VTotal), !.
collectnonemptyvalues([A | RestAttributes], [V | RestValues],
	AAccum, ATotal, VAccum, VTotal) :-
	collectnonemptyvalues(RestAttributes, RestValues, [A | AAccum], ATotal,
	[V | VAccum], VTotal), !.

getgroupname(Name, GroupName) :-
	atom_chars(Name, NameList),
	/* Append 'group_for_' */
	append([103,114,111,117,112,95,102,111,114,95], NameList, GroupNameList),
	atom_chars(GroupName, GroupNameList).

systemsnotgrouped(Systems) :-
	getallentities(group(_), Groups),
	fetch(group, _, Groups, subsystems, GroupSubsystems),
	disjoint(Systems, GroupSubsystems).

/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(group(base),
	[subsystems, newcalls,
	newspawns, newdataconnectors,
	extendedshareddata, 
	newcall_layers, newspawn_layers, newdataconnector_layers],
	[atomset(substructure([system(base)])), atomset(substructure([controlconnector(base)])),
	atomset(substructure([controlconnector(base)])), 
	atomset(substructure([dataconnector(base)])),
	tupleset(3), tupleset(2), tupleset(2), tupleset(2)],
	[],
	[],
	[]).

informalnames(group(base),
	['Subsystems                  ', 
	'New Bridging Calls          ', 
	'New Bridging Spawns         ',
	'New Bridging Data Connectors', 
	'Extended Shared Data        ',
	'New Call Layers             ', 
	'New Spawn Layers            ', 
	'New Data Connector Layers   ']).

