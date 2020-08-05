/* :- compile(library(sets)). */

/* alter attributes list, alter validentity predicates */
:- multifile(typeschema/6).
:- multifile(informalnames/2).
:- multifile(validentity/2).
:- multifile(style/2).

style(base, 'Style-less').

/* Check constraints on an entity */
/* validentity(entity(style), name of entity) */
/*  NOTE: IN ALL CASES WHERE THE DATABASE IS CHECKED FOR AN ATTRIBUTE,
    MAKE SURE TO ALLOW FOR POSSIBILITY THAT THE ATTRIBUTE WAS NOT
    ENTERED BY THE USER (A PARTIAL DESCRIPTION)!*/

/*validentity(, Name) :-
	indatabase([Name]),
*	confirmtype(, Name),*
	validentity(, Name).
*/

/* Missing (?): circuit creation and destruction */
validentity(port(base), Name) :-
	indatabase([Name]).

validentity(datacomponent(base), Name) :-
	indatabase([Name]).

validentity(controlcomponent(base), Name) :-
	indatabase([Name]).

validentity(object(base), Name) :-
	indatabase([Name]),
	getvalues(object, _, Name,
	[methods, nodes, platform],
	[Methods, Nodes, Platform]),

	(Methods == [unspecified] ->
		true;
		(findall(M, (member(M,Methods),getvalue(controlcomponent,_,M,method,T),
		(T == [unspecified] ; T == [true])), TrueUnspecifiedMethods),
		findall(N, (member(M,Methods),getvalue(controlcomponent,_,M,nodes,N),
		N \== [unspecified]), AllMethodNodes),
		findall(P, (member(M,Methods),getvalue(controlcomponent,_,M,platform,P),
		P \== [unspecified]), MethodPlatforms),

		(seteq(TrueUnspecifiedMethods, Methods) ->
			true;
			zz('Nonmethod declared as method', Name,
				(TrueUnspecifiedMethods,Methods))
		),
		(Nodes == [unspecified] ->
			true;
			(mapvartofix(subset, AllMethodNodes, Nodes) ->
				true;
				zz('Method nodes not subset of object nodes', Name,
					(AllMethodNodes, Nodes))
			)
		),
		(Platform == [unspecified] ->
			true;
			(mapvartofix(platformcompatible, MethodPlatforms, Platform) ->
				true;
				zz('Incompatible method platform with object platform', Name,
					(MethodPlatforms, Platform))
			)
		))
	).

validentity(dataconnector(base), Name) :-
	indatabase([Name]),
	getvalues(dataconnector, _, Name,
	[ccobj, p1, p2, c1, c2, o1, o2, nodes],
	[CO, [P1], [P2], [C1], [C2], [O1], [O2], Nodes]),

	(CO == [unspecified] ->
		true;
		(((CO == [ctype], C1 \== unspecified, P1 \== unspecified) ->
			(getvalue(controlcomponent,_,C1,ports,C1Ports),
			((C1Ports == [unspecified] ; member(P1,C1Ports)) ->
				true;
				zz('Dataconnector port 1 not found in controlcomponent 1',
					Name, (P1,C1Ports))
			));
			true
		),
		((CO == [ctype], C2 \== unspecified, P2 \== unspecified) ->
			(getvalue(controlcomponent,_,C2,ports,C2Ports),
			((C2Ports == [unspecified] ; member(P2,C2Ports)) ->
				true;
				zz('Dataconnector port 2 not found in controlcomponent 2',
					Name, (P2,C2Ports))
			));
			true
		),
		((CO == [otype], O1 \== unspecified, P1 \== unspecified) ->
			(getvalue(object,_,O1,ports,O1Ports),
			((O1Ports == [unspecified] ; member(P1,O1Ports)) ->
				true;
				zz('Dataconnector port 1 not found in object 1',
					Name, (P1,O1Ports))
			));
			true
		),
		((CO == [otype], O2 \== unspecified, P2 \== unspecified) ->
			(getvalue(object,_,O2,ports,O2Ports),
			((O2Ports == [unspecified] ; member(P2,O2Ports)) ->
				true;
				zz('Dataconnector port 2 not found in object 2',
					Name, (P2,O2Ports))
			));
			true
		))
	),
	((C1 == unspecified ; O1 == unspecified) ->
		true;
		(getvalue(controlcomponent,_,C1,method,MB1),
		getvalue(object,_,O1,methods,O1Methods),
		((MB1 == [unspecified] ; O1Methods == [unspecified]) ->
			true;
			(((MB1 == [true], member(C1,O1Methods));
			(MB1 == [false], nonmember(C1,O1Methods))) ->
				true;
				zz('Controlconnector object incompatible with controlcomponent',
				Name, (MB1,C1,O1Methods))
			)
		))
	),
	((C2 == unspecified ; O2 == unspecified) ->
		true;
		(getvalue(controlcomponent,_,C2,method,MB2),
		getvalue(object,_,O2,methods,O2Methods),
		((MB2 == [unspecified] ; O2Methods == [unspecified]) ->
			true;
			(((MB2 == [true], member(C2,O2Methods));
			(MB2 == [false], nonmember(C2,O2Methods))) ->
				true;
				zz('Controlconnector object incompatible with controlcomponent',
				Name, (MB2,C2,O2Methods))
			)
		))
	),
	((P1 == unspecified ; P2 == unspecified) ->
		true;
		(getvalue(port,_,P1,iotype,[P1iotype]),
		getvalue(port,_,P2,iotype,[P2iotype]),
		((P1iotype == unspecified ; P2iotype == unspecified) ->
			true;
			(((P1iotype == ioin, P2iotype == ioout);
			(P1iotype == ioin, P2iotype == ioinout);
			(P1iotype == ioinout, P2iotype == ioinout);
			(P2iotype == ioin, P1iotype == ioout);
			(P2iotype == ioin, P1iotype == ioinout)) ->
				true;
				zz('Invalid dataconnector ports', Name, (P1iotype, P2iotype))
			))
		)
	),
	(Nodes == [unspecified] ->
		true;
		((C1 == unspecified ->
			true;
			(getvalue(controlcomponent,_,C1,nodes,C1Nodes),
			(C1Nodes == [unspecified] ->
				true;
				(subset(C1Nodes,Nodes) ->
					true;
					zz('Dataconnector component 1 node not in dataconnector nodes',
					Name, (C1Nodes, Nodes))
				)
			))
		),
		(C2 == unspecified ->
			true;
			(getvalue(controlcomponent,_,C2,nodes,C2Nodes),
			(C2Nodes == [unspecified] ->
				true;
				(subset(C2Nodes,Nodes) ->
					true;
					zz('Dataconnector component 2 node not in dataconnector nodes',
					Name, (C2Nodes, Nodes))
				)
			))
		),
		(O1 == unspecified ->
			true;
			(getvalue(controlcomponent,_,O1,nodes,O1Nodes),
			(O1Nodes == [unspecified] ->
				true;
				(subset(O1Nodes,Nodes) ->
					true;
					zz('Dataconnector object 1 node not in dataconnector nodes',
					Name, (O1Nodes, Nodes))
				)
			))
		),
		(O2 == unspecified ->
			true;
			(getvalue(controlcomponent,_,O2,nodes,O2Nodes),
			(O2Nodes == [unspecified] ->
				true;
				(subset(O2Nodes,Nodes) ->
					true;
					zz('Dataconnector object 2 node not in dataconnector nodes',
					Name, (O2Nodes, Nodes))
				)
			))
		))
	).

validentity(controlconnector(base), Name) :-
	indatabase([Name]),
	getvalues(controlconnector, _, Name,
	[c1, c2, o1, o2, arguments, returnvalues, nodes],
	[[C1], [C2], [O1], [O2], Arguments, ReturnValues, Nodes]),

	((Arguments == [unspecified] ; C2 == unspecified) ->
		true;
		(getvalue(controlcomponent,_,C2,arguments,C2Args),
		(seteq(C2Args, Arguments) ->
			true;
			zz('Arguments differ from controlconnector to controlcomponent',
			Name, (Arguments,C2Args))
		))
	),
	((ReturnValues == [unspecified] ; C2 == unspecified) ->
		true;
		(getvalue(controlcomponent,_,C2,returnvalues,C2RetVals),
		(seteq(C2RetVals, ReturnValues) ->
			true;
			zz('Return values differ from controlconnector to controlcomponent',
			Name, (ReturnValues,C2RetVals))
		))
	),
	((C1 == unspecified ; O1 == unspecified) ->
		true;
		(getvalue(controlcomponent,_,C1,method,MB1),
		getvalue(object,_,O1,methods,O1Methods),
		((MB1 == [unspecified] ; O1Methods == [unspecified]) ->
			true;
			(((MB1 == [true], member(C1,O1Methods));
			(MB1 == [false], nonmember(C1,O1Methods))) ->
				true;
				zz('Controlconnector object incompatible with controlcomponent',
				Name, (MB1,C1,O1Methods))
			)
		))
	),
	((C2 == unspecified ; O2 == unspecified) ->
		true;
		(getvalue(controlcomponent,_,C2,method,MB2),
		getvalue(object,_,O2,methods,O2Methods),
		((MB2 == [unspecified] ; O2Methods == [unspecified]) ->
			true;
			(((MB2 == [true], member(C2,O2Methods));
			(MB2 == [false], nonmember(C2,O2Methods))) ->
				true;
				zz('Controlconnector object incompatible with controlcomponent',
				Name, (MB2,C2,O2Methods))
			)
		))
	),
	(Nodes == [unspecified] ->
		true;
		((C1 == unspecified ->
			true;
			(getvalue(controlcomponent,_,C1,nodes,C1Nodes),
			(C1Nodes == [unspecified] ->
				true;
				(subset(C1Nodes,Nodes) ->
					true;
					zz('Dataconnector component 1 node not in dataconnector nodes',
					Name, (C1Nodes, Nodes))
				)
			))
		),
		(C2 == unspecified ->
			true;
			(getvalue(controlcomponent,_,C2,nodes,C2Nodes),
			(C2Nodes == [unspecified] ->
				true;
				(subset(C2Nodes,Nodes) ->
					true;
					zz('Dataconnector component 2 node not in dataconnector nodes',
					Name, (C2Nodes, Nodes))
				)
			))
		),
		(O1 == unspecified ->
			true;
			(getvalue(controlcomponent,_,O1,nodes,O1Nodes),
			(O1Nodes == [unspecified] ->
				true;
				(subset(O1Nodes,Nodes) ->
					true;
					zz('Dataconnector object 1 node not in dataconnector nodes',
					Name, (O1Nodes, Nodes))
				)
			))
		),
		(O2 == unspecified ->
			true;
			(getvalue(controlcomponent,_,O2,nodes,O2Nodes),
			(O2Nodes == [unspecified] ->
				true;
				(subset(O2Nodes,Nodes) ->
					true;
					zz('Dataconnector object 2 node not in dataconnector nodes',
					Name, (O2Nodes, Nodes))
				)
			))
		))
	).

validentity(trigger(base), Name) :-
	indatabase([Name]),
	getvalues(trigger, _, Name,
	[inports, outports, subtype, controlcomponent, object, outcalls, outspawns],
	[Inports, Outports, Subtype, Controlcomponent, Object, Outcalls, Outspawns]),

	(Inports == [unspecified] ->
		true;
		(findall(X, (member(P,Inports),getvalue(port,_,P,iotype,X),X \== [unspecified]), API),
		remove_dups(API, InportIotypes),
		(subset(InportIotypes,[ioin,ioinout]) ->
			true;
			zz('Trigger inports not io_in or io_inout', Name, InportIotypes)
		))
	),
	(Outports == [unspecified] ->
		true;
		(findall(X, (member(P,Outports),getvalue(port,_,P,iotype,X),X \== [unspecified]), APO),
		remove_dups(APO, OutportIotypes),
		(subset(OutportIotypes,[ioout,ioinout]) ->
			true;
			zz('Trigger outports not io_out or io_inout', Name, OutportIotypes)
		))
	),
	(Subtype == [unspecified] ->
		true;
		((Subtype == [ctype] ->
			(Controlcomponent == [unspecified] ->
				true;
				(getvalue(controlcomponent,_,Controlcomponent,ports,CPorts),
				getvalue(controlcomponent,_,Controlcomponent,method,CM),
				(Inports == [unspecified] ->
					true;
					(subset(Inports,CPorts) ->
						true;
						zz('Trigger controlcomponent inports not in controlcomponent ports',
						Name, (Inports, Controlcomponent, CPorts))
					)
				),
				(Outports == [unspecified] ->
					true;
					(subset(Outports,CPorts) ->
						true;
						zz('Trigger controlcomponent outports not in controlcomponent ports',
						Name, (Outports, Controlcomponent, CPorts))
					)
				),
				(CM == [unspecified] ->
					true;
					(CM == [false] ->
						true;
						zz('Trigger controlcomponent is a method', Name,
						(Controlcomponent, CM))
					)
				))
			)
		),
		(Subtype == [otype] ->
			(Object == [unspecified] ->
				true;
				(getvalue(object,_,Object,ports,OPorts),
				getvalue(object,_,Object,methods,OMethods),
				(Inports == [unspecified] ->
					true;
					(subset(Inports,OPorts) ->
						true;
						zz('Trigger object inports not in object ports',
						Name, (Inports, Object, OPorts))
					)
				),
				(Outports == [unspecified] ->
					true;
					(subset(Outports,OPorts) ->
						true;
						zz('Trigger object outports not in object ports',
						Name, (Outports, Object, OPorts))
					)
				),
				((OMethods == [unspecified] ; Outcalls == [unspecified]) ->
					true;
					(subset(Outcalls, OMethods) ->
						true;
						zz('Trigger outcalls not subset of object methods',
						Name, (Outcalls, OMethods))
					)
				),
				(Outspawns == [unspecified] ->
					true;
					zz('Triggered object spawns not allowed',
					Name, (Outspawns))
				))
			)
		))
	).

validentity(system(base), Name) :-
	indatabase([Name]),
	getvalues(system, _, Name,
	[controlcomponents, initialcontrolcomponents, globalobjects, classes,
	initialdataconnectors, dataconnectors, calls, spawns, call_layers,
	spawn_layers, dataconnector_layers, shareddata, triggers, recognizedmessages,
	nodes],
	[Controlcomponents, Initialcontrolcomponents, Globalobjects, Classes,
	Initialdataconnectors, Dataconnectors, Calls, Spawns, Call_layers,
	Spawn_layers, Dataconnector_layers, Shareddata, Triggers, RecognizedMessages,
	Nodes]),

	declaredandowned(Controlcomponents, controlcomponent, Name),
	declaredandowned(Globalobjects, object, Name),
	declaredandowned(Dataconnectors, dataconnector, Name),
	declaredandowned(Calls, controlconnector, Name),
	declaredandowned(Spawns, controlconnector, Name),

	fetch(controlcomponent, _, Controlcomponents, localobjects, Localobjects),
	r_getallobjects(Localobjects, RLocalobjects),
	r_getallmethods(RLocalobjects, Localobjectmethods),
	(Globalobjects == [unspecified] ->
		(Globalobjectmethods = [], Allobjects = RLocalobjects);
		(r_getallmethods(Globalobjects, Globalobjectmethods),
		r_getallobjects(Globalobjects, RGlobalobjects),
		union([RGlobalobjects, RLocalobjects], Allobjects))
	),
	(Controlcomponents == [unspecified] ->
		union([Globalobjectmethods, Localobjectmethods], AllCCs);
		union([Controlcomponents, Globalobjectmethods, Localobjectmethods], AllCCs)
	),
	((Initialcontrolcomponents == [unspecified] ; Globalobjects == [unspecified]) ->
		true;
		(length(Initialcontrolcomponents, LIC), length(Globalobjects, LGO),
		LL is LIC + LGO,
		(LL >= 1 ->
			true;
			zz('System must have at least one initial control component or global object',
			Name, (Initialcontrolcomponents, Globalobjects))
		))
	),
	((Initialcontrolcomponents == [unspecified] ; Controlcomponents == [unspecified]) ->
		true;
		(subset(Initialcontrolcomponents, Controlcomponents) ->
			true;
			zz('System initial control components not found in control components',
			Name, (Initialcontrolcomponents, Controlcomponents))
		)
	),
	(Classes == [unspecified] ->
		true;
		(fetch(object, _, Allobjects, class, Objectclasses),
		(subset(Objectclasses,Classes) ->
			true;
			zz('System object class not found in classes', Name,
			(Allobjects,Objectclasses,Classes))
		))
	),
	((Initialdataconnectors == [unspecified] ;
		Dataconnectors == [unspecified]) ->
		true;
		(subset(Initialdataconnectors, Dataconnectors) ->
			true;
			zz('System initial dataconnector not in dataconnectors', Name,
			(Initialdataconnectors, Dataconnectors))
		)
	),
	(Calls == [unspecified] ->
		true;
		(findall(X,(member(C,Calls),getvalue(controlconnector,_,C,blocking,X),
		X \== [unspecified]),CB),
		flatten(CB,CB2), remove_dups(CB2, CB3),
		(subset(CB3, [true]) ->
			true;
			zz('System call is non-blocking', Name, Calls))
		)
	),
	(Spawns == [unspecified] ->
		true;
		(findall(X,(member(C,Spawns),getvalue(controlconnector,_,C,blocking,X),
		X \== [unspecified]),SB),
		flatten(SB,SB2), remove_dups(SB2, SB3),
		(subset(SB3, [false]) ->
			true;
			zz('System spawn is blocking', Name, Spawns))
		)
	),
	fetch(controlconnector, _, Calls, c1, CallsC1s),
	fetch(controlconnector, _, Calls, c2, CallsC2s),
	(subset(CallsC1s, AllCCs) ->
		true;
		zz('Call.c1 not found in system controlcomponents or object methods',
		Name, (CallsC1s, AllCCs))
	),
	(subset(CallsC2s, AllCCs) ->
		true;
		zz('Call.c2 not found in system controlcomponents or object methods',
		Name, (CallsC2s, AllCCs))
	),
	fetch(controlconnector, _, Spawns, c1, SpawnsC1s),
	fetch(controlconnector, _, Spawns, c2, SpawnsC2s),
	(subset(SpawnsC1s, AllCCs) ->
		true;
		zz('Spawn.c1 not found in system controlcomponents or object methods',
		Name, (SpawnsC1s, AllCCs))
	),
	(subset(SpawnsC2s, AllCCs) ->
		true;
		zz('Spawn.c2 not found in system controlcomponents or object methods',
		Name, (SpawnsC2s, AllCCs))
	),
	(Dataconnectors == [unspecified] ->
		(DataconnectorsO1s = [], DataconnectorsO2s = []);
		(findall(X,(member(C,Dataconnectors),getvalue(dataconnector,_,C,ccobj,CCOBJ1),
		CCOBJ1 == [otype],getvalue(dataconnector,_,C,o1,[X]),X \== unspecified),
		DataconnectorsO1s),
		findall(X,(member(C,Dataconnectors),getvalue(dataconnector,_,C,ccobj,CCOBJ2),
		CCOBJ2 == [otype],getvalue(dataconnector,_,C,o2,[X]),X \== unspecified),
		DataconnectorsO2s))
	),
	(Dataconnectors == [unspecified] ->
		(DataconnectorsC1s = [], DataconnectorsC2s = []);
		(findall(X,(member(C,Dataconnectors),getvalue(dataconnector,_,C,ccobj,CCOBJ1),
		CCOBJ1 == [ctype],getvalue(dataconnector,_,C,c1,[X]),X \== unspecified),
		DataconnectorsC1s),
		findall(X,(member(C,Dataconnectors),getvalue(dataconnector,_,C,ccobj,CCOBJ2),
		CCOBJ2 == [ctype],getvalue(dataconnector,_,C,c2,[X]),X \== unspecified),
		DataconnectorsC2s))
	),
	(subset(DataconnectorsC1s, AllCCs) ->
		true;
		zz('Dataconnector.c1 not found in system controlcomponents or object methods',
		Name, (DataconnectorsC1s, AllCCs))
	),
	(subset(DataconnectorsC2s, AllCCs) ->
		true;
		zz('Dataconnector.c2 not found in system controlcomponents or object methods',
		Name, (DataconnectorsC2s, AllCCs))
	),
	(subset(DataconnectorsO1s, Allobjects) ->
		true;
		zz('Dataconnector.o1 not found in system objects',
		Name, (DataconnectorsO1s, Allobjects))
	),
	(subset(DataconnectorsO2s, Allobjects) ->
		true;
		zz('Dataconnector.o2 not found in system objects',
		Name, (DataconnectorsO2s, Allobjects))
	),
	fetch(dataconnector, _, Dataconnectors, p1, DataconnectorP1s),
	fetch(dataconnector, _, Dataconnectors, p2, DataconnectorP2s),
	fetch(controlcomponent, _, AllCCs, ports, CCports),
	fetch(object, _, Allobjects, ports, Objectports),
	union([CCports,Objectports],Allports),
	(subset(DataconnectorP1s,Allports) ->
		true;
		zz('Dataconnector.p1 not found in system ports',
		Name, (DataconnectorP1s, Allports))
	),
	(subset(DataconnectorP2s,Allports) ->
		true;
		zz('Dataconnector.p2 not found in system ports',
		Name, (DataconnectorP2s, Allports))
	),
	(Shareddata == [unspecified] ->
		true;
		(findall(X, member([X,_,_],Shareddata), ShareddataDS),
		findall(X, member([_,X,_],Shareddata), SdCS),
		flatten(SdCS, SdCS2), remove_dups(SdCS2, ShareddataCS),
		findall(X, member([_,_,X],Shareddata), SdOS),
		flatten(SdOS, SdOS2), remove_dups(SdOS2, ShareddataOS),
		(is_set(ShareddataDS) ->
			true;
			zz('System shareddata refers to same data twice',
			Name, (ShareddataDS))
		),
		(subset(ShareddataCS, AllCCs) ->
			true;
			zz('System shareddata refers to controlcomponent not in system',
			Name, (ShareddataCS, AllCCs))
		),
		(subset(ShareddataOS, Allobjects) ->
			true;
			zz('System shareddata refers to object not in system',
			Name, (ShareddataOS, Allobjects))
		))
	),
	(Call_layers == [unspecified] ->
		(Call_layers_tops = [], Call_layers_bottoms = []);
		(findall(X, member([X,_],Call_layers), Clt),
		flatten(Clt, Clt1), remove_dups(Clt1, Call_layers_tops),
		findall(X, member([_,X],Call_layers), Clb),
		flatten(Clb, Clb1), remove_dups(Clb1, Call_layers_bottoms))
	),
	union([Call_layers_tops, Call_layers_bottoms], Call_layers_ccs),
	(Spawn_layers == [unspecified] ->
		(Spawn_layers_tops = [], Spawn_layers_bottoms = []);
		(findall(X, member([X,_],Spawn_layers), Slt),
		flatten(Slt, Slt1), remove_dups(Slt1, Spawn_layers_tops),
		findall(X, member([_,X],Spawn_layers), Slb),
		flatten(Slb, Slb1), remove_dups(Slb1, Spawn_layers_bottoms))
	),
	union([Spawn_layers_tops, Spawn_layers_bottoms], Spawn_layers_ccs),
	(Dataconnector_layers == [unspecified] ->
		(Dataconnector_layers_tops = [], Dataconnector_layers_bottoms = []);
		(findall(X, member([X,_],Dataconnector_layers), Dlt),
		flatten(Dlt, Dlt1), remove_dups(Dlt1, Dataconnector_layers_tops),
		findall(X, member([_,X],Dataconnector_layers), Dlb),
		flatten(Dlb, Dlb1), remove_dups(Dlb1, Dataconnector_layers_bottoms))
	),
	union([Dataconnector_layers_tops, Dataconnector_layers_bottoms], Dataconnector_layers_ccs),
	(subset(Call_layers_ccs, AllCCs) ->
		true;
		zz('Call layers refer to component outside of system', Name,
		(Call_layers_ccs, AllCCs))
	),
	(subset(Spawn_layers_ccs, AllCCs) ->
		true;
		zz('Spawn layers refer to component outside of system', Name,
		(Spawn_layers_ccs, AllCCs))
	),
	(subset(Dataconnector_layers_ccs, AllCCs) ->
		true;
		zz('Dataconnector layers refer to component outside of system', Name,
		(Dataconnector_layers_ccs, AllCCs))
	),
	((Calls == [unspecified] ; Call_layers == [unspecified]) ->
		true;
		(findall([X,Y],(member(C,Calls),getvalue(controlconnector,_,C,c1,[X]),
		X \== unspecified,getvalue(controlconnector,_,C,c2,[Y]),Y \== unspecified),CallC12s),
		(layered(CallC12s, Call_layers) ->
			true;
			zz('System call breaks layering constraint', Name, (Calls,Call_layers))
		))
	),
	((Spawns == [unspecified] ; Spawn_layers == [unspecified]) ->
		true;
		(findall([X,Y],(member(C,Spawns),getvalue(controlconnector,_,C,c1,[X]),
		X \== unspecified,getvalue(controlconnector,_,C,c2,[Y]),Y \== unspecified),SpawnC12s),
		(layered(SpawnC12s, Spawn_layers) ->
			true;
			zz('System spawn breaks layering constraint', Name, (Spawns,Spawn_layers))
		))
	),
	((Dataconnectors == [unspecified] ; Dataconnector_layers == [unspecified]) ->
		true;
		(findall([X,Y],(member(C,Dataconnectors),getvalue(dataconnector,_,C,c1,[X]),
		X \== unspecified,getvalue(dataconnector,_,C,c2,[Y]),Y \== unspecified),DataconnectorC12s),
		(layered(DataconnectorC12s, Dataconnector_layers) ->
			true;
			zz('System dataconnector breaks layering constraint', 
			Name, (Dataconnectors,Dataconnector_layers))
		))
	),
	(Calls == [unspecified] ->
		true;
		((Initialcontrolcomponents == [unspecified] ->
			getmaxthreads([], Triggers, Spawns, Calls, Threads);
			getmaxthreads(Initialcontrolcomponents, Triggers, Spawns, Calls, Threads)
		),
		(mapvartovar(member, AllCCs, Threads) ->
			true;
			zz('System component not initial, called, spawned, or triggered in threads',
			Name, (AllCCs, Threads))
		),
		findall([C1,C2,O1,O2],((member(C,Calls);member(C,Spawns)),
		getvalue(controlconnector,_,C,c1,[C1]),C1 \== unspecified,
		getvalue(controlconnector,_,C,c2,[C2]),C2 \== unspecified,
		getvalue(controlcomponent,_,C1,method,C1t),C1t == [true],
		getvalue(controlcomponent,_,C2,method,C2t),C2t == [true],
		getvalue(controlconnector,_,C,o1,[O1]),O1 \== unspecified,
		getvalue(controlconnector,_,C,o2,[O2]),O2 \== unspecified),TTCallList),
		(validmmcontrolconnectors(TTCallList,Globalobjects,AllCCs,Threads) ->
			true;
			zz('Invalid method-method control connector', Name,
			(TTCallList,Globalobjects,AllCCs,Threads))
		),
		findall([C1,C2,O2],((member(C,Calls);member(C,Spawns)),
		getvalue(controlconnector,_,C,c1,[C1]),C1 \== unspecified,
		getvalue(controlconnector,_,C,c2,[C2]),C2 \== unspecified,
		getvalue(controlcomponent,_,C1,method,C1t),C1t == [false],
		getvalue(controlcomponent,_,C2,method,C2t),C2t == [true],
		getvalue(controlconnector,_,C,o2,[O2]),O2 \== unspecified),FTCallList),
		(validcmcontrolconnectors(FTCallList,Globalobjects) ->
			true;
			zz('Invalid nonmethod-method controlconnector', Name,
			(FTCallList,Globalobjects))
		))
	),
	(Triggers == [unspecified] ->
		true;
		(findall(X, (member(X,Triggers),getvalue(trigger,_,X,ccobj,[S]),S==[ctype]), CTriggers),
		findall(X, (member(X,Triggers),getvalue(trigger,_,X,ccobj,[S]),S==[otype]), OTriggers),
		fetch(trigger, _, CTriggers, controlcomponent, CTriggersCCs),
		fetch(trigger, _, OTriggers, object, OTriggersOs),
		fetch(trigger, _, Triggers, outcalls, Outcalls),
		fetch(trigger, _, Triggers, outspawns, Outspawns),
		(subset(CTriggersCCs, AllCCs) ->
			true;
			zz('Trigger controlcomponent not found in system', Name,
			(CTriggersCCs, AllCCs))
		),
		(subset(Outcalls, AllCCs) ->
			true;
			zz('Trigger outcall not found in system', Name,
			(Outcalls, AllCCs))
		),
		(subset(Outspawns, AllCCs) ->
			true;
			zz('Trigger outspawn not found in system', Name,
			(Outspawns, AllCCs))
		),
		(subset(OTriggersOs, Allobjects) ->
			true;
			zz('Trigger object not found in system', Name,
			(OTriggersOs, Allobjects))
		),
		(RecognizedMessages == [unspecified] ->
			true;
			(fetch(trigger, _, Triggers, inmessage, TriggerInmessages),
			fetch(trigger, _, Triggers, outmessageset, TriggerOutmessages),
			(subset(TriggerInmessages, RecognizedMessages) ->
				true;
				zz('Trigger inmessage not recognized in system', Name,
				(TriggerInmessages, RecognizedMessages))
			),
			(subset(TriggerOutmessages, RecognizedMessages) ->
				true;
				zz('Trigger outmessage not recognized in system', Name,
				(TriggerOutmessages, RecognizedMessages))
			))
		),
		(Call_layers == [unspecified] ->
			true;
			(findall([X,Y],(member(T,CTriggers),getvalue(trigger,_,T,controlcomponent,[X]),
			X \== unspecified,getvalue(trigger,_,T,outcalls,OC),OC \== unspecified,
			member(Y,OC)),CTriggerCallC12s),
			(layered(CTriggerCallC12s, Call_layers) ->
				true;
				zz('System trigger call breaks layering constraint', Name,
				(CTriggerCallC12s, Call_layers))
			))
		),
		(Spawn_layers == [unspecified] ->
			true;
			(findall([X,Y],(member(T,CTriggers),getvalue(trigger,_,T,controlcomponent,[X]),
			X \== unspecified,getvalue(trigger,_,T,outspawns,OS),OS \== unspecified,
			member(Y,OS)),CTriggerSpawnC12s),
			(layered(CTriggerSpawnC12s, Spawn_layers) ->
				true;
				zz('System trigger spawn breaks layering constraint', Name,
				(CTriggerSpawnC12s, Spawn_layers))
			))
		))
	),
	(Nodes == [unspecified] ->
		true;
		(fetch(controlcomponent, _, Controlcomponents, nodes, CCnodes),
		fetch(controlconnector, _, Calls, nodes, Cnodes),
		fetch(controlconnector, _, Spawns, nodes, Snodes),
		fetch(dataconnector, _, Dataconnectors, nodes, DCnodes),
		union([CCnodes,Cnodes,Snodes,DCnodes], Allnodes),
		(subset(Allnodes, Nodes) ->
			true;
			zz('Subnode not found in system nodes', Name, (Allnodes,Nodes))
		))
	),
	(Shareddata == [unspecified] ->
		true;
		(findall(X, member([X,_,_],Shareddata), ShareddataDS),
		getsize(ShareddataDS, [], Shareddatasize),
		write('System Shared Data Size: '), write(Shareddatasize), write('K.'), nl)
	),
	(Controlcomponents == [unspecified] ->
		true;
		(getsize([], Controlcomponents, Controlcomponentssize),
		write('System Code Size: '), write(Controlcomponentssize), write('K.'), nl)
	).


/* Checks if a list of method-method control connectors are valid */
validmmcontrolconnectors([], _, _, _) :- !.
validmmcontrolconnectors([[_C1,_C2,_O1,O2] | OtherCalls], Globalobjects, AllCCs, Threads) :-
	member(O2, Globalobjects),
	!, validmmcontrolconnectors(OtherCalls, Globalobjects, AllCCs, Threads).
validmmcontrolconnectors([[_C1,_C2,O1,O2] | OtherCalls], Globalobjects, AllCCs, Threads) :-
	intraccmmcontrolconnnector(O1, O2, AllCCs),
	!, validmmcontrolconnectors(OtherCalls, Globalobjects, AllCCs, Threads).
validmmcontrolconnectors([[_C1,_C2,O1,O2] | OtherCalls], Globalobjects, AllCCs, Threads) :-
	interthreadmmcontrolconnector(O1, O2, AllCCs, Threads),
	!, validmmcontrolconnectors(OtherCalls, Globalobjects, AllCCs, Threads).

/* Checks if a list of nonmethod-method control connectors are valid */
validcmcontrolconnectors([], _) :- !.
validcmcontrolconnectors([[_C1,_C2,O2] | OtherCalls], Globalobjects) :-
	member(O2, Globalobjects),
	!, validcmcontrolconnectors(OtherCalls, Globalobjects).
validcmcontrolconnectors([[C1,_C2,O2] | OtherCalls], Globalobjects) :-
	getvalue(controlcomponent,_,C1,localobjects,C1Localobjects),
	member(O2, C1Localobjects),
	!, validcmcontrolconnectors(OtherCalls, Globalobjects).

/* Determines if a method-method control connector is within a control component */
intraccmmcontrolconnnector(O1, O2, AllCCs) :-
	member(CC, AllCCs), getvalue(controlcomponent,_,CC,localobjects,CCLocalObjects),
	subset([O1,O2], CCLocalObjects), !.

/* Determine if a method-method control connector is across threads */
interthreadmmcontrolconnector(O1, O2, AllCCs, Threads) :-
	member(CC1, AllCCs), member(CC2, AllCCs), CC1 \== CC2,
	member(T1, Threads), member(T2, Threads), T1 \== T2,
	member(CC1, T1), member(CC2, T2),
	getvalue(controlcomponent, _, CC1, localobjects, LO1),
	getvalue(controlcomponent, _, CC1, localobjects, LO2),
	member(O1, LO1), member(O2, LO2), !.

getmaxthreads(Initialcontrolcomponents, Triggers, Spawns, Calls, Threads) :-
	fetch(trigger, _, Triggers, outspawns, TOS),
	fetch(trigger, _, Triggers, outcalls, TOC),
	union([TOS,TOC], TriggerCCs),
	fetch(controlconnector, _, Spawns, c2, SpawnC2s),
	union([Initialcontrolcomponents, TriggerCCs, SpawnC2s], TopCCs),
	fetch(controlconnector, _, Calls, c1, CallC1s),
	fetch(controlconnector, _, Calls, c2, CallC2s),
	union([CallC1s,CallC2s], CallCCs),
	findall(X, (member(TopCC,TopCCs),getthread(TopCC,CallCCs,Calls,X)), Threads), !.

getthread(TopCC, CallCCs, Calls, Thread) :-
	findall([X,Y],(member(C,Calls),getvalue(controlconnector,_,C,c1,[X]),X \== unspecified,
	getvalue(controlconnector,_,C,c2,[Y]),Y \== unspecified),CallC12s),
	findall(X, (member(X,CallCCs),dconnected(TopCC,X,CallC12s)), T1),
	union([[TopCC],T1], Thread), !.

/* Horribly inefficient */
r_getallobjects([], []) :- !.
r_getallobjects(CurrentObjects, AllObjects) :-
	fetch(object, _, CurrentObjects, methods, CurrentMethods),
	fetch(controlcomponent, _, CurrentMethods, localobjects, NextObjects),
	r_getallobjects(NextObjects, NextNextObjects),
	append(CurrentObjects, NextObjects, A),
	append(A, NextNextObjects, All),
	remove_dups(All, AllObjects).

r_getallmethods([], []) :- !.
r_getallmethods(CurrentObjects, AllMethods) :-
	fetch(object, _, CurrentObjects, methods, CurrentMethods),
	fetch(controlcomponent, _, CurrentMethods, localobjects, NextObjects),
	r_getallmethods(NextObjects, NextMethods),
	append(CurrentMethods, NextMethods, Methods1),
	remove_dups(Methods1, AllMethods).

getallcontrolcomponents(SystemName, OutputCCs) :-
	getvalues(system, _, SystemName,
	[controlcomponents, globalobjects],
	[Controlcomponents, Globalobjects]),
	getallcontrolcomponents(Controlcomponents, Globalobjects, OutputCCs).

getallcontrolcomponents(InputCCs, InputOBs, OutputCCs) :-
	fetch(controlcomponent, _, InputCCs, localobjects, LocalOBs),
	r_getallobjects(LocalOBs, RLocalOBs),
	r_getallmethods(RLocalOBs, LocalOBmethods),
	(InputOBs == [unspecified] ->
		InputOBmethods = [];
		r_getallmethods(InputOBs, InputOBmethods)
	),
	(InputCCs == [unspecified] ->
		union([InputOBmethods, LocalOBmethods], OutputCCs);
		union([InputCCs, InputOBmethods, LocalOBmethods], OutputCCs)
	).

getallobjects(SystemName, OutputOBs) :-
	getvalues(system, _, SystemName,
	[controlcomponents, globalobjects],
	[Controlcomponents, Globalobjects]),
	getallobjects(Controlcomponents, Globalobjects, OutputOBs).

getallobjects(InputCCs, InputOBs, OutputOBs) :-
	fetch(controlcomponent, _, InputCCs, localobjects, LocalOBs),
	r_getallobjects(LocalOBs, RLocalOBs),
	union([InputOBs, RLocalOBs], OutputOBs).

getsize([unspecified], [unspecified], 0) :- !.
getsize(Datacomponents, [unspecified], Size) :-
	!, getdatasizes(Datacomponents, 0, Size).
getsize([unspecified], Controlcomponents, Size) :-
	!, getccsizes(Controlcomponents, 0, Size).
getsize(Datacomponents, Controlcomponents, Size) :-
	!, getdatasizes(Datacomponents, 0, SD),
	getccsizes(Controlcomponents, 0, SC),
	Size is SD + SC.

getdatasizes([], Total, Total).
getdatasizes([HD | TD], Accum, Total) :-
	(indatabase([HD]) ->
		(alltypes(AllTypes),
		getactualtype(HD, AllTypes, ActualType),
		ActualType =.. [ActualEntity, _],
		getvalue(ActualEntity, _, HD, datasize, [S]),
		(S == unspecified ->
			getdatasizes(TD, Accum, Total);
			(NewAccum is Accum + S,
			getdatasizes(TD, NewAccum, Total))
		));
		getdatasizes(TD, Accum, Total)
	).

getccsizes([], Total, Total).
getccsizes([HD | TD], Accum, Total) :-
	(indatabase([HD]) ->
		(alltypes(AllTypes),
		getactualtype(HD, AllTypes, ActualType),
		ActualType =.. [ActualEntity, _],
		getvalue(ActualEntity, _, HD, codesize, [S]),
		(S == unspecified ->
			getccsizes(TD, Accum, Total);
			(NewAccum is Accum + S,
			getccsizes(TD, NewAccum, Total))
		));
		getccsizes(TD, Accum, Total)
	).

/*************************************************************************/
/*typeschema(entity(style), variable attributes, types of former,
	fixed attributes, fixed attributes values, parent entity(style))*/

typeschema(port(base),
	[iotype, dataformat, rate, buffersize, streaming, blocking],
	[enum(iotype), atomlist, positive, nonnegative, boolean, boolean],
	[],
	[],
	[]).
typeschema(datacomponent(base),
	[type, datasize, nodes],
	[enum(datatype), positive, atomset],
	[],
	[],
	[]).
typeschema(controlcomponent(base),
	[ports, arguments, returnvalues,
	locals,
	localobjects,
	method, nodes, codesize, platform],
	[atomset(substructure([port(base)])), atomlist, atomlist,
	atomset(substructure([datacomponent(base)])),
	atomset(substructure([object(base)])),
	boolean, atomset, positive, atom],
	[],
	[],
	[]).
typeschema(object(base),
	[class, ports,
	data,
	methods, nodes, resources, platform],
	[atom, atomset(substructure([port(base)])),
	atomset(substructure([datacomponent(base)])),
	atomset(substructure([controlcomponent(base)])), atomset, atom, atom],
	[],
	[],
	[]).
typeschema(dataconnector(base),
	[ccobj, p1, p2, c1, c2, o1, o2, dataformat,
	rate, directionality, buffersize, reliable, nodes, resources, platform],
	[enum(cosubtype), atom, atom, atom, atom, atom, atom, atomlist,
	positive, directionality, nonnegative, boolean, atomset, atom, atom],
	[],
	[],
	[]).
typeschema(controlconnector(base),
	[c1, c2, o1, o2, arguments, returnvalues, rate, queuesize, blocking,
	reliable, nodes, resources, platform],
	[atom, atom, atom, atom, atomlist, atomlist, positive, nonnegative, boolean,
	boolean, atomset, atom, atom],
	[],
	[],
	[]).
typeschema(trigger(base),
	[ccobj, controlcomponent, object, inmessage, inports, outports,
	outmessageset, outcalls, outspawns],
	[enum(cosubtype), atom, atom, atom, atomset, atomset,
	atomset, atomset, atomset],
	[],
	[],
	[]).
typeschema(system(base),
	[initialcontrolcomponents, initialdataconnectors, globalobjects,
	controlcomponents, classes, shareddata,
	dataconnectors,
	calls,
	spawns,
	recognizedmessages, triggers, call_layers, 
	spawn_layers, dataconnector_layers,
	nodes, resources, platform],
	[atomset, atomset, atomset(substructure([object(base)])),
	atomset(substructure([controlcomponent(base)])), atomset, tupleset(3),
	atomset(substructure([dataconnector(base)])),
	atomset(substructure([controlconnector(base)])),
	atomset(substructure([controlconnector(base)])),
	atomset, atomset(substructure([trigger(base)])), tupleset(2),
	tupleset(2), tupleset(2),
	atomset, atom, atom],
	[],
	[],
	[]).

informalnames(port(base),
	['Port I/O type',
	'Data Format  ',
	'Rate         ',
	'Buffer size  ',
	'Streaming    ',
	'Blocking     ']).
informalnames(datacomponent(base),
	['Data Type',
	'Data Size',
	'Nodes    ']).
informalnames(controlcomponent(base),
	['Ports                ',
	'Arguments            ',
	'Return Values        ',
	'Local Data Components',
	'Local Objects        ',
	'Method               ',
	'Nodes                ',
	'Code Size            ',
	'Platform             ']).
informalnames(object(base),
	['Class         ',
	'Ports         ',
	'Object Data   ',
	'Object Methods',
	'Nodes         ',
	'Resources     ',
	'Platform      ']).
informalnames(dataconnector(base),
	['Control Component or Object?', 
	'Port 1                      ',
	'Port 2                      ', 
	'Control Component 1         ', 
	'Control Component 2         ',
	'Object 1                    ', 
	'Object 2                    ',
	'Data Format                 ', 
	'Rate                        ', 
	'Directionality              ', 
	'Buffer Size                 ', 
	'Reliable                    ',
	'Nodes                       ', 
	'Resources                   ', 
	'Platform                    ']).
informalnames(controlconnector(base),
	['Control Component 1',
	'Control Component 2',
	'Object 1           ', 
	'Object 2           ', 
	'Arguments          ', 
	'Return Values      ',
	'Rate               ', 
	'Queue Size         ', 
	'Blocking           ', 
	'Reliable           ', 
	'Nodes              ',
	'Resources          ', 
	'Platform           ']).
informalnames(trigger(base),
	['Control Component or Object?',
	'Control Component           ', 
	'Object                      ', 
	'Input Message               ', 
	'Input Ports                 ',
	'Output Ports                ', 
	'Output Messages             ', 
	'Output Calls                ', 
	'Output Spawns               ']).
informalnames(system(base),
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

