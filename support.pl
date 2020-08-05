/*
:- compile(library(basics)).
:- compile(library(sets)).
*/
/*******************************************************************/
/* Rules for reporting information */

/* Rule for writing a line to standard output with an arbitrary indent */
spit(N,A) :- tab(N), write(A), nl.

spit(A) :- spit(0, A).

/* Simple message with header */
msg(Header, Data) :- write(Header), write(': '), write(Data), nl.

/* Simple message with header, indent */
msg(N, Header, Data) :- tab(N), write(Header), write(': '), write(Data), nl.

zz(Msg, Name, Data) :-
	write('MISMATCH: '), write(Msg), nl,
	tab(10), write('Name: '), write(Name), nl,
	tab(10), write(Data), nl, nl.

xx(Msg, Name, Data) :-
	write('ERROR: '), write(Msg), nl,
	tab(10), write('Name: '), write(Name), nl,
	tab(10), write(Data), nl, nl,
	!, fail.

/*******************************************************************/
/* slist(structurename) */
slist(S, N) :-
        atom(S), integer(N), functor(Func, S, N),
        !, call(Func), write(Func), nl, fail.
slist(_, _) :- write('Error!'), nl.

/*******************************************************************/
/* flattens a list */
flatten(InList, OutList) :-
	flatten(InList, [], OutList).

flatten([], [], []) :- !.
flatten([], [Top | Stack], OutList) :-
	flatten(Top, Stack, OutList).
flatten([Head | Tail], Stack, [Head | Rest]) :-
	atomic(Head), Head \== [], flatten(Tail, Stack, Rest).
flatten([Head | Tail], Stack, [Head | Rest]) :-
	compound(Head), not_is_list(Head), Head \== [], flatten(Tail, Stack, Rest).
flatten([Head | Tail], Stack, Out) :-
	is_list(Head), flatten(Head, [Tail | Stack], Out).

/*******************************************************************/
/* converts a list to an atom */

listtoatom([], '[]') :- !.
listtoatom(List, Atom) :-
	is_list(List),
	convertlisttochars(List, Chars),
	atom_chars(Atom, Chars).

convertlisttochars([], "[]") :- !.
convertlisttochars(List, Chars) :-
	q(List, [], InnerChars),
	append(["[", InnerChars, "]"], Chars).

q([], Chars, Chars) :- !.
q([H | T], Accum, Total) :-
	H \== [], atom(H), atom_chars(H, HChars),
	(Accum == [] ->
		NewAccum = HChars;
		append([Accum, ", ", HChars], NewAccum)
	),
	q(T, NewAccum, Total).
q([H | T], Accum, Total) :-
	number(H), number_chars(H, HChars),
	(Accum == [] ->
		NewAccum = HChars;
		append([Accum, ", ", HChars], NewAccum)
	),
	q(T, NewAccum, Total).
q([H | T], Accum, Total) :-
	is_list(H),
	convertlisttochars(H, HChars),
	(Accum == [] ->
		NewAccum = HChars;
		append([Accum, ", ", HChars], NewAccum)
	),
	q(T, NewAccum, Total).
q([H | T], Accum, Total) :-
	compound(H), not_is_list(H),
	H =.. [Name | Args],
	atom_chars(Name, NameChars),
	(Accum == [] ->
		append([Accum, NameChars, "("], NewAccum1);
		append([Accum, ", ", NameChars, "("], NewAccum1)
	),
	q(Args, [], ArgsChars),
	append([NewAccum1, ArgsChars, ")"], NewAccum),
	q(T, NewAccum, Total).

/*******************************************************************/
/* determines if argument is not a list */
not_is_list(X) :- is_list(X), !, fail.
not_is_list(_).

/*******************************************************************/
/* checks if a list has only a single unique element in it,
or if it is empty */
identicalvalues([]) :- !.
identicalvalues([Head|Tail]) :-
	remove_dups([Head|Tail], NoDups), !, length(NoDups, 1).

/* Assumes the input is a list of sets */
identicalsets([]) :- !.
identicalsets([_]) :- !.
identicalsets([Set1 | Sets]) :-
	length([Set1 | Sets], L), L >= 2,
	identicalsets(Set1, Sets), !.

identicalsets(_, []) :- !.
identicalsets(Master, [H | T]) :-
	seteq(Master, H), identicalsets(Master, T), !.

/*******************************************************************/
/* Applies a binary predicate over a list with respect to a fixed
	term.  This is a refinement of maplist.
	Example: mapvartofix(subset, [[1], [2], [3]], [1, 2, 3]).
	Example: mapfixtovar(subset, [1, 2, 3], [[1], [2], [3]]).
*/

mapvartofix(_, [], _) :- !.
mapvartofix(Pred, [Variable | OtherVariables], Fixed) :-
	Fact =.. [Pred, Variable, Fixed],
	call(Fact), !,
	mapvartofix(Pred, OtherVariables, Fixed).

mapfixtovar(_, _, []) :- !.
mapfixtovar(Pred, Fixed, [Variable | OtherVariables]) :-
	Fact =.. [Pred, Fixed, Variable],
	call(Fact), !,
	mapfixtovar(Pred, Fixed, OtherVariables).

mapvartovar(Pred, V1s, V2s) :-
	mapvartovar(Pred, V1s, V2s, V2s).

mapvartovar(_, [], _, _) :- !.
mapvartovar(_, V1s, [], _) :- V1s \== [], !, fail.
mapvartovar(Pred, [V1 | V1Remaining], [V2 | _], OriginalV2) :-
	Fact =.. [Pred, V1, V2],
	call(Fact), !,
	mapvartovar(Pred, V1Remaining, OriginalV2, OriginalV2).
mapvartovar(Pred, [V1 | V1Remaining], [_ | V2Remaining], OriginalV2) :-
	mapvartovar(Pred, [V1 | V1Remaining], V2Remaining, OriginalV2), !.

/*******************************************************************/
/* temporary section */
platformcompatible(X,X).

/*******************************************************************/
/* show section */

show(styles) :-
	nl, write('Supported styles:'), nl,
	style(S,_), spit(8, S), fail.
show(_).
