/* :- compile(library(ctypes)). */

d :- purge, parse_file(datafile).
readdata :- purge, parse_file(datafile).

/*******************************************************************/
/* Main rule for reading in and processing the input file */
parse_file(X) :-
	see(X), read_file(_, [], Tokens), !, seen,
	!,
/*	write('Tokens: '), write(Tokens), nl, nl,*/
	process_tokens(Tokens).
/*	write('Successfully Parsed.'), nl.*/


/*******************************************************************/
/* Process what has been read rule */
process_tokens([]) :- write('No input!!!'), nl, !, fail.
process_tokens(Tokens) :- 
	styleunits(Tokens, []), 
	nl, spit(0, '---> Specification succesfully parsed.').
process_tokens(_) :-
	nl, spit(0, '---> Unable to parse input!'), nl.

/*******************************************************************/
read_file(End, Total, Total) :-
	nonvar(End), /*write('Endoffile test with: '), write(End), nl,*/
	is_endfile(End).
read_file(_, OldWords, Total) :-
	get_word(NewW, NextC), /*write('Found word: '), write(NewW), nl,*/
	(NewW \== '' -> append(OldWords, [NewW], NewWords) ; NewWords=OldWords),
	read_file(NextC, NewWords, Total).

/*******************************************************************/
get_word(Word, NextC) :-
	get_string(_, [], String, NextC), !,
	stringtoword(String, Word).

/*******************************************************************/
stringtoword([], '').
stringtoword(String, Word) :- name(Word, String).

/*******************************************************************/
/* Rules to parse for words, atomic symbols in input file */
/* get_string(current char, chars so far, final word, next char) */
/*******************************************************************/
/* Haven't instantiated first character to read yet */
get_string(VarC, AccumList, WordAsList, NextC) :-
	var(VarC), peek_char(VarC), get_string(VarC, AccumList, WordAsList, NextC).
/* Looking at end of file, return with what we have */
get_string(CurrentC, WordAsList, WordAsList, CurrentC) :-
	is_endfile(CurrentC), get0(CurrentC).
/* Ignore beginning whitespace */
get_string(CurrentC, [], WordAsList, NextC) :-
	is_space(CurrentC), get0(CurrentC), peek_char(NewC),
	get_string(NewC, [], WordAsList, NextC).
/* Ending whitespace ends search for word */
get_string(CurrentC, WordAsList, WordAsList, NextC) :-
	is_space(CurrentC), get0(CurrentC), peek_char(NextC).
/* Punctuation by itself is consumed immediately */
get_string(CurrentC, [], [CurrentC], NextC) :-
	is_punct(CurrentC), get0(CurrentC), peek_char(NextC).
/* Ending punctuation is left alone for next gets, except underscore */
get_string(CurrentC, WordAsList, WordAsList, CurrentC) :-
	is_punct(CurrentC), CurrentC \== 95.
/* Append current character to accumulated string */
get_string(CurrentC, AccumList, WordAsList, NextC) :-
	get0(CurrentC), peek_char(NewC),
	append(AccumList, [CurrentC], NewAccumList), 
	get_string(NewC, NewAccumList, WordAsList, NextC).

/*******************************************************************/
/* Definite Clause Grammar (dcg).                                  */
/*******************************************************************/

styleunits --> styleunit, styleunits, !.
styleunits --> !, [].

styleunit --> comment, [style], comment, dcgstyle(Style),
	{msg('Working Style', Style)}, 
	comment, dcgentitys(Style), comment,
	[end], comment, [;], comment, !,
	{msg('End Working Style', Style), nl}.

dcgstyle(Style) --> [Style], {style(Style,_)}.

dcgentitys(Style) --> dcgentity(Style), dcgentitys(Style), !.
dcgentitys(_) --> !, [].

dcgentity(Style) -->
	[Entity], identifier(Name),
	{Type =.. [Entity, Style], recognizedtype(Type, RealType),
	msg(4, RealType, Name)},
	multipledeclarations(Attributes, ValueLists), !,
	{processentity(Name, RealType, Attributes, ValueLists)},
	[end, ;].

multipledeclarations([Attribute|Attributes], [ValueList|ValueLists]) -->
	multipledeclaration(Attribute, ValueList),
	/*{write(Attribute), write(' '), write(ValueList), nl},*/
	multipledeclarations(Attributes, ValueLists), !.
multipledeclarations([], []) --> !.

multipledeclaration(Attribute, []) -->
	[Attribute, ':', '[', ']', ';'],
	{msg(8, Attribute, '[]')}.
multipledeclaration(Attribute, List) -->
	[Attribute, ':'], list_of_tuples(List), [';'],
	{msg(8, Attribute, List)}.
multipledeclaration(Attribute, List) -->
	[Attribute, ':'], list_of_identifiers(List), [';'],
	{msg(8, Attribute, List)}.

/* List of Identifiers */
list_of_identifiers( [ Identifier | Identifiers ] ) --> 
	identifier( Identifier ),  [','], list_of_identifiers( Identifiers ).
list_of_identifiers( [ Identifier ] ) --> 
	identifier( Identifier ).
list_of_identifiers( [] ) --> !.

list_of_tuples( [ Tuple | Tuples ] ) -->
	tuple( Tuple ), [','], list_of_tuples( Tuples ).
list_of_tuples( [ Tuple ] ) -->
	tuple( Tuple ).
list_of_tuples( [] ) --> !.

tuple( Identifiers ) -->
	[ '[' ], list_of_identifiers( Identifiers ), [ ']' ].

identifier( T ) --> tuple( T ). /* need to change the name of this rule */
identifier( I ) --> [I], {notkeyword(I)}.

numeral( N ) --> [N], { number(N) }.

nonkeyword( N ) --> [N], { notkeyword(N) }.

/* Used above to ignore what can't be understood */
many_words --> single_word, many_words.
many_words --> single_word.

single_word --> [_].

/*******************************************************************/

comment --> leftbrace, nonrightbraces, rightbrace, comment, !.
comment --> !.

leftbrace --> ['{'].
rightbrace --> ['}'].

nonrightbraces --> nonrightbrace, nonrightbraces.
nonrightbraces --> nonrightbrace.
nonrightbraces --> !.

nonrightbrace --> [NonRightBrace], {NonRightBrace \== '}'}.

/****************************/
/* Support routines for DCG */

notkeyword(K) :- keyword(K), !, fail.
notkeyword(_).

/* Listing of the keywords */

keyword(base).
keyword(buffersize).
keyword(callback).
keyword(circuit).
keyword(controlcomponent).
keyword(controlconnector).
keyword(datacomponent).
keyword(dataconnector).
keyword(datastructure).
keyword(distributed).
keyword(distributedprocess).
keyword(end).
keyword(event).
keyword(eventbased).
keyword(filter).
keyword(filters).
keyword(group).
keyword(inputsockets).
keyword(interviews).
keyword(logicprogram).
keyword(mach).
keyword(mainsubroutine).
keyword(message).
keyword(multithreaded).
keyword(object).
keyword(objectoriented).
keyword(obst).
keyword(outputsockets).
keyword(pipefilter).
keyword(pipes).
keyword(port).
keyword(procedure).
keyword(procedurecall).
keyword(process).
keyword(processcall).
keyword(processspawn).
keyword(receiver).
keyword(rulebased).
keyword(sender).
keyword(socket).
keyword(sockets).
keyword(softbench).
keyword(softwarebus).
keyword(style).
keyword(system).
keyword(task).
keyword(trigger).
keyword(unas).
keyword(unspecified).

/*******************************************************************/
