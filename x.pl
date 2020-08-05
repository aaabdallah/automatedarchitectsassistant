:- ensure_loaded(library(proxt)).
:- ensure_loaded(library(proxl)).
:- ensure_loaded(library(xif)).

:- dynamic aaax/2.

/*****************************************************************************/
/* Callbacks */

test_callback(_Widget, _ClientData, _CallData) :-
	display_browsershell.

user_quit(_Widget, _ClientData, _CallData) :-
	assert(aaax(quitprogram, true)), !.

/***********************/
/* Specification Shell */
open_specshell(_Widget, ClientData, _CallData) :-
	validtype(ClientData) ->
		display_specshell(ClientData, unspecified);
		display_errordialog('AAA Error', 'AAA is unable to recognize this architectural type.').

exit_from_specshell(_Widget, _ClientData, _CallData) :-
	hide_specshell.

analyze_from_specshell(_Widget, _ClientData, _CallData) :-
	aaax(currententityname, CurrentEntityName), !,
	aaax(currententity, CurrentEntity),
	aaax(currentstyle, CurrentStyle),
	CurrentType =.. [CurrentEntity, CurrentStyle],
	tell(tempo), validentity(CurrentType, CurrentEntityName), told,
	readinresults(tempo), !,
	atom_chars('Results of analyzing: ', R),
	atom_chars(CurrentEntityName, P),
	append(R, P, TitleChars),
	atom_chars(Title, TitleChars),
	display_resultsdialog(Title).
analyze_from_specshell(_Widget, _ClientData, _CallData) :-
	display_errordialog('Analyze Entity Error', 'Unable to perform analysis!').

/****************/
/* Error Dialog */
ok_from_errordialog(_Widget, _ClientData, _CallData) :-
	hide_errordialog.

not_ready_yet(_Widget, _ClientData, _CallData) :-
	display_errordialog('AAA Error', 'Vaporware.').

/*****************/
/* Prompt Dialog */
ok_from_promptdialog(_Widget, _ClientData, _CallData) :-
	aaax(promptdialogselectionbox, PromptDialogSelectionBox),
	xmSelectionBoxGetChild(PromptDialogSelectionBox, xmDIALOG_TEXT, TextField),
	xmTextFieldGetString(TextField, ProjectFile),
	tell(tempo), purge, parse_file(ProjectFile), told,
	readinresults(tempo), !,
	hide_promptdialog,
	atom_chars('Results of parsing project file: ', R),
	atom_chars(ProjectFile, P),
	append(R, P, TitleChars),
	atom_chars(Title, TitleChars),
	display_resultsdialog(Title),
	update_current_project_label(ProjectFile).

cancel_from_promptdialog(_Widget, _ClientData, _CallData) :-
	hide_promptdialog.

prompt_for_projectfile(_Widget, _ClientData, _CallData) :-
	display_promptdialog('Load Project', 'Please Enter Project Name:').

/*****************/
/* Browser Shell */
open_browsershell(_Widget, _ClientData, _CallData) :-
	display_browsershell.

cancel_from_browser(_Widget, _ClientData, _CallData) :-
	hide_browsershell.

ok_from_browser(_Widget, _ClientData, CallData) :-
	proxtGetCallbackFields(CallData, Fields),
	member(value(EntryString), Fields),
	xmStringInitContext(Context, EntryString),
	xmStringGetNextSegment(Context, _Text, Tag, _Direction, _Separator),
	xmStringFreeContext(Context),
	xmStringGetLtoR(EntryString, Tag, Entry),
	atom_chars(Entry, EntryChars),
	append([InformalStyleNameChars, " : ", EntityChars, " : ", NameChars],
	EntryChars),
	atom_chars(InformalStyleName, InformalStyleNameChars),
	atom_chars(Entity, EntityChars),
	atom_chars(Name, NameChars),
	style(Style, InformalStyleName),
	Type =.. [Entity, Style],
	display_specshell(Type, Name).

change_browser_option(_Widget, ClientData, _CallData) :-
	aaax(browsercurrentoption, OldBrowserCurrentOption),
	(OldBrowserCurrentOption == ClientData ->
		true;
		(retractall(aaax(browsercurrentoption, _)),
		assert(aaax(browsercurrentoption, ClientData)),
		aaax(browserselectionbox, BrowserSelectionBox),
		collectentities(ClientData, StyleEntityNameList),
		convertbrowserentries(StyleEntityNameList, BrowserEntries),
		xtUnmanageChild(BrowserSelectionBox),
		xtSetValues(BrowserSelectionBox, [xmNlistItems(BrowserEntries)]),
		free_strings(BrowserEntries),
		xtManageChild(BrowserSelectionBox))
	).

/******************/
/* Results Dialog */
ok_from_resultsdialog(_Widget, _ClientData, _CallData) :-
	hide_resultsdialog.

/*****************************************************************************/
/*****************************************************************************/
/* Main routine */

runtime_entry(start) :-
	do.

do :-
/*	xtAppInitialize(App, 'AAA', [], Shell),*/
	xif_initialize('AAA', 'AAA', Shell),
	xtCreateApplicationContext(App),

	initialize_aaa,

	assert(aaax(app,App)),
	assert(aaax(shell,Shell)),
	xtDisplay(Shell, Display),
	assert(aaax(display, Display)),
	get_screen_attributes([height(ScreenHeight), width(ScreenWidth)]),
	assert(aaax(screenheight, ScreenHeight)),
	assert(aaax(screenwidth, ScreenWidth)),

/* Create AAA's fonts */
	xLoadQueryFont(Display, '9x15bold', MainAAAFont),
	xmFontListCreate(MainAAAFont, xmFONTLIST_DEFAULT_TAG, MainAAAFontList),
	assert(aaax(mainaaafontlist, MainAAAFontList)),
	xLoadQueryFont(Display, '-*-times-medium-i-*-*-20-*-*-*-*-*-*-*', Font1),
	xmFontListCreate(Font1, xmFONTLIST_DEFAULT_TAG, Font1List),
	assert(aaax(font1list, Font1List)),
	xLoadQueryFont(Display, '10x20', MenuFont),
	xmFontListCreate(MenuFont, xmFONTLIST_DEFAULT_TAG, MenuFontList),
	assert(aaax(menufontlist, MenuFontList)),

	xtSetValues(Shell, [xmNtitle(' Architects Automated Assistant (0.5) ')]),

/* Create main widgets */
	xtCreateManagedWidget(master, xmFormWidgetClass, Shell, [], Master),
	xmStringCreateLtoR('Current Project: [Unspecified]', xmFONTLIST_DEFAULT_TAG,
	CurrentProjectString),
	xtCreateManagedWidget(currentproject, xmLabelWidgetClass, Master,
	[xmNfontList(Font1List),xmNlabelString(CurrentProjectString),
	xmNbottomAttachment(xmATTACH_FORM),xmNleftAttachment(xmATTACH_FORM),
	xmNrightAttachment(xmATTACH_FORM),
	xmNalignment(xmALIGNMENT_CENTER)], CurrentProjectLabel),
	xmStringFree(CurrentProjectString),
	assert(aaax(currentprojectlabel, CurrentProjectLabel)),
	xtCreateManagedWidget(row1, xmRowColumnWidgetClass, Master, 
	[xmNmarginHeight(0),xmNmarginWidth(0),
	xmNbottomAttachment(xmATTACH_WIDGET), xmNbottomWidget(CurrentProjectLabel),
	xmNtopAttachment(xmATTACH_FORM),xmNleftAttachment(xmATTACH_FORM),
	xmNrightAttachment(xmATTACH_FORM)], Row1),

/* Create main menu bar */
	xtCreateManagedWidget(mainmenubar, xmRowColumnWidgetClass, Row1, [xmNrowColumnType(xmMENU_BAR),
	xmNentryClass(xmCascadeButtonWidgetClass),xmNisHomogeneous(true),
	xmNmenuAccelerator('KMenuBar'),xmNorientation(xmHORIZONTAL),
	xmNmenuPost('<Btn1Down>')], MainMenuBar),
/**/

	xtCreateManagedWidget(mainseparator, xmSeparatorWidgetClass, Master, [], _MS),

/* Create project menu */
	make_strings([['Load',PS1], ['Save',PS2], ['Quit AAA',PS3]]),
	xmCreatePulldownMenu(MainMenuBar, projectmenu, [], ProjectMenu),
	xtCreateManagedWidget(projectloadbutton, xmPushButtonWidgetClass, ProjectMenu, 
	[xmNlabelString(PS1),xmNfontList(MenuFontList)], ProjectLoadButton),
	xtCreateManagedWidget(projectsavebutton, xmPushButtonWidgetClass, ProjectMenu, 
	[xmNlabelString(PS2),xmNfontList(MenuFontList)], ProjectSaveButton),
	xtCreateManagedWidget(projectquitbutton, xmPushButtonWidgetClass, ProjectMenu, 
	[xmNlabelString(PS3),xmNfontList(MenuFontList)], ProjectQuitButton),
	free_strings([PS1, PS2, PS3]),
	xtAddCallback(ProjectLoadButton, xmNactivateCallback, prompt_for_projectfile, _),
	xtAddCallback(ProjectSaveButton, xmNactivateCallback, not_ready_yet, _),
	xtAddCallback(ProjectQuitButton, xmNactivateCallback, user_quit, _),
	xmStringCreateLtoR('Project   ', xmFONTLIST_DEFAULT_TAG, ProjectString),
	xtCreateManagedWidget(projectbutton, xmCascadeButtonWidgetClass, MainMenuBar,
	[xmNlabelString(ProjectString),xmNfontList(MenuFontList),
	xmNmnemonic('P'),xmNsubMenuId(ProjectMenu)], _ProjectButton),
	xmStringFree(ProjectString),
/**/

/* Create browse menu */
	xmCreatePulldownMenu(MainMenuBar, browsemenu, [], BrowseMenu),

	/* Create systems styles submenu */
	findall([X,Y], style(X,Y), Styles),
	xmCreatePulldownMenu(BrowseMenu, stylesmenu, [], StylesMenu),
	design_styles_submenu(Styles, StylesMenu),
	/**/

	make_strings([['Current Database',SS1], ['Supported System Styles',SS2]]),
	xtCreateManagedWidget(currentdatabasebutton, xmPushButtonWidgetClass, BrowseMenu, 
	[xmNlabelString(SS1),xmNfontList(MenuFontList)], CurrentDatabaseButton),
	xtCreateManagedWidget(allowedtypesbutton, xmCascadeButtonWidgetClass, BrowseMenu, 
	[xmNlabelString(SS2),xmNfontList(MenuFontList),
	xmNsubMenuId(StylesMenu)], _AllowedTypesButton),
	free_strings([SS1, SS2]),
	xtAddCallback(CurrentDatabaseButton, xmNactivateCallback, open_browsershell, _),
	xmStringCreateLtoR('Browse   ', xmFONTLIST_DEFAULT_TAG, BrowseString),
	xtCreateManagedWidget(browsebutton, xmCascadeButtonWidgetClass, MainMenuBar,
	[xmNlabelString(BrowseString),xmNfontList(MenuFontList),
	xmNsubMenuId(BrowseMenu)], _BrowseButton),
	xmStringFree(BrowseString),
/**/

/* Create analyze menu */
	make_strings([['System',AS1], ['Group',AS2], ['Project',AS3]]),
	xmCreatePulldownMenu(MainMenuBar, analyzemenu, [], AnalyzeMenu),
	xtCreateManagedWidget(analyzesystembutton, xmPushButtonWidgetClass, AnalyzeMenu, 
	[xmNlabelString(AS1),xmNfontList(MenuFontList)], _AnalyzeSpecifyButton),
	xtCreateManagedWidget(analyzegroupbutton, xmPushButtonWidgetClass, AnalyzeMenu, 
	[xmNlabelString(AS2),xmNfontList(MenuFontList)], _AnalyzeGroupButton),
	xtCreateManagedWidget(analyzeprojectbutton, xmPushButtonWidgetClass, AnalyzeMenu, 
	[xmNlabelString(AS3),xmNfontList(MenuFontList)], _AnalyzeProjectButton),
	free_strings([AS1, AS2, AS3]),
	xmStringCreateLtoR('Analyze', xmFONTLIST_DEFAULT_TAG, AnalyzeString),
	xtCreateManagedWidget(analyzebutton, xmCascadeButtonWidgetClass, MainMenuBar,
	[xmNlabelString(AnalyzeString),xmNfontList(MenuFontList),
	xmNmnemonic('A'),xmNsubMenuId(AnalyzeMenu)], _AnalyzeButton),
	xmStringFree(AnalyzeString),
/**/

	create_specshell,
	create_browsershell,
	create_errordialog,
	create_promptdialog,
	create_resultsdialog,

	xtRealizeWidget(Shell),

	event_loop(App),
	flush,
	quit_aaa.

/*	xif_main_loop.*/
/*	xtAppMainLoop(App).*/

/*****************************************************************************/
/*****************************************************************************/
/* Other Functions */

/*****************************************************************************/
/* Miscellaneous Main Window Functions */
initialize_aaa :-
	retractall(aaax(_,_)).

quit_aaa :-
	aaax(app, App),
	aaax(shell, Shell),
	aaax(mainaaafontlist, MainAAAFontList),
	xmFontListFree(MainAAAFontList),
	aaax(menufontlist, MenuFontList),
	xmFontListFree(MenuFontList),
	aaax(font1list, Font1List),
	xmFontListFree(Font1List),
	/* Retract all database stuff */
	retractall(aaax(_,_)),
	xtDestroyApplicationContext(App),
	xtDestroyWidget(Shell),
	flush,
	nl, write('Exit from AAA.'), nl.

event_loop(Context) :-
	repeat,
	(aaax(quitprogram, true) ->
		true;
		(xtNextEvent(XtEvent),
		xtDispatchEvent(XtEvent), /*A ProXT Event?*/
		XtEvent = xtevent(Addr),
		XEvent = xevent(Addr),
		/* dispatch_event/3 fails unless this is an exit condition */
		dispatch_event(XEvent, never_exit, Context))  /*A ProXL Event?*/
	).

design_styles_submenu([], _) :- !.
design_styles_submenu([[Style, InformalStyleName] | OtherStyles], StylesMenu) :-
	aaax(menufontlist, MenuFontList),
	xmStringCreateLtoR(InformalStyleName, xmFONTLIST_DEFAULT_TAG, StyleString),
	xtCreateManagedWidget(Style, xmPushButtonWidgetClass, StylesMenu,
	[xmNlabelString(StyleString),xmNfontList(MenuFontList)], StyleButton),
	xmStringFree(StyleString),
	xtAddCallback(StyleButton, xmNactivateCallback, open_specshell, system(Style)),
	design_styles_submenu(OtherStyles, StylesMenu), !.

update_current_project_label(ProjectFile) :-
	aaax(currentprojectlabel, CurrentProjectLabel),
	atom_chars('Current Project: ', R),
	atom_chars(ProjectFile, P),
	append(R, P, CurrentProjectList),
	atom_chars(CurrentProject, CurrentProjectList),
	xmStringCreateLtoR(CurrentProject, xmFONTLIST_DEFAULT_TAG, CurrentProjectString),
	xtUnmanageChild(CurrentProjectLabel),
	xtSetValues(CurrentProjectLabel, [xmNlabelString(CurrentProjectString)]),
	xtManageChild(CurrentProjectLabel),
	xmStringFree(CurrentProjectString),
	assert(aaax(currentproject, ProjectFile)).

/*****************************************************************************/
/*****************************************************************************/
/* Browser Shell */

create_browsershell :-
	aaax(shell, Shell),
	aaax(mainaaafontlist, MainAAAFontList),
	aaax(menufontlist, MenuFontList),
	xtCreatePopupShell(browsershell, transientShellWidgetClass, Shell, [xmNmwmDecorations(17)], 
	BrowserShell),
	assert(aaax(browsershell, BrowserShell)),
	xtCreateManagedWidget(browserform, xmFormWidgetClass, BrowserShell,
	[], BrowserForm),
	xtCreateManagedWidget(browserformrc, xmRowColumnWidgetClass, BrowserForm,
	[xmNtopAttachment(xmATTACH_FORM),xmNrightAttachment(xmATTACH_FORM),
	xmNleftAttachment(xmATTACH_FORM),xmNorientation(xmHORIZONTAL)], BrowserFormRC),

	/* Create browser "choose type" option area */
	xmCreatePulldownMenu(BrowserFormRC, browserpulldownmenu, [], BrowserPulldownMenu),
	make_strings([['All',BO1],['Ports',BO2],['Data Components',BO3],
	['Control Components',BO4],['Objects',BO5],['Data Connectors',BO6],
	['Control Connectors',BO7],['Triggers',BO8],['Systems',BO9],
	['Groups',BO10]]),
	xtCreateManagedWidget(bo1button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO1),xmNfontList(MenuFontList)], BO1Button),
	xtCreateManagedWidget(bo2button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO2),xmNfontList(MenuFontList)], BO2Button),
	xtCreateManagedWidget(bo3button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO3),xmNfontList(MenuFontList)], BO3Button),
	xtCreateManagedWidget(bo4button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO4),xmNfontList(MenuFontList)], BO4Button),
	xtCreateManagedWidget(bo5button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO5),xmNfontList(MenuFontList)], BO5Button),
	xtCreateManagedWidget(bo6button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO6),xmNfontList(MenuFontList)], BO6Button),
	xtCreateManagedWidget(bo7button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO7),xmNfontList(MenuFontList)], BO7Button),
	xtCreateManagedWidget(bo8button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO8),xmNfontList(MenuFontList)], BO8Button),
	xtCreateManagedWidget(bo9button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO9),xmNfontList(MenuFontList)], BO9Button),
	xtCreateManagedWidget(bo10button, xmPushButtonWidgetClass, BrowserPulldownMenu, 
	[xmNlabelString(BO10),xmNfontList(MenuFontList)], BO10Button),
	free_strings([BO1, BO2, BO3, BO4, BO5, BO6, BO7, BO8, BO9, BO10]),
	xtAddCallback(BO1Button, xmNactivateCallback, change_browser_option, all),
	xtAddCallback(BO2Button, xmNactivateCallback, change_browser_option, port),
	xtAddCallback(BO3Button, xmNactivateCallback, change_browser_option, datacomponent),
	xtAddCallback(BO4Button, xmNactivateCallback, change_browser_option, controlcomponent),
	xtAddCallback(BO5Button, xmNactivateCallback, change_browser_option, object),
	xtAddCallback(BO6Button, xmNactivateCallback, change_browser_option, dataconnector),
	xtAddCallback(BO7Button, xmNactivateCallback, change_browser_option, controlconnector),
	xtAddCallback(BO8Button, xmNactivateCallback, change_browser_option, trigger),
	xtAddCallback(BO9Button, xmNactivateCallback, change_browser_option, system),
	xtAddCallback(BO10Button, xmNactivateCallback, change_browser_option, group),
	xmStringCreateLtoR('Choose Type To Browse:', xmFONTLIST_DEFAULT_TAG, BrowserOptionString),
	xmCreateOptionMenu(BrowserFormRC, browseroptionmenu, [xmNsubMenuId(BrowserPulldownMenu),
	xmNlabelString(BrowserOptionString)], BrowserOptionMenu),
	xmStringFree(BrowserOptionString),
	xtManageChild(BrowserOptionMenu),
	assert(aaax(browsercurrentoption, all)),
	/**/

	xtCreateManagedWidget(browserseparator, xmSeparatorWidgetClass, BrowserForm, 
	[xmNleftAttachment(xmATTACH_FORM), xmNrightAttachment(xmATTACH_FORM),
	xmNtopAttachment(xmATTACH_WIDGET), xmNtopWidget(BrowserFormRC)], BrowserSeparator),
	make_strings([['View',BrowserOKLabelString], ['Exit',BrowserCancelLabelString],
	['Entities ("Style : Entity : Name")',BrowserEntityListString]]),
	xtCreateManagedWidget(browserselectionbox, xmSelectionBoxWidgetClass, BrowserForm,
	[xmNfontList(MainAAAFontList),xmNbottomAttachment(xmATTACH_FORM),
	xmNleftAttachment(xmATTACH_FORM), xmNrightAttachment(xmATTACH_FORM),
	xmNtopAttachment(xmATTACH_WIDGET), xmNtopWidget(BrowserSeparator),
	xmNmessageAlignment(xmALIGNMENT_CENTER), xmNdialogType(xmDIALOG_FILE_SELECTION),
	xmNcancelLabelString(BrowserCancelLabelString),xmNokLabelString(BrowserOKLabelString),
	xmNlistLabelString(BrowserEntityListString)], BrowserSelectionBox),
	free_strings([BrowserOKLabelString, BrowserCancelLabelString, BrowserEntityListString]),
	assert(aaax(browserselectionbox, BrowserSelectionBox)),
	xmSelectionBoxGetChild(BrowserSelectionBox, xmDIALOG_SELECTION_LABEL, BS_DSL),
	xmSelectionBoxGetChild(BrowserSelectionBox, xmDIALOG_TEXT, BS_DT),
	xmSelectionBoxGetChild(BrowserSelectionBox, xmDIALOG_HELP_BUTTON, BS_DHB),
	xmSelectionBoxGetChild(BrowserSelectionBox, xmDIALOG_APPLY_BUTTON, BS_DAB),
	xtUnmanageChildren([BS_DSL, BS_DT, BS_DAB, BS_DHB]),
	xtAddCallback(BrowserSelectionBox, xmNokCallback, ok_from_browser, _),
	xtAddCallback(BrowserSelectionBox, xmNcancelCallback, cancel_from_browser, _).

display_browsershell :-
	aaax(currentproject, CurrentProject), !,
	(aaax(browsershelldisplayed,true) ->
		hide_browsershell;
		true
	),
	aaax(browsershell, BrowserShell),
	aaax(browsercurrentoption, BrowserCurrentOption),
	aaax(browserselectionbox, BrowserSelectionBox),
	atom_chars(CurrentProject, CurrentProjectChars),
	append(["Currently Loaded Entities [Project: ", CurrentProjectChars, "]"], TitleChars),
	atom_chars(Title, TitleChars),
	xtSetValues(BrowserShell, [xmNtitle(Title),xmNiconName(CurrentProject)]),
	collectentities(BrowserCurrentOption, StyleEntityNameList),
	convertbrowserentries(StyleEntityNameList, BrowserEntries),
	length(BrowserEntries, LBE),
	xtUnmanageChild(BrowserSelectionBox),
	xtSetValues(BrowserSelectionBox, [xmNlistItems(BrowserEntries),xmNlistItemCount(LBE)]),
	free_strings(BrowserEntries),
	xtManageChild(BrowserSelectionBox),
	xtPopup(BrowserShell, xtGrabExclusive),
	assert(aaax(browsershelldisplayed, true)).
display_browsershell :-
	display_errordialog('Project Load Error', 'Please load a project file before browsing.').

hide_browsershell :-
	aaax(browsershell, BrowserShell),
	xtPopdown(BrowserShell),
	retractall(aaax(browsershelldisplayed,_)).

/* Produces a list of entities, each one a [InformalStyleName, Entity, Name] */
collectentities(all, FinalList) :-
	findall(L, (member(E, [port, datacomponent, controlcomponent, object, dataconnector,
	controlconnector, trigger, system, group]), collectentities(E, L)), F1),
	union(F1, FinalList).
collectentities(Entity, FinalList) :-
	Entity \== all,
	Type =.. [Entity, base],
	getdescendanttypes(Type, DescendantTypes),
	append([Type], DescendantTypes, AllTypes),
	findall(L, (member(T,AllTypes),collectsingletypeentities(T, L)), F1),
	union(F1, FinalList), !.

collectsingletypeentities(Type, List) :-
	getallentities(Type, Entities),
	Type =.. [Entity, Style],
	style(Style, InformalStyleName),
	findall([InformalStyleName, Entity, E], member(E, Entities), List).

convertbrowserentries([], []).
convertbrowserentries([H | T], [HS | TS]) :-
	convertbrowserentry(H, HS),
	convertbrowserentries(T, TS).

convertbrowserentry([InformalStyleName, Entity, Name], EntryString) :-
	atom_chars(InformalStyleName, InformalStyleNameChars),
	atom_chars(Entity, EntityChars),
	atom_chars(Name, NameChars),
	append([InformalStyleNameChars, " : ", EntityChars, " : ", NameChars], EntryChars),
	atom_chars(Entry, EntryChars),
	make_strings([[Entry, EntryString]]).

/*****************************************************************************/
/*****************************************************************************/
/* Error Dialog */

create_errordialog :-
	aaax(shell, Shell),
	aaax(mainaaafontlist, MainAAAFontList),
	xtCreatePopupShell(errordialog, transientShellWidgetClass, Shell, [xmNmwmDecorations(17)],
	ErrorDialog),
	assert(aaax(errordialog, ErrorDialog)),
	xtCreateManagedWidget(errordialogmessagebox, xmMessageBoxWidgetClass, ErrorDialog,
	[xmNfontList(MainAAAFontList),xmNmessageAlignment(xmALIGNMENT_CENTER)], ErrorDialogMessageBox),
	assert(aaax(errordialogmessagebox, ErrorDialogMessageBox)),
	xmMessageBoxGetChild(ErrorDialogMessageBox, xmDIALOG_CANCEL_BUTTON, ED_DCB),
	xmMessageBoxGetChild(ErrorDialogMessageBox, xmDIALOG_HELP_BUTTON, ED_DHB),
	xtUnmanageChildren([ED_DCB, ED_DHB]),
	xtAddCallback(ErrorDialogMessageBox, xmNokCallback, ok_from_errordialog, _).

display_errordialog(Title, Error) :-
	(aaax(errordialogdisplayed,true) ->
		hide_errordialog;
		true
	),
	aaax(errordialog, ErrorDialog),
	aaax(errordialogmessagebox, ErrorDialogMessageBox),
	atom_chars(Error, ErrorChars),
	length(ErrorChars, LE),
	ErrorWindowWidth is LE * 13 + 20,
	centerwindow(110, ErrorWindowWidth, Position),
	xtUnrealizeWidget(ErrorDialog), /* Necessary to reflect new position */
	xtSetValues(ErrorDialog, [xmNwidth(ErrorWindowWidth), xmNheight(110),
	xmNgeometry(Position), xmNtitle(Title)]),
	xmStringCreateLtoR(Error, xmFONTLIST_DEFAULT_TAG, ErrorString),
	xtSetValues(ErrorDialogMessageBox, [xmNmessageString(ErrorString)]),
	xmStringFree(ErrorString),
	xtRealizeWidget(ErrorDialog),
	xtPopup(ErrorDialog, xtGrabExclusive),
	assert(aaax(errordialogdisplayed, true)).

hide_errordialog :-
	aaax(errordialog, ErrorDialog),
	xtPopdown(ErrorDialog),
	retractall(aaax(errordialogdisplayed,_)).

/*****************************************************************************/
/*****************************************************************************/
/* Prompt Dialog */

create_promptdialog :-
	aaax(shell, Shell),
	aaax(mainaaafontlist, MainAAAFontList),
	xtCreatePopupShell(promptdialog, transientShellWidgetClass, Shell, [xmNmwmDecorations(17)],
	PromptDialog),
	assert(aaax(promptdialog, PromptDialog)),
	xtCreateManagedWidget(promptdialogselectionbox, xmSelectionBoxWidgetClass, PromptDialog,
	[xmNfontList(MainAAAFontList),xmNmessageAlignment(xmALIGNMENT_CENTER),
	xmNdialogType(xmDIALOG_PROMPT)], PromptDialogSelectionBox),
	assert(aaax(promptdialogselectionbox, PromptDialogSelectionBox)),
	xmSelectionBoxGetChild(PromptDialogSelectionBox, xmDIALOG_HELP_BUTTON, PD_DHB),
	xtUnmanageChild(PD_DHB),
	xtAddCallback(PromptDialogSelectionBox, xmNokCallback, ok_from_promptdialog, _),
	xtAddCallback(PromptDialogSelectionBox, xmNcancelCallback, cancel_from_promptdialog, _).

display_promptdialog(Title, Message) :-
	(aaax(promptdialogdisplayed,true) ->
		hide_promptdialog;
		true
	),
	aaax(promptdialog, PromptDialog),
	aaax(promptdialogselectionbox, PromptDialogSelectionBox),
	atom_chars(Message, MessageChars),
	length(MessageChars, LM),
	PromptWindowWidth is LM * 13 + 20,
	centerwindow(140, PromptWindowWidth, Position),
	xtUnrealizeWidget(PromptDialog), /* Necessary to reflect new position */
	xtSetValues(PromptDialog, [xmNwidth(PromptWindowWidth), xmNheight(140),
	xmNgeometry(Position), xmNtitle(Title)]),
	xmStringCreateLtoR(Message, xmFONTLIST_DEFAULT_TAG, MessageString),
	xtSetValues(PromptDialogSelectionBox, [xmNselectionLabelString(MessageString)]),
	xmStringFree(MessageString),
	xtRealizeWidget(PromptDialog),
	xtPopup(PromptDialog, xtGrabExclusive),
	assert(aaax(promptdialogdisplayed, true)).

hide_promptdialog :-
	aaax(promptdialog, PromptDialog),
	xtPopdown(PromptDialog),
	retractall(aaax(promptdialogdisplayed,_)).

/*****************************************************************************/
/*****************************************************************************/
/* Results Dialog */

create_resultsdialog :-
	aaax(shell, Shell),
	aaax(mainaaafontlist, MainAAAFontList),
	xtCreatePopupShell(resultsdialog, transientShellWidgetClass, Shell,
	[xmNmwmDecorations(17)], ResultsDialog),
	assert(aaax(resultsdialog, ResultsDialog)),
	xtCreateManagedWidget(resultsdialogform, xmFormWidgetClass, ResultsDialog,
	[], ResultsDialogForm),
	xmStringCreateLtoR('OK', xmFONTLIST_DEFAULT_TAG, ResultsOKString),
	xtCreateManagedWidget(resultsok, xmPushButtonWidgetClass, ResultsDialogForm,
	[xmNfontList(MainAAAFontList),xmNlabelString(ResultsOKString),
	xmNalignment(xmALIGNMENT_CENTER),xmNbottomAttachment(xmATTACH_FORM),
	xmNheight(30),xmNleftAttachment(xmATTACH_FORM),xmNrightAttachment(xmATTACH_FORM)],
	ResultsOKButton),
	xmStringFree(ResultsOKString),
	xmCreateScrolledText(ResultsDialogForm, resultsdialogtext, 
	[xmNeditable(false),xmNfontList(MainAAAFontList),xmNeditMode(xmMULTI_LINE_EDIT),
	xmNwordWrap(true),xmNtopAttachment(xmATTACH_FORM),xmNbottomAttachment(xmATTACH_WIDGET),
	xmNbottomWidget(ResultsOKButton),xmNleftAttachment(xmATTACH_FORM),
	xmNrightAttachment(xmATTACH_FORM)], ResultsDialogText),
	assert(aaax(resultsdialogtext, ResultsDialogText)),
	xtAddCallback(ResultsOKButton, xmNactivateCallback, ok_from_resultsdialog, _),
	xtManageChild(ResultsDialogText).

display_resultsdialog(Title) :-
	(aaax(resultsdialogdisplayed,true) ->
		hide_resultsdialog;
		true
	),
	aaax(resultsdialog, ResultsDialog),
	xtSetValues(ResultsDialog, [xmNtitle(Title)]),
	aaax(resultsdialogtext, ResultsDialogText),
	aaax(screenheight, ScreenHeight),
	aaax(screenwidth, ScreenWidth),
	HalfScreenWidth is ScreenWidth div 2,
	xtSetValues(ResultsDialog, [xmNwidth(HalfScreenWidth),xmNheight(ScreenHeight)]),
	xmTextShowPosition(ResultsDialogText, 0),
	xtPopup(ResultsDialog, xtGrabExclusive),
	xmProcessTraversal(ResultsDialogText, xmTRAVERSE_CURRENT),
	assert(aaax(resultsdialogdisplayed, true)).

hide_resultsdialog :-
	aaax(resultsdialog, ResultsDialog),
	xtPopdown(ResultsDialog),
	retractall(aaax(resultsdialogdisplayed,_)).

readinresults(File) :-
	aaax(resultsdialogtext, ResultsDialogText),
	xmTextSetString(ResultsDialogText, ''),
	see(File),
	get_lines_into_text(ResultsDialogText),
	seen.

/*****************************************************************************/
/*****************************************************************************/
/* Specification Shell */

create_specshell :-
	aaax(shell, Shell),
	aaax(mainaaafontlist, MainAAAFontList),
	aaax(menufontlist, MenuFontList),
	xtCreatePopupShell(specshell, transientShellWidgetClass, Shell, 
	[xmNmwmDecorations(17),xmNtitle('Entity Specification')], SpecShell),
	assert(aaax(specshell, SpecShell)),
	xtCreateManagedWidget(specshellform, xmFormWidgetClass, SpecShell, [], SpecShellForm),
	assert(aaax(specshellform, SpecShellForm)),
	xtCreateManagedWidget(specshellnameform, xmFormWidgetClass, SpecShellForm,
	[xmNrightAttachment(xmATTACH_FORM),xmNleftAttachment(xmATTACH_FORM),
	xmNtopAttachment(xmATTACH_FORM)], SpecShellNameForm),

	xtCreateManagedWidget(ssstyle, xmLabelWidgetClass, SpecShellNameForm,
	[xmNalignment(xmALIGNMENT_CENTER),xmNfontList(MenuFontList),
	xmNrightAttachment(xmATTACH_FORM),xmNleftAttachment(xmATTACH_FORM),
	xmNtopAttachment(xmATTACH_FORM)], SpecShellTypeLabel),
	assert(aaax(specshelltypelabel, SpecShellTypeLabel)),

	xtCreateManagedWidget(specshellrc, xmRowColumnWidgetClass, SpecShellForm,
	[xmNrightAttachment(xmATTACH_FORM),xmNleftAttachment(xmATTACH_FORM),
	xmNtopAttachment(xmATTACH_WIDGET),xmNtopWidget(SpecShellNameForm),
	xmNbackground(0)], SpecShellRC),
	assert(aaax(specshellrc, SpecShellRC)),

	xtCreateManagedWidget(ssseparator, xmSeparatorWidgetClass, SpecShellForm,
	[xmNleftAttachment(xmATTACH_FORM),xmNrightAttachment(xmATTACH_FORM),
	xmNtopAttachment(xmATTACH_WIDGET),xmNtopWidget(SpecShellRC)], SpecShellSeparator),

	xmStringCreateLtoR('Exit', xmFONTLIST_DEFAULT_TAG, SSExitString),
	xtCreateManagedWidget(ssexit, xmPushButtonWidgetClass, SpecShellForm, 
	[xmNfontList(MainAAAFontList),xmNlabelString(SSExitString),xmNtopAttachment(xmATTACH_WIDGET),
	xmNbottomAttachment(xmATTACH_FORM),xmNleftAttachment(xmATTACH_POSITION),
	xmNleftPosition(50),
	xmNtopWidget(SpecShellSeparator),xmNrightAttachment(xmATTACH_FORM)], SSExit),
	xmStringFree(SSExitString),

	xmStringCreateLtoR('Analyze', xmFONTLIST_DEFAULT_TAG, SSAnalyzeString),
	xtCreateManagedWidget(ssanalyze, xmPushButtonWidgetClass, SpecShellForm, 
	[xmNfontList(MainAAAFontList),xmNlabelString(SSAnalyzeString),
	xmNtopAttachment(xmATTACH_WIDGET),
	xmNbottomAttachment(xmATTACH_FORM),xmNrightAttachment(xmATTACH_POSITION),
	xmNrightPosition(50),
	xmNtopWidget(SpecShellSeparator),xmNleftAttachment(xmATTACH_FORM)], SSAnalyze),
	xmStringFree(SSAnalyzeString),

	xtAddCallback(SSExit, xmNactivateCallback, exit_from_specshell, _),
	xtAddCallback(SSAnalyze, xmNactivateCallback, analyze_from_specshell, _).

display_specshell(Type, EntityName) :-
	(aaax(specshelldisplayed,true) ->
		hide_specshell;
		true
	),
	aaax(specshell, SpecShell),
	design_specshell(Type, EntityName, ApproximateRows),
	Height is ApproximateRows * 36 + 100,
	xtSetValues(SpecShell, [xmNwidth(625),xmNheight(Height)]),
	xtPopup(SpecShell, xtGrabNonexclusive),
	assert(aaax(specshelldisplayed, true)),
	(EntityName == unspecified ->
		true;
		assert(aaax(currententityname, EntityName))
	).

hide_specshell :-
	aaax(specshell, SpecShell),
	xtPopdown(SpecShell),
	retractall(aaax(specshelldisplayed,_)),
	undesign_specshell.

design_specshell(Type, EntityName, ApproximateRows) :-
	gettypeschemaattributes(Type, Attributes, _, _),
	aaax(specshellrc, SpecShellRC),
	aaax(mainaaafontlist, MainAAAFontList),
	Type =.. [Entity, Style],
	assert(aaax(currentstyle, Style)),
	assert(aaax(currententity, Entity)),

	atom_chars(Entity, [FirstChar | OtherChars]),
	to_upper(FirstChar, UpperFirstChar),
	style(Style, InformalStyleName),
	atom_chars(InformalStyleName, InformalStyleNameChars),
	append(["Style: ", InformalStyleNameChars, ", Entity: ", [UpperFirstChar | OtherChars]],
	TypeLabelChars),
	atom_chars(TypeLabel, TypeLabelChars),

	aaax(specshelltypelabel, SpecShellTypeLabel),
	xmStringCreateLtoR(TypeLabel, xmFONTLIST_DEFAULT_TAG, TypeLabelString),
	xtSetValues(SpecShellTypeLabel, [xmNlabelString(TypeLabelString)]),
	xmStringFree(TypeLabelString),

	xtCreateManagedWidget(ssnamerow, xmFormWidgetClass, SpecShellRC, [], NameRow),
	xmStringCreateLtoR('Name:', xmFONTLIST_DEFAULT_TAG, NameLabelString),
	xtCreateManagedWidget(ssnamelabel, xmLabelWidgetClass, NameRow,
	[xmNfontList(MainAAAFontList),xmNlabelString(NameLabelString),
	xmNleftAttachment(xmATTACH_FORM)], NameLabelWidget),
	xmStringFree(NameLabelString),

	xtCreateManagedWidget(namefield, xmTextFieldWidgetClass, NameRow,
	[xmNfontList(MainAAAFontList),xmNrightAttachment(xmATTACH_FORM),xmNeditable(false),
	xmNleftAttachment(xmATTACH_WIDGET),xmNleftWidget(NameLabelWidget)], NameFieldWidget),
	xmTextSetString(NameFieldWidget, EntityName),

	xtCreateManagedWidget(ssseparator, xmSeparatorWidgetClass, SpecShellRC, 
	[], SeparatorWidget),

	design_specshell(Type, EntityName, Attributes, 0, [NameRow, NameLabelWidget, 
	NameFieldWidget, SeparatorWidget], SpecShellWidgets),
	length(Attributes, ApproximateRows),
	assert(aaax(specshellwidgets, SpecShellWidgets)).

design_specshell(_, _, [], _, SpecShellWidgets, SpecShellWidgets) :- !.
design_specshell(Type, EntityName, [Attribute | OtherAttributes], Counter, Accum, 
	SpecShellWidgets) :-
	aaax(specshellrc, SpecShellRC),
	aaax(mainaaafontlist, MainAAAFontList),
	Type =.. [Entity, _],
	(Entity == system ->
		getinformalname(Type, Attribute, InformalName);
		(getbaseancestor(Type, BaseType),
		getinformalname(BaseType, Attribute, InformalName))
	),
	xmStringCreateLtoR(InformalName, xmFONTLIST_DEFAULT_TAG, InformalNameString),
	makewidgetname(ssrow, Counter, RowName),
	makewidgetname(sslabel, Counter, LabelName),
	makewidgetname(ssfield, Counter, FieldName),
	xtCreateManagedWidget(RowName, xmFormWidgetClass, SpecShellRC, [], RowWidget),
	xtCreateManagedWidget(LabelName, xmLabelWidgetClass, RowWidget,
	[xmNfontList(MainAAAFontList),xmNlabelString(InformalNameString),
	xmNleftAttachment(xmATTACH_FORM)], LabelWidget),
	xmStringFree(InformalNameString),
	xtCreateManagedWidget(FieldName, xmTextFieldWidgetClass, RowWidget,
	[xmNfontList(MainAAAFontList),xmNrightAttachment(xmATTACH_FORM),xmNeditable(false),
	xmNleftAttachment(xmATTACH_WIDGET),xmNleftWidget(LabelWidget)], TextFieldWidget),

	(EntityName == unspecified ->
		true;
		(getvalue(Entity, _, EntityName, Attribute, ValueList),
		listtoatom(ValueList, ValueAtom),
/*		tell(tempo2), write(ValueList), nl, told,
		see(tempo2), get_line(Chars, _Terminator), seen,
		atom_chars(ValueAtom, Chars),*/
		xmTextSetString(TextFieldWidget, ValueAtom))
	),

	NewCounter is Counter + 1,
	design_specshell(Type, EntityName, OtherAttributes, NewCounter,
	[RowWidget, LabelWidget, TextFieldWidget | Accum], SpecShellWidgets), !.

undesign_specshell :-
	aaax(specshellwidgets, SpecShellWidgets),
	destroy_widgets(SpecShellWidgets),
	retractall(aaax(currententityname,_)),
	retractall(aaax(currententity,_)),
	retractall(aaax(currentstyle,_)),
	retractall(aaax(specshellwidgets,_)).

/*****************************************************************************/
/*****************************************************************************/
get_lines_into_text(TextWidget) :-
	get_line(Chars, Terminator),
	append(Chars, [10], CharsT),
	length(CharsT, L),
	atom_chars(Line, CharsT),
	xmTextGetInsertionPosition(TextWidget, Position),
	xmTextInsert(TextWidget, Position, Line),
	NewPosition is Position + L,
	xmTextSetInsertionPosition(TextWidget, NewPosition),
	(is_endfile(Terminator) ->
		true;
		get_lines_into_text(TextWidget)
	).

centerwindow(WindowHeight, WindowWidth, Position) :-
	aaax(screenheight, ScreenHeight),
	aaax(screenwidth, ScreenWidth),
	LeftSide is (ScreenWidth - WindowWidth) div 2,
	TopSide is (ScreenHeight - WindowHeight) div 2,
	number_chars(LeftSide, LeftSideChars),
	number_chars(TopSide, TopSideChars),
	append(["+", LeftSideChars, "+", TopSideChars], PositionChars),
	atom_chars(Position, PositionChars).

makewidgetname(Prefix, Counter, WidgetName) :-
	number_chars(Counter, CounterChars),
	atom_chars(Prefix, PrefixChars),
	/* Append 'group_for_' */
	append(PrefixChars, CounterChars, WidgetChars),
	atom_chars(WidgetName, WidgetChars).

destroy_widgets([]) :- !.
destroy_widgets([W | TW]) :-
	xtDestroyWidget(W),
	destroy_widgets(TW).

make_strings([]) :- !.
make_strings([[Atom, String] | T]) :-
	xmStringCreateLtoR(Atom, xmFONTLIST_DEFAULT_TAG, String),
	make_strings(T).

free_strings([]) :- !.
free_strings([H | T]) :-
	xmStringFree(H),
	free_strings(T), !.
