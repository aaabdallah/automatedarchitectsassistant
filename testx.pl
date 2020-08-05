:- ensure_loaded(library(proxt)).

:- dynamic aaax/2.

/* Callbacks */

quit_aaa(_Widget, _ClientData, _CallData) :-
	aaax(app, App),
	xtDestroyApplicationContext(App),
	aaax(mainaaafontlist, MainAAAFontList),
	xmFontListFree(MainAAAFontList),
	aaax(menufontlist, MenuFontList),
	xmFontListFree(MenuFontList),
/* Retract all database stuff */
	retractall(aaax(_,_)),
	raise_exception('Exiting Normally.').

enter_into_specshell(_Widget, _ClientData, _CallData).

ok_from_errordialog(_Widget, _ClientData, _CallData) :-
	hide_errordialog.

cancel_from_specshell(_Widget, _ClientData, _CallData) :-
	hide_specshell.

runtime_entry(start) :-
	do.

/* Main routine */
test :-
	xtAppInitialize(App, 'AAA', [], Shell),
	assert(aaax(app,App)),
	assert(aaax(toplevel,Shell)),
	xtDisplay(Shell, Display),
	assert(aaax(display, Display)),

/* Create AAA's fonts */
	xtSetValues(Shell, [xmNtitle('Architects Automated Assistant')]),
	xLoadQueryFont(Display, '7x13bold', MainAAAFont),
	xmFontListCreate(MainAAAFont, xmFONTLIST_DEFAULT_TAG, MainAAAFontList),
	assert(aaax(mainaaafontlist, MainAAAFontList)),
	xLoadQueryFont(Display, '10x20', MenuFont),
	xmFontListCreate(MenuFont, xmFONTLIST_DEFAULT_TAG, MenuFontList),
	assert(aaax(menufontlist, MenuFontList)),

	xtCreateManagedWidget(master, xmRowColumnWidgetClass, Shell, [], Master),
	xtCreateManagedWidget(row1, xmRowColumnWidgetClass, Master, [], Row1),

/* Create main menu bar */
	xtCreateManagedWidget(mainmenubar, xmRowColumnWidgetClass, Row1, [xmNrowColumnType(xmMENU_BAR),
	xmNentryClass(xmCascadeButtonWidgetClass),xmNisHomogeneous(true),
	xmNmenuAccelerator('KMenuBar'),xmNorientation(xmHORIZONTAL),
	xmNmenuPost('<Btn1Down>')], MainMenuBar),
	xmStringCreateLtoR('Project', xmFONTLIST_DEFAULT_TAG, ProjectString),
	xtCreateManagedWidget(projectbutton, xmCascadeButtonWidgetClass, MainMenuBar,
	[xmNlabelString(ProjectString),xmNfontList(MenuFontList),xmNmnemonic('P')], ProjectButton),
	xmStringFree(ProjectString),
/**/

/* Create main menus */
	xmStringCreateLtoR('Load', xmFONTLIST_DEFAULT_TAG, PS1),
	xmCreatePulldownMenu(MainMenuBar, projectmenu, [], ProjectMenu),
	xtManageChild(ProjectMenu),
/*	[xmNbuttonCount(1),xmNrowColumnType(xmMENU_PULLDOWN),
	xmNbuttons([PS1]),xmNpostFromButton(0)],ProjectMenu),*/
	xmStringFree(PS1),
	xtSetValues(ProjectButton, [xmNsubMenuId(ProjectMenu)]),

	xtCreateManagedWidget(brow, xmRowColumnWidgetClass, Master, [xmNorientation(xmHORIZONTAL)],
	BRow),

	xmStringCreateLtoR('Specify System', xmFONTLIST_DEFAULT_TAG, SSString),
	xtCreateManagedWidget(specifybutton, xmPushButtonWidgetClass, BRow,
	[xmNfontList(MainAAAFontList),xmNlabelString(SSString)], SpecifyButton),
	xmStringFree(SSString),

	xmStringCreateLtoR('Quit AAA', xmFONTLIST_DEFAULT_TAG, QuitString),
	xtCreateManagedWidget(quitbutton, xmPushButtonWidgetClass, BRow,
	[xmNfontList(MainAAAFontList),xmNlabelString(QuitString)], QuitButton),
	xmStringFree(QuitString),

	xtAddCallback(SpecifyButton, xmNactivateCallback, enter_into_specshell, _),
	xtAddCallback(QuitButton, xmNactivateCallback, quit_aaa, _),

/* Specification popup shell */
	xtCreatePopupShell(specshell, transientShellWidgetClass, Shell, [xmNmwmDecorations(17)],
	SpecShell),
	assert(aaax(specshell, SpecShell)),
	xtCreateManagedWidget(specshellform, xmFormWidgetClass, SpecShell, [], SpecShellForm),
	assert(aaax(specshellform, SpecShellForm)),
	xtCreateManagedWidget(specshellrc, xmRowColumnWidgetClass, SpecShellForm,
	[xmNrightAttachment(xmATTACH_FORM),xmNleftAttachment(xmATTACH_FORM),xmNbackground(0)],
	SpecShellRC),
	assert(aaax(specshellrc, SpecShellRC)),

	xtCreateManagedWidget(ssseparator, xmSeparatorWidgetClass, SpecShellForm,
	[xmNleftAttachment(xmATTACH_FORM),xmNrightAttachment(xmATTACH_FORM),
	xmNtopAttachment(xmATTACH_WIDGET),xmNtopWidget(SpecShellRC)], SpecShellSeparator),

	xmStringCreateLtoR('Cancel', xmFONTLIST_DEFAULT_TAG, SSCancelString),
	xtCreateManagedWidget(sscancel, xmPushButtonWidgetClass, SpecShellForm, 
	[xmNfontList(MainAAAFontList),xmNlabelString(SSCancelString),xmNtopAttachment(xmATTACH_WIDGET),
	xmNtopWidget(SpecShellSeparator),xmNrightAttachment(xmATTACH_FORM)], SSCancel),
	xmStringFree(SSCancelString),

	xmStringCreateLtoR('Enter', xmFONTLIST_DEFAULT_TAG, SSEnterString),
	xtCreateManagedWidget(ssenter, xmPushButtonWidgetClass, SpecShellForm, 
	[xmNfontList(MainAAAFontList),xmNlabelString(SSEnterString),xmNtopAttachment(xmATTACH_WIDGET),
	xmNtopWidget(SpecShellSeparator),xmNleftAttachment(xmATTACH_FORM)], _SSEnter),
	xmStringFree(SSEnterString),

	xtAddCallback(SSCancel, xmNactivateCallback, cancel_from_specshell, _),

/* Error Dialog */
	xtCreatePopupShell(errordialog, transientShellWidgetClass, Shell, [xmNmwmDecorations(17)],
	ErrorDialog),
	assert(aaax(errordialog, ErrorDialog)),
	xmStringCreateLtoR('ERROR', xmFONTLIST_DEFAULT_TAG, ErrorDialogTitleString),

	xtCreateManagedWidget(errordialogmessage, xmMessageBoxWidgetClass, ErrorDialog,
	[xmNfontList(MainAAAFontList),xmNdialogTitle(ErrorDialogTitleString),
	xmNmessageAlignment(xmALIGNMENT_CENTER)],ErrorDialogMessageBox),

	xmStringFree(ErrorDialogTitleString),
	assert(aaax(errordialogmessagebox, ErrorDialogMessageBox)),
	xtAddCallback(ErrorDialogMessageBox, xmNokCallback, ok_from_errordialog, _),

	xmMessageBoxGetChild(ErrorDialogMessageBox, xmDIALOG_CANCEL_BUTTON, DCB),
	xmMessageBoxGetChild(ErrorDialogMessageBox, xmDIALOG_HELP_BUTTON, DHB),
	xtUnmanageChildren([DCB, DHB]),

	xtRealizeWidget(Shell),
	xtAppMainLoop(App).

display_errordialog(Error) :-
	aaax(errordialog, ErrorDialog),
	aaax(errordialogmessagebox, ErrorDialogMessageBox),
	xmStringCreateLtoR(Error, xmFONTLIST_DEFAULT_TAG, ErrorString),
	xtSetValues(ErrorDialogMessageBox, [xmNmessageString(ErrorString)]),
	xmStringFree(ErrorString),
	xtPopup(ErrorDialog, xtGrabExclusive).

hide_errordialog :-
	aaax(errordialog, ErrorDialog),
	xtPopdown(ErrorDialog).

display_specshell(Type) :-
	aaax(specshell, SpecShell),
	design_specshell(Type),
	xtPopup(SpecShell, xtGrabNone).

hide_specshell :-
	aaax(specshell, SpecShell),
	xtPopdown(SpecShell),
	undesign_specshell.

design_specshell(Type) :-
	gettypeschemaattributes(Type, Attributes, _, _),
	design_specshell(Type, Attributes, 0, [], SpecShellWidgets),
	assert(aaax(specshellwidgets, SpecShellWidgets)).

design_specshell(_, [], _, SpecShellWidgets, SpecShellWidgets) :- !.
design_specshell(Type, [Attribute | OtherAttributes], Counter, Accum, SpecShellWidgets) :-
	aaax(specshellrc, SpecShellRC),
	aaax(mainaaafontlist, MainAAAFontList),
	getinformalname(Type, Attribute, InformalName),
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
	[xmNfontList(MainAAAFontList),xmNrightAttachment(xmATTACH_FORM),
	xmNleftAttachment(xmATTACH_WIDGET),xmNleftWidget(LabelWidget)], TextFieldWidget),

	NewCounter is Counter + 1,
	design_specshell(Type, OtherAttributes, NewCounter,
	[RowWidget, LabelWidget, TextFieldWidget | Accum], SpecShellWidgets), !.

undesign_specshell :-
	aaax(specshellwidgets, SpecShellWidgets),
	destroy_widgets(SpecShellWidgets),
	retractall(aaax(specshellwidgets,_)).

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

