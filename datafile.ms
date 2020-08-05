style mainsubroutine
	procedure display
	end;
	procedure edit
	end;
	procedure store
	end;
	procedure auto
	end;
	procedure editormain
	end;
	procedurecall main2edit
		caller: editormain;
		callee: edit;
	end;
	procedurecall edit2display
		caller: edit;
		callee: display;
	end;
	procedurecall edit2store
		caller: edit;
		callee: store;
	end;
	procedurecall main2auto
		caller: editormain;
		callee: auto;
	end;
	system editor
		main: editormain;
		subroutines: edit, display, store, auto;
		procedurecalls: main2edit, edit2display, edit2store, main2auto;
	end;

	procedure analyzermain
	end;
	procedure analyze
	end;
	procedure report
	end;
	procedurecall main2analyze
		caller: analyzermain;
		callee: analyze;
	end;
	procedurecall analyze2report
		caller: analyze;
		callee: report;
	end;
	procedurecall main2report
		caller: analyzermain;
		callee: report;
	end;
	system analyzer
		main: analyzermain;
		subroutines: analyze, report;
		procedurecalls: main2analyze, analyze2report, main2report;
	end;

	procedure bigmain
	end;

	procedurecall bigmain2editormain
		caller: bigmain;
		callee: editormain;
	end;

	procedurecall bigmain2analyzermain
		caller: bigmain;
		callee: analyzermain;
	end;

	construction entiresystem
		main: bigmain;
		subsystem1: editor;
		subsystem2: analyzer;
		interprocedurecalls: bigmain2editormain, bigmain2analyzermain;
	end;
end;
