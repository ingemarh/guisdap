#   File:       TLAN2PS.make
#   Target:     TLAN2PS
#   Sources:    etlan.c
#               tlan2ps.c
#   Created:    Monday, January 8, 1996 01:43:36 PM


MAKEFILE     = TLAN2PS.make
ÄMondoBuildÄ = {MAKEFILE}  # Make blank to avoid rebuilds when makefile is modified
Includes     =
SymÄPPC      = 
ObjDirÄPPC   =

PPCCOptions  = {Includes} {SymÄPPC} 

ObjectsÄPPC  = è
		"{ObjDirÄPPC}etlan.c.x" è
		"{ObjDirÄPPC}tlan2ps.c.x"


TLAN2PS üü {ÄMondoBuildÄ} {ObjectsÄPPC}
	PPCLink è
		-o {Targ} {SymÄPPC} è
		{ObjectsÄPPC} è
		-t 'MPST' è
		-c 'MPS ' è
		"{SharedLibraries}InterfaceLib" è
		"{SharedLibraries}StdCLib" è
		"{SharedLibraries}MathLib" è
		"{PPCLibraries}StdCRuntime.o" è
		"{PPCLibraries}PPCCRuntime.o" è
		"{PPCLibraries}PPCToolLibs.o"


"{ObjDirÄPPC}etlan.c.x" ü {ÄMondoBuildÄ} etlan.c
	{PPCC} etlan.c -o {Targ} {PPCCOptions}

"{ObjDirÄPPC}tlan2ps.c.x" ü {ÄMondoBuildÄ} tlan2ps.c
	{PPCC} tlan2ps.c -o {Targ} {PPCCOptions}

