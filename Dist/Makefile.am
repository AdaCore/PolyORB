##
## Makefile.am,v 1.8 1996/05/11 13:09:51 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

GNATSRC = @GNAT_SOURCE@
GARLIC = ${libdir}/garlic
GNATFLAGS = -gnatpg @GNATFLAGS@
EXTRA_LIBS = @EXTRA_LIBS@
GNATMAKE = @GNATMAKE@
RSH = @RSH@

bin_SCRIPTS = gnatdist
libexec_PROGRAMS = dist1
dist1_DEPENDENCIES = xe_defs.o sdefault.o
dist1_SOURCES =
dist1_LDADD =

CLEANFILES = *.ali gnatdist xe_defs.adb sdefault.adb

force: 

dist1: ${dist1_DEPENDENCIES} force
	${GNATMAKE} -a ${GNATFLAGS} -I. -I${srcdir} -I${GNATSRC} \
	   ${srcdir}/xe_build.adb -o dist1

xe_defs.adb: Makefile
	@echo Generating xe_defs.adb
	@echo "package body XE_Defs is" >xe_defs.adb
	@echo "   GARLIC_Dir : aliased constant String" >>xe_defs.adb
	@echo "      := \"${GARLIC}\";" >>xe_defs.adb
	@for i in GARLIC; do \
		echo "   function Get_"$$i"_Dir return String_Access is"; \
		echo "   begin"; \
		echo "      return new String'("$$i"_Dir);"; \
		echo "   end Get_"$$i"_Dir;"; \
	done >> xe_defs.adb
	@echo "   function Get_Rsh_Command return String is" >>xe_defs.adb
	@echo "   begin" >>xe_defs.adb
	@echo "      return \"${RSH}\";" >>xe_defs.adb
	@echo "   end Get_Rsh_Command;" >>xe_defs.adb
	@echo "end XE_Defs;" >>xe_defs.adb

xe_defs.o: xe_defs.ads xe_defs.adb
	${CC} -c -g -O2 -gnatp -I${srcdir} -I${GNATSRC} xe_defs.adb

sdefault.adb: ${srcdir}/config.sdefault
	@echo Generating sdefault.adb
	@if [ -f ${GNATSRC}/sdefault.adb ]; then \
	  cp ${GNATSRC}/sdefault.adb .; else \
	  ${SHELL} ${srcdir}/config.sdefault; fi

sdefault.o: sdefault.adb ${GNATSRC}/sdefault.ads
	${CC} -c -g -O2 -gnatp -I${GNATSRC} sdefault.adb

gnatdist: Makefile
	@echo '#!/bin/sh' >gnatdist
	@echo "PATH=${libexecdir}"':$${PATH}' >> gnatdist
	@echo "export PATH" >> gnatdist
	@echo 'if [ $$# = 0 ]; then' >>gnatdist
	@echo "   dist1" >>gnatdist
	@echo "else" >>gnatdist
	@echo '   dist1' -I${GARLIC} '"$$@"' -largs ${EXTRA_LIBS} >> gnatdist
	@echo "fi" >>gnatdist
	@chmod 755 gnatdist
	@echo "gnatdist has been generated"
