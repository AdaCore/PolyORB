##
## Makefile.am,v 1.8 1996/05/11 13:09:51 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

ADA = @ADA@
GNATMAKE = @GNATMAKE@
GNATSRC = @GNAT_SOURCE@
GARLIC = ${libdir}/garlic
GNATFLAGS = -gnatpg @GNATFLAGS@
RSH = @RSH@
srcdir = @srcdir@

bin_PROGRAMS = gnatdist
gnatdist_DEPENDENCIES = xe_defs.o sdefault.o
gnatdist_SOURCES =
gnatdist_LDADD =

GNATMAKE = gnatmake

CLEANFILES = *.ali gnatdist$(EXEEXT) xe_defs.adb sdefault.adb

force: 

gnatdist$(EXEEXT): ${gnatdist_DEPENDENCIES} force
	${GNATMAKE} -m ${GNATFLAGS} -I. -I${srcdir} -I${GNATSRC} \
	   ${srcdir}/xe_build.adb -o gnatdist$(EXEEXT)

xe_defs.adb: Makefile
	@echo Generating xe_defs.adb
	@if [ "${PSNAME}" = "Windows NT" ]; then \
	   echo "with XE_Reg;" >xe_defs.adb;\
	   echo "package body XE_Defs is" >>xe_defs.adb;\
	   echo "   GARLIC_Dir : aliased constant String" >>xe_defs.adb;\
	   echo "      := \"${GARLIC}\";" >>xe_defs.adb;\
	   for i in GARLIC; do \
		echo "   function Get_"$$i"_Dir return String_Access is"; \
		echo "   begin"; \
		echo "      if GNAT.OS_Lib.Is_Directory ("$$i"_Dir) then"; \
		echo "         return new String'("$$i"_Dir);"; \
		echo "      else"; \
		echo "         return XE_Reg.Get_"$$i"_Dir;"; \
		echo "      end if;"; \
		echo "   end Get_"$$i"_Dir;"; \
	   done >> xe_defs.adb; \
	   echo "   function Get_Rsh_Command return String is" >>xe_defs.adb;\
	   echo "   begin" >>xe_defs.adb;\
	   echo "      return \"${RSH}\";" >>xe_defs.adb;\
	   echo "   end Get_Rsh_Command;" >>xe_defs.adb;\
	   echo "end XE_Defs;" >>xe_defs.adb;\
	else \
	   echo "package body XE_Defs is" >xe_defs.adb;\
	   echo "   GARLIC_Dir : aliased constant String" >>xe_defs.adb;\
	   echo "      := \"${GARLIC}\";" >>xe_defs.adb;\
	   for i in GARLIC; do \
		echo "   function Get_"$$i"_Dir return String_Access is"; \
		echo "   begin"; \
		echo "      return new String'("$$i"_Dir);"; \
		echo "   end Get_"$$i"_Dir;"; \
	   done >> xe_defs.adb; \
	   echo "   function Get_Rsh_Command return String is" >>xe_defs.adb;\
	   echo "   begin" >>xe_defs.adb;\
	   echo "      return \"${RSH}\";" >>xe_defs.adb;\
	   echo "   end Get_Rsh_Command;" >>xe_defs.adb;\
	   echo "end XE_Defs;" >>xe_defs.adb;\
	fi

xe_defs.o: xe_defs.ads xe_defs.adb
	${ADA} -c -g -O2 -gnatp -I${srcdir} -I${GNATSRC} xe_defs.adb

sdefault.adb: ${srcdir}/config.sdefault
	@if [ -f ${GNATSRC}/sdefault.adb ]; then \
	  echo Copying sdefault.adb; \
	  cp ${GNATSRC}/sdefault.adb .; else \
	  echo Generating sdefault.adb; \
	  ${SHELL} ${srcdir}/config.sdefault; fi

sdefault.o: sdefault.adb ${GNATSRC}/sdefault.ads
	${ADA} -c -g -O2 -gnatp -I${GNATSRC} sdefault.adb
