##
## Makefile.am,v 1.8 1996/05/11 13:09:51 tardieu Exp
##

AUTOMAKE_OPTIONS = no-dependencies foreign

ADA = @ADA@
GNATMAKE = @GNATMAKE@
GNATSRC = @GNAT_SOURCE@
GARLIC = $(libdir)/garlic
GNATFLAGS = -gnatpg @GNATFLAGS@
RSH_CMD = @RSH_CMD@
RSH_OPT = @RSH_OPT@
srcdir = @srcdir@

bin_PROGRAMS = gnatdist
gnatdist_DEPENDENCIES = xe_defs.o xe_sysdep.o sdefault.o
gnatdist_SOURCES =
gnatdist_LDADD =

CLEANFILES = *.ali gnatdist$(EXEEXT) xe_defs.adb xe_sysdep.adb sdefault.adb

force: 

gnatdist$(EXEEXT): $(gnatdist_DEPENDENCIES) force
	$(GNATMAKE) -m $(GNATFLAGS) -I. -I$(srcdir) -I$(GNATSRC) \
	   $(srcdir)/xe_build.adb -o gnatdist$(EXEEXT)

xe_sysdep.adb: xe_sysdep-std.adb xe_sysdep-nt.adb
	@echo Selecting xe_sysdep.adb
	@if [ "$(PSNAME)" = "Windows NT" ]; then \
		cp $(srcdir)/xe_sysdep-nt.adb xe_sysdep.adb; \
	else \
		cp $(srcdir)/xe_sysdep-std.adb xe_sysdep.adb; \
	fi

xe_defs.adb: Makefile
	@echo Generating xe_defs.adb
	@$(RM) -f xe_defs.adb
	@if [ "$(PSNAME)" = "Windows NT" ]; then \
	   echo "with XE_Reg;" >>xe_defs.adb;\
	fi
	@echo "package body XE_Defs is" >>xe_defs.adb;
	@echo "   GARLIC_Dir : aliased constant String" >>xe_defs.adb;
	@echo "      := \"$(GARLIC)\";" >>xe_defs.adb;
	@for i in GARLIC; do \
	  echo "   function Get_"$$i"_Dir return String_Access is"; \
	  echo "   begin"; \
	  if [ "$(PSNAME)" = "Windows NT" ]; then \
	    echo "      return XE_Reg.Get_"$$i"_Dir;"; \
	  else \
	    echo "      return new String'("$$i"_Dir);"; \
	  fi; \
	  echo "   end Get_"$$i"_Dir;"; \
	done >> xe_defs.adb; 
	@echo "   function Get_Rsh_Command return String is" >>xe_defs.adb;
	@echo "   begin" >>xe_defs.adb;
	@echo "      return \"$(RSH_CMD)\";" >>xe_defs.adb;
	@echo "   end Get_Rsh_Command;" >>xe_defs.adb;
	@echo "" >>xe_defs.adb;
	@echo "   function Get_Rsh_Options return String is" >>xe_defs.adb;
	@echo "   begin" >>xe_defs.adb;
	@echo "      return \"$(RSH_OPT)\";" >>xe_defs.adb;
	@echo "   end Get_Rsh_Options;" >>xe_defs.adb;
	@echo "   function Get_Def_Storage_Data return String is" >>xe_defs.adb;
	@echo "   begin" >>xe_defs.adb;
	@echo "      return \"$(DEFSTORAGEDATA)\";" >>xe_defs.adb;
	@echo "   end Get_Def_Storage_Data;" >>xe_defs.adb;
	@echo "   function Get_Def_Storage_Name return String is" >>xe_defs.adb;
	@echo "   begin" >>xe_defs.adb;
	@echo "      return \"$(DEFSTORAGENAME)\";" >>xe_defs.adb;
	@echo "   end Get_Def_Storage_Name;" >>xe_defs.adb;
	@echo "end XE_Defs;" >>xe_defs.adb;

xe_defs.o: xe_defs.ads xe_defs.adb
	$(ADA) -c -g -O2 -gnatp -I$(srcdir) -I$(GNATSRC) xe_defs.adb

xe_sysdep.o: xe_sysdep.ads xe_sysdep.adb
	$(ADA) -c -g -O2 -gnatp -I$(srcdir) -I$(GNATSRC) xe_sysdep.adb

sdefault.adb: $(srcdir)/config.sdefault
	@if [ -f $(GNATSRC)/sdefault.adb ]; then \
	  echo Copying sdefault.adb; \
	  cp $(GNATSRC)/sdefault.adb .; else \
	  echo Generating sdefault.adb; \
	  $(SHELL) $(srcdir)/config.sdefault; fi

sdefault.o: sdefault.adb $(GNATSRC)/sdefault.ads
	$(ADA) -c -g -O2 -gnatp -I$(GNATSRC) sdefault.adb
