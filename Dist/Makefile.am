AUTOMAKE_OPTIONS = no-dependencies foreign

GNAT_FOR_HOST = @GNAT_FOR_HOST@
COMPILE       = -f -u -a
GNAT_RTS_FLAG = @GNAT_RTS_FLAG@
GNATDISTFLAGS = $(GNAT_RTS_FLAG) -@GNATDISTOPT@ @PCSFLAGS@
GNATFLAGS     = -gnatpg -gnatyo @GNATFLAGS@ $(GNAT_RTS_FLAG)
PCSNAME       = @PCSNAME@
RSH_CMD       = @RSH_CMD@
RSH_OPT       = @RSH_OPT@
srcdir        = @srcdir@

bin_PROGRAMS = gnatdist
gnatdist_DEPENDENCIES = xe_defs.o xe_sysdep.o xe_back.o
gnatdist_SOURCES =
gnatdist_LDADD =

CLEANFILES = b~*.ad[bs] *.ali gnatdist$(EXEEXT) b_xe_defs$(EXEEXT) \
             xe_defs.adb xe_sysdep.adb xe_back.adb

force: 

gnatdist$(EXEEXT): $(gnatdist_DEPENDENCIES) force
	$(GNAT_FOR_HOST) -m $(GNATFLAGS) -I. -I$(srcdir) \
	   $(srcdir)/xe_main.adb -o gnatdist$(EXEEXT)

b_xe_defs$(EXEEXT): $(srcdir)/b_xe_defs.adb
	@$(GNAT_FOR_HOST) $(GNATFLAGS) $(srcdir)/b_xe_defs.adb \
	  -o b_xe_defs$(EXEEXT)

xe_sysdep.adb: xe_sysdep-std.adb xe_sysdep-nt.adb
	@echo Selecting xe_sysdep.adb
	@if [ "$(PSNAME)" = "Windows/NT" ]; then \
	   cp $(srcdir)/xe_sysdep-nt.adb xe_sysdep.adb; \
	else \
	   cp $(srcdir)/xe_sysdep-std.adb xe_sysdep.adb; \
	fi

xe_defs.adb: Makefile b_xe_defs$(EXEEXT)
	@echo Generating xe_defs.adb
	@./b_xe_defs$(EXEEXT) $(GNATDISTFLAGS) \
	   RSH_CMD="$(RSH_CMD)" \
	   RSH_OPT="$(RSH_OPT)" \
	   PCSNAME="$(PCSNAME)" \
	   DEFSTORAGEDATA="$(DEFSTORAGEDATA)" \
	   DEFSTORAGENAME="$(DEFSTORAGENAME)" \
	   DEFPROTOCOLDATA="$(DEFPROTOCOLDATA)" \
	   DEFPROTOCOLNAME="$(DEFPROTOCOLNAME)" \
	   PSNAME="$(PSNAME)" >xe_defs.adb

xe_sysdep.o: xe_sysdep.adb xe_sysdep.ads
	$(GNAT_FOR_HOST) $(COMPILE) $(GNATFLAGS) -I$(srcdir) $<

xe_defs.o: xe_defs.adb xe_defs.ads
	$(GNAT_FOR_HOST) $(COMPILE) $(GNATFLAGS) -I$(srcdir) $<

xe_back.o: xe_back.adb xe_back.ads
	$(GNAT_FOR_HOST) $(COMPILE) $(GNATFLAGS) -I$(srcdir) $<

xe_back.adb: Makefile
	cp xe_back-$(PCSNAME).adb xe_back.adb