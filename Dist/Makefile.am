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
gnatdist_DEPENDENCIES = xe_sysdep.o
gnatdist_SOURCES =
gnatdist_LDADD =

CLEANFILES = b~*.ad[bs] *.ali gnatdist$(EXEEXT) xe_sysdep.adb

force: 

gnatdist$(EXEEXT): $(gnatdist_DEPENDENCIES) force
	$(GNAT_FOR_HOST) -m $(GNATFLAGS) -I. -I$(srcdir) \
	   $(srcdir)/xe_main.adb -o gnatdist$(EXEEXT)

xe_sysdep.adb: xe_sysdep-std.adb xe_sysdep-nt.adb
	@echo Selecting xe_sysdep.adb
	@if [ "$(PSNAME)" = "Windows/NT" ]; then \
	   cp $(srcdir)/xe_sysdep-nt.adb xe_sysdep.adb; \
	else \
	   cp $(srcdir)/xe_sysdep-std.adb xe_sysdep.adb; \
	fi

xe_sysdep.o: xe_sysdep.adb xe_sysdep.ads
	$(GNAT_FOR_HOST) $(COMPILE) $(GNATFLAGS) -I$(srcdir) $<
