AUTOMAKE_OPTIONS = no-dependencies foreign

GNAT_FOR_HOST = @GNAT_FOR_HOST@
GNATSRC       = @GNAT_SOURCE@
GARLIC        = $(libdir)/garlic
COMPILE       = -f -u -a
GNAT_RTS_FLAG = @GNAT_RTS_FLAG@
GNATDISTFLAGS = \
	  -cargs $(GNAT_RTS_FLAG) -@GNATDISTOPT@ -I. -I$(prefix)/lib/garlic \
	  -bargs $(GNAT_RTS_FLAG) -I. -I$(prefix)/lib/garlic \
          -largs -lgarlic
GNATFLAGS     = -gnatpg @GNATFLAGS@ $(GNAT_RTS_FLAG)
PCS_NAME      = System.Garlic
RSH_CMD       = @RSH_CMD@
RSH_OPT       = @RSH_OPT@
srcdir        = @srcdir@

bin_PROGRAMS = gnatdist
gnatdist_DEPENDENCIES = xe_defs.o xe_sysdep.adb sdefault.adb link.o
gnatdist_SOURCES =
gnatdist_LDADD =

CLEANFILES = b~*.ad[bs] *.ali gnatdist$(EXEEXT) b_xe_defs$(EXEEXT) \
             xe_defs.adb xe_sysdep.adb sdefault.adb

force: 

link.o: $(GNATSRC)/link.c
	$(CC) -c -I$(srcdir) -I$(GNATSRC) \
	  $<

gnatdist$(EXEEXT): $(gnatdist_DEPENDENCIES) force
	$(GNAT_FOR_HOST) -m $(GNATFLAGS) -I. -I$(srcdir) -I$(GNATSRC) \
	   $(srcdir)/xe_build.adb -o gnatdist$(EXEEXT) \
	   -bargs -static -largs -lgccprefix $(LIBS)

xe_sysdep.adb: xe_sysdep-std.adb xe_sysdep-nt.adb
	@echo Selecting xe_sysdep.adb
	@if [ "$(PSNAME)" = "Windows/NT" ]; then \
		cp $(srcdir)/xe_sysdep-nt.adb xe_sysdep.adb; \
	else \
		cp $(srcdir)/xe_sysdep-std.adb xe_sysdep.adb; \
	fi

b_xe_defs$(EXEEXT): $(srcdir)/b_xe_defs.adb
	@$(GNAT_FOR_HOST) $(GNATFLAGS) $(srcdir)/b_xe_defs.adb \
	  -o b_xe_defs$(EXEEXT) -I$(GNATSRC)

xe_defs.adb: Makefile b_xe_defs$(EXEEXT)
	@echo Generating xe_defs.adb
	@./b_xe_defs$(EXEEXT) $(GNATDISTFLAGS) \
	   RSH_CMD="$(RSH_CMD)" \
	   RSH_OPT="$(RSH_OPT)" \
	   PCS_NAME="$(PCS_NAME)" \
	   DEFSTORAGEDATA="$(DEFSTORAGEDATA)" \
	   DEFSTORAGENAME="$(DEFSTORAGENAME)" \
	   DEFPROTOCOLDATA="$(DEFPROTOCOLDATA)" \
	   DEFPROTOCOLNAME="$(DEFPROTOCOLNAME)" \
	   PSNAME="$(PSNAME)" >xe_defs.adb

xe_defs.o: xe_defs.adb xe_defs.ads
	$(GNAT_FOR_HOST) $(COMPILE) $(GNATFLAGS) -I$(srcdir) -I$(GNATSRC) \
	   $<

xe_sysdep.o: xe_sysdep.adb xe_sysdep.ads
	$(GNAT_FOR_HOST) $(COMPILE) -$(GNATFLAGS) -I$(srcdir) -I$(GNATSRC) \
	   $<

sdefault.adb: $(srcdir)/config.sdefault
	@if [ -f $(GNATSRC)/sdefault.adb ]; then \
	  echo Copying sdefault.adb; \
	  cp $(GNATSRC)/sdefault.adb .; else \
	  echo Generating sdefault.adb; \
	  $(SHELL) $(srcdir)/config.sdefault; \
	fi
