AUTOMAKE_OPTIONS = no-dependencies foreign

GNAT_FOR_HOST = @GNAT_FOR_HOST@
COMPILE       = -f -u -a
GNAT_RTS_FLAG = @GNAT_RTS_FLAG@
GNATDISTFLAGS = $(GNAT_RTS_FLAG) -@GNATDISTOPT@ @PCSFLAGS@
GNATFLAGS     = -gnatpg @GNATFLAGS@ $(GNAT_RTS_FLAG)
PCSNAME       = @PCSNAME@
RSH_CMD       = @RSH_CMD@
RSH_OPT       = @RSH_OPT@
srcdir        = @srcdir@

bin_PROGRAMS = gnatdist
gnatdist_DEPENDENCIES =
gnatdist_SOURCES =
gnatdist_LDADD =

CLEANFILES = b~*.ad[bs] *.ali gnatdist$(EXEEXT)

force: 

gnatdist$(EXEEXT): $(gnatdist_DEPENDENCIES) force
	$(GNAT_FOR_HOST) -m $(GNATFLAGS) -I. -I$(srcdir) \
	   $(srcdir)/xe_main.adb -o gnatdist$(EXEEXT)
