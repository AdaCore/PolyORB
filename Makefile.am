AUTOMAKE_OPTIONS = foreign no-dependencies
SUBDIRS = @GLADEDIRS@
ACLOCAL_AMFLAGS = -I support

make-check-stamp:
	@$(MAKE) -v -f /dev/null 2>&1 | grep GNU > /dev/null 2>&1 || \
	(echo; echo; \
	 echo WARNING: you are not using GNU make. If you encounter problems; \
	 echo while building GLADE, please install and use GNU make,; \
	 echo available freely from any GNU mirror.; \
	 echo; echo) >&2
	@touch make-check-stamp

Makefile: make-check-stamp

### Build Dist/gnatdist from the main GLADE Makefile
### This allows the Dist/gnatdist source directory to be shared between
### GLADE and PolyORB.

GNAT_FOR_HOST = @GNAT_FOR_HOST@
GNAT_RTS_FLAG = @GNAT_RTS_FLAG@
GNATFLAGS     = -gnatpg @GNATFLAGS@ $(GNAT_RTS_FLAG)
srcdir        = @srcdir@

bin_PROGRAMS = Dist/gnatdist

force: 

Dist/gnatdist$(EXEEXT): force
	$(GNAT_FOR_HOST) $(GNATFLAGS) -D Dist -IDist -I$(srcdir)/Dist \
	   $(srcdir)/Dist/xe_main.adb -o Dist/gnatdist$(EXEEXT)

CLEANFILES = Dist/*.ali Dist/*.o Dist/gnatdist$(EXEEXT) make-check-stamp
