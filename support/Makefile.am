if WINDOWS
WRAPPERS = run_script.exe linker.exe
else
WRAPPERS =
endif

all-local: $(WRAPPERS)

run_script.exe: force
	$(GNATMAKE) -m -I. -I$(srcdir) $(@:.exe=) -cargs -gnatwae -gnatg

linker.exe: run_script.exe
	rm -f linker.exe ; cp run_script.exe linker.exe

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(WRAPPERS)

force::

install-exec-local:
