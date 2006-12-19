if WINDOWS
WRAPPERS = linker.exe native-linker.exe
else
WRAPPERS =
endif

all-local: $(WRAPPERS)

run_script.exe: force
	$(GNATMAKE) -m -I. -I$(srcdir) $(@:.exe=) -cargs -gnatwae -gnatg

$(WRAPPERS): run_script.exe
	rm -f $@ ; cp run_script.exe $@

CLEANFILES = *.o *.ali b~*.ads b~*.adb

DISTCLEANFILES = $(WRAPPERS)

force::

install-exec-local:
