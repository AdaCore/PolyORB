EXAMPLES_ROOT=..
EXAMPLE = dsa-dummy

DSA_SRC=$(srcdir)/../../src/dsa

#corba_exe=evolutedp_corba
corba_exe=
dsa_exe=simplep evolutedp serverp
moma_exe=

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir) -I$(DSA_SRC) -gnatD -O0
simplep_FLAGS=		-Aclient.stubs
evolutedp_FLAGS=	-Aclient.stubs
serverp_FLAGS=		-Aserver.stubs

s-polint.ali: $(DSA_SRC)/s-polint.ads $(DSA_SRC)/s-polint.adb
	$(GNATMAKE_FOR_TARGET) -a -c $(DSA_SRC)/s-polint.adb $(GNATMAKE_FLAGS)
# -gnatg cannot be used as the moment, because s-polint has a dep
# on generated CosNaming code, which does not support it.

common.o common.ali: common.ads common.adb
	$(GNATMAKE_FOR_TARGET) -c common.adb

server.stubs/server.ali: $(srcdir)/server.ads $(srcdir)/server.adb common.ali s-polint.ali
	@-mkdir server.stubs
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/server.adb $(serverp_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/server.o

client.stubs/server.ali: $(srcdir)/server.ads common.ali s-polint.ali
	@-mkdir client.stubs
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/server.ads $(simplep_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/server.o

serverp: server.stubs/server.ali
simplep evolutedp: client.stubs/server.ali

clean: clean-stubs
clean-stubs:
	rm -fr server.stubs client.stubs

dsa-dummy.ads: ;

include $(srcdir)/../Makefile.common

