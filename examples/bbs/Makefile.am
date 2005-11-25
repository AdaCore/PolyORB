#EXAMPLE = dsa-dummy
EXAMPLE = bbs

DSA_SRC=$(srcdir)/../../src/dsa

ir_exe=ir_evolutedp ir_serverp
corba_exe=evolutedp_corba
#corba_exe=
dsa_exe=simplep evolutedp serverp
moma_exe=

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir) -I$(DSA_SRC) -O0
simplep_FLAGS=		-Aclient.stubs
evolutedp_FLAGS=	-Aclient.stubs
serverp_FLAGS=		-Aserver.stubs
ir_serverp_FLAGS=	-Aserver.stubs

common.o common.ali: $(srcdir)/common.ads $(srcdir)/common.adb
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/common.adb

server.stubs/server.ali: $(srcdir)/server.ads $(srcdir)/server.adb common.ali
	@-mkdir server.stubs
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/server.adb $(serverp_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/server.o

client.stubs/server.ali: $(srcdir)/server.ads common.ali
	@-mkdir client.stubs
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/server.ads $(simplep_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/server.o

serverp: server.stubs/server.ali
simplep evolutedp: client.stubs/server.ali

clean: clean-stubs clean-am
clean-stubs:
	rm -fr server.stubs client.stubs

#dsa-dummy.ads: ;

include $(srcdir)/../Makefile.common

