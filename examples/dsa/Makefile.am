EXAMPLES_ROOT=..
EXAMPLE = dsa-dummy

DSA_SRC=$(srcdir)/../../src/dsa

corba_exe=
dsa_exe=client server
moma_exe=

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir) -I$(DSA_SRC) -gnatD
server_FLAGS=	-Aserver.stubs
client_FLAGS=	-Aclient.stubs

s-polint.ali: $(DSA_SRC)/s-polint.ads $(DSA_SRC)/s-polint.adb
	$(GNATMAKE_FOR_TARGET) -a -c $(DSA_SRC)/s-polint.adb $(GNATMAKE_FLAGS)
# -gnatg cannot be used as the moment, because s-polint has a dep
# on generated CosNaming code, which does not support it.

rt.ali rt.o: rt.ads s-polint.ali
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/rt.ads $(GNATMAKE_FLAGS)

server.stubs/rci.ali: $(srcdir)/rci.ads $(srcdir)/rci.adb rt.ali s-polint.ali
	@-mkdir server.stubs
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/rci.adb $(server_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/rci.o

client.stubs/rci.ali: $(srcdir)/rci.ads rt.ali s-polint.ali
	@-mkdir client.stubs
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/rci.ads $(client_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/rci.o

clean: clean-stubs
clean-stubs:
	rm -fr server.stubs client.stubs

server: server.stubs/rci.ali
client: client.stubs/rci.ali

dsa-dummy.ads: ;

include $(srcdir)/../Makefile.common

