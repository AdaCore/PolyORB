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
	$(GNATMAKE_FOR_TARGET) -c $(DSA_SRC)/s-polint.adb $(GNATMAKE_FLAGS)
polyorb-dsa_p.ali: $(DSA_SRC)/polyorb-dsa_p.ads
	$(GNATMAKE_FOR_TARGET) -c $(DSA_SRC)/polyorb-dsa_p.ads $(GNATMAKE_FLAGS)
# -gnatg cannot be used as the moment, because s-polint has a dep
# on generated CosNaming code, which does not support it.

rt.ali rt.o: rt.ads s-polint.ali
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/rt.ads $(GNATMAKE_FLAGS)

### RCI server stubs

server.stubs/rci.ali: $(srcdir)/rci.ads $(srcdir)/rci.adb rt.ali s-polint.ali
	@-mkdir server.stubs
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/rci.adb $(server_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/rci.o

server.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(DSA_SRC)/polyorb-dsa_p-partitions.adb s-polint.ali
	@-mkdir server.stubs
	$(GNATMAKE_FOR_TARGET) -c $(DSA_SRC)/polyorb-dsa_p-partitions.adb $(server_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/polyorb-dsa_p-partitions.o

### RCI client stubs

client.stubs/rci.ali: $(srcdir)/rci.ads rt.ali s-polint.ali
	@-mkdir client.stubs
	$(GNATMAKE_FOR_TARGET) -c $(srcdir)/rci.ads $(client_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/rci.o

client.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(DSA_SRC)/polyorb-dsa_p-partitions.adb s-polint.ali
	@-mkdir client.stubs
	$(GNATMAKE_FOR_TARGET) -c $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(client_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/polyorb-dsa_p-partitions.o

clean: clean-stubs
clean-stubs:
	rm -fr server.stubs client.stubs

server: server.stubs/rci.ali server.stubs/polyorb-dsa_p-partitions.ali polyorb-dsa_p.ali rt.ali
client: client.stubs/rci.ali client.stubs/polyorb-dsa_p-partitions.ali polyorb-dsa_p.ali rt.ali

dsa-dummy.ads: ;

include $(srcdir)/../Makefile.common

