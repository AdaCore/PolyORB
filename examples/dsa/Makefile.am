EXAMPLES_ROOT=..
EXAMPLE = dsa-dummy

DSA_SRC=$(srcdir)/../../src/dsa

corba_exe=
dsa_exe=client server
moma_exe=

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir) -I$(DSA_SRC)
server_FLAGS=	-Aserver.stubs
client_FLAGS=	-Aclient.stubs

s-polint.ali: $(DSA_SRC)/s-polint.ads $(DSA_SRC)/s-polint.adb
	$(GNATMAKE_FOR_TARGET) -a -u $(DSA_SRC)/s-polint.adb $(GNATMAKE_FLAGS)

rt.ali rt.o: rt.ads s-polint.ali
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/rt.ads $(GNATMAKE_FLAGS)

### RCI server stubs

server.stubs/rci.ali: $(srcdir)/rci.ads $(srcdir)/rci.adb rt.ali s-polint.ali
	@-mkdir server.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/rci.adb $(server_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/rci.o

server.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(DSA_SRC)/polyorb-dsa_p-partitions.adb s-polint.ali polyorb-dsa_p.ali
	@-mkdir server.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.adb $(server_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/polyorb-dsa_p-partitions.o

### RCI client stubs

client.stubs/rci.ali: $(srcdir)/rci.ads rt.ali s-polint.ali
	@-mkdir client.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/rci.ads $(client_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/rci.o

client.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(DSA_SRC)/polyorb-dsa_p-partitions.adb s-polint.ali polyorb-dsa_p.ali
	@-mkdir client.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(client_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/polyorb-dsa_p-partitions.o

clean: clean-stubs
clean-stubs:
	rm -fr server.stubs client.stubs

server: server.stubs/rci.ali server.stubs/polyorb-dsa_p-partitions.ali polyorb-dsa_p.ali rt.ali
client: client.stubs/rci.ali client.stubs/polyorb-dsa_p-partitions.ali polyorb-dsa_p.ali rt.ali

dsa-dummy.ads: ;

include $(srcdir)/../Makefile.common

