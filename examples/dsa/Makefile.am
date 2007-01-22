EXAMPLE = dsa-dummy

DSA_SRC=$(srcdir)/../../src/dsa

corba_exe=
dsa_exe=client server
moma_exe=

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir) -I$(top_builddir)/src/dsa -I$(DSA_SRC)
server_FLAGS=	-Aserver.stubs
client_FLAGS=	-Aclient.stubs

polyorb-partition_elaboration.ali polyorb-partition_elaboration.o: polyorb-partition_elaboration.adb $(DSA_SRC)/polyorb-partition_elaboration.ads
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/polyorb-partition_elaboration.adb $(GNATMAKE_FLAGS)

rt.ali rt.o: rt.ads
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/rt.ads $(GNATMAKE_FLAGS)

matrices.ali matrices.o: matrices.adb
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/matrices.adb $(GNATMAKE_FLAGS)

### RCI server stubs

server.stubs/rci.ali: $(srcdir)/rci.ads $(srcdir)/rci.adb rt.ali
	@-mkdir server.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/rci.adb $(server_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/rci.o

server.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(DSA_SRC)/polyorb-dsa_p-partitions.adb
	@-mkdir server.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.adb $(server_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/polyorb-dsa_p-partitions.o

### RCI client stubs

client.stubs/rci.ali: $(srcdir)/rci.ads rt.ali
	@-mkdir client.stubs
	$(GNATMAKE_FOR_TARGET) -u $(srcdir)/rci.ads $(client_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/rci.o

client.stubs/polyorb-dsa_p-partitions.ali: $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(DSA_SRC)/polyorb-dsa_p-partitions.adb
	@-mkdir client.stubs
	$(GNATMAKE_FOR_TARGET) -u $(DSA_SRC)/polyorb-dsa_p-partitions.ads $(client_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/polyorb-dsa_p-partitions.o

clean: clean-stubs clean-am
clean-stubs:
	rm -fr server.stubs client.stubs

server: server.stubs/rci.ali server.stubs/polyorb-dsa_p-partitions.ali rt.ali matrices.ali polyorb-partition_elaboration.ali
client: client.stubs/rci.ali client.stubs/polyorb-dsa_p-partitions.ali rt.ali matrices.ali polyorb-partition_elaboration.ali

dsa-dummy.ads: ;

include $(srcdir)/../Makefile.common

