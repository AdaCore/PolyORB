EXAMPLES_ROOT=..
EXAMPLE = dsa-dummy

EXAMPLE_GNATMAKE_FLAGS=-I$(srcdir)
server_FLAGS=	-Aserver.stubs
client_FLAGS=	-Aclient.stubs

server.stubs/rci.ali: $(srcdir)/rci.ads $(srcdir)/rci.adb
	@-mkdir server.stubs
	$(GNATMAKE) -c $(srcdir)/rci.adb $(server_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzr -o server.stubs/rci.o

client.stubs/rci.ali: $(srcdir)/rci.ads
	@-mkdir client.stubs
	$(GNATMAKE) -c $(srcdir)/rci.ads $(client_FLAGS) $(GNATMAKE_FLAGS) -cargs -gnatzc -o client.stubs/rci.o

clean: clean-stubs
clean-stubs:
	rm -fr server.stubs client.stubs

server: server.stubs/rci.ali
client: client.stubs/rci.ali

dsa-dummy.ads: ;

include $(srcdir)/../Makefile.common

