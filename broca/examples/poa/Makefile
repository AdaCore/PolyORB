GNATMAKE=gnatmake
ADASOCKETS_PATH=$(HOME)/devel/adasockets-0.1.3/src
GNATMAKE_FLAGS=-I$(ADASOCKETS_PATH) -g -gnatf -gnatwu -gnatwl -gnatg

CLIENTS=client
SERVERS=serverp

all: $(SERVERS) $(CLIENTS)

$(SERVERS) $(CLIENTS): force
	$(GNATMAKE) $(GNATMAKE_FLAGS) -I../../src $@

clean: force
	$(RM) -f *.ali *.o *~ $(CLIENTS) $(SERVERS) *.c ior
	$(RM) echo-proxy.* echo-skel.* echo-stream.* echo.ads echo.adb
	$(RM) echo_idl_file.ads

force:
