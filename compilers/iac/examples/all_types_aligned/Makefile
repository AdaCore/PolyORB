SHELL = /bin/sh
GNATCHOP = gnatchop
GNATFLAGS =-g -O2 -gnatfy -gnatoa -fstack-check -gnat05
GNATMAKE = gnatmake

all: server client

force:

server: force all_types-skel.ads 
	$(GNATMAKE) $(GNATFLAGS) server.adb `polyorb-config` -bargs -t

client: force all_types-skel.ads
	$(GNATMAKE) $(GNATFLAGS) client.adb `polyorb-config` -bargs -t

all_types-skel.ads: all_types.idl 
	iac -ada -ra all_types.idl

clean: 
	@-rm -f *.o *.ali
	@-rm -f client server *skel* *helper*
	@-rm -f all_types.ad* *~ b~*
	@-rm -f all_types-cdr.ad* all_types-aligned.ads

