SHELL = /bin/sh

GNATFLAGS=-g -O2 -gnatfy -gnatwae -gnatoa -fstack-check
GNATMAKE=gnatmake
IAC=iac -ada

all : client server

client : client.adb cyclic_m-cyclic_i.ads cyclic_m-test_cyclic.ads
	$(GNATMAKE) $(GNATFLAGS) client.adb `polyorb-config`

server : server.adb cyclic_m-cyclic_i-impl.adb cyclic_m-cyclic_i.ads cyclic_m-test_cyclic-impl.adb cyclic_m-test_cyclic.ads
	$(GNATMAKE) $(GNATFLAGS) server.adb `polyorb-config`

cyclic_m-cyclic_i-impl.adb cyclic_m-cyclic_i.ads cyclic_m-test_cyclic-impl.adb cyclic_m-test_cyclic.ads : cyclic.idl
	$(IAC) cyclic.idl

clean :
	@-rm -f *.ref
	@-rm -f *skel* *ir_info* *helper*
	@-rm -f cyclic_m.ad? cyclic_m-cyclic_i.ad? cyclic_m-test_cyclic.ad? *_idl_file.ads
	@-rm -f *.ali *.o
	@-rm -f GNAT* *~
	@-rm -f core*
	@-rm -f client server
	@-rm -f b~*

