SHELL = /bin/sh
GNATCHOP = gnatchop
GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatpn
GNATMAKE = gnatmake

all: server client

force:

server: force all_types-skel.ads 
	$(GNATMAKE)  server.adb -I../../../../perforce/cos/naming/ `polyorb-config`

client: force all_types-skel.ads
	$(GNATMAKE)  client.adb -I../../../../perforce/cos/naming/ `polyorb-config`

all_types-skel.ads: all_types.idl 
	iac -ada all_types.idl > iac.ada 
	$(GNATCHOP) -w iac.ada

clean: 
	@-rm -f *.o *.ali *ir_info* 
	@-rm -f client server *skel* *helper* *idl_file* 
	@-rm -f all_types.ad* iac.ada  GNAT* *~

