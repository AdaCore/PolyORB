SHELL = /bin/sh
GNATCHOP = gnatchop
GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatpn  -gnatwl
GNATMAKE = gnatmake

all: servers client

force:

servers: force harness-skel.ads 
	$(GNATMAKE)  server*.adb -I../../../../perforce/cos/naming/ -I../../../common_files/ `polyorb-config`

client: force harness-skel.ads
	$(GNATMAKE)  client.adb -I../../../../perforce/cos/naming/ `polyorb-config`

harness-skel.ads: harness.idl 
	iac -ada harness.idl > iac.ada 
	$(GNATCHOP) -w iac.ada

clean: 
	@-rm -f *.o *.ali *ir_info* 
	@-rm -f client server_no_tasking *idl_file* 
	@-rm -f iac.ada  GNAT* *~
	@-rm -f server*_tasking2 s*request s*pool s*session s*lf s*hahs

