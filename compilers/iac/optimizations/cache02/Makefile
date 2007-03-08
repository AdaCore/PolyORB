SHELL = /bin/sh
GNATCHOP = gnatchop
GNATFLAGS = -g -O2 
GNATMAKE = gnatmake

all: servers client

force:

servers: force harness-skel.ads 
	$(GNATMAKE)  server*.adb $(GNATFLAGS) -I../../../../perforce/cos/naming/ -I../../../common_files/ `polyorb-config` -margs -largs -g

client: force harness-skel.ads
	$(GNATMAKE)  client.adb $(GNATFLAGS) -I../../../../perforce/cos/naming/ -I../../../common_files/ 	`polyorb-config`

harness-skel.ads: harness.idl 
	iac -ada harness.idl > iac.ada 
#	$(GNATCHOP) -w iac.ada

clean: 
	@-rm -f *.o *.ali *ir_info* 
	@-rm -f server*_tasking2 s*request s*pool s*session s*lf s*hahs client s*tasking

