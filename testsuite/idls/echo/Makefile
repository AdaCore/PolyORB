SHELL = /bin/sh
GNATCHOP = gnatchop
GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatpn
GNATMAKE = gnatmake

all: server client

force:

server: force echo-skel.ads 
	$(GNATMAKE)  server.adb `polyorb-config`

client: force echo-skel.ads
	$(GNATMAKE)  client.adb `polyorb-config`

echo-skel.ads: echo.idl 
	iac -ada echo.idl > iac.ada 
	$(GNATCHOP) -w iac.ada

clean: 
	@-rm -f *.o *.ali *ir_info* 
	@-rm -f client server *skel* *helper* *idl_file* 
	@-rm -f echo.ad* iac.ada  GNAT* *~

