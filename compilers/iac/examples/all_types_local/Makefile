SHELL = /bin/sh
GNATCHOP = gnatchop
GNATFLAGS =-g -O2 -gnatfy -gnatwae -gnatoa -fstack-check
GNATMAKE = gnatmake
IAC = iac -ada

all: client

force:

client: force all_types-helper.ads
	$(GNATMAKE) $(GNATFLAGS) client.adb `polyorb-config --with-corba-services=naming`

all_types-helper.ads: all_types_local.idl 
	$(IAC) all_types_local.idl

clean: 
	@-rm -f *.o *.ali *ir_info* 
	@-rm -f client *skel* *helper* *idl_file* 
	@-rm -f all_types.ad* GNAT* *~ b~* all_types_hash.ad?

