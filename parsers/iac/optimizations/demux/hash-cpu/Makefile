SHELL = /bin/sh
GNATCHOP = gnatchop
GNATFLAGS = -g -O2 
GNATMAKE = gnatmake

all: servers client 

force: 
servers: force harness-skel.ads harness_operation_hash.ads 
	$(GNATMAKE)  $(GNATFLAGS) server*.adb -I../../../../perforce/cos/naming/ -I../../../common_files/ `polyorb-config`

client: force harness-skel.ads harness_operation_hash.ads
	$(GNATMAKE)  $(GNATFLAGS) client.adb -I../../../../perforce/cos/naming/ `polyorb-config`

harness-skel.ads: harness.idl 
	iac -ada harness.idl > iac.ada 
	$(GNATCHOP) -w iac.ada

harness_operation_hash.ads: 
	$(GNATMAKE) $(GNATFLAGS) harness_gen_table.adb 
	./harness_gen_table
clean: 
	@-rm -f *.o *.ali *ir_info* b~*
	@-rm -f client server_no_tasking *idl_file* 
	@-rm -f iac.ada  GNAT* *~ *operation* harness_gen_table 
	@-rm -f server*_tasking2 s*request s*pool s*session s*lf s*hahs

