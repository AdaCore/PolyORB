LINKER=	../support/linker

corba_dir = corba polyorb
dsa_dir   = dsa
moma_dir  = moma
giop_dir  =
soap_dir  = 
srp_dir   =

SUBDIRS= @PROTO_DIRS@ @APPLI_DIRS@

all: orb 

orb:
	(cd $(top_builddir)/src && $(MAKE) all)


