LINKER=	../support/linker

corba_dir = corba
dsa_dir   = dsa bbs
moma_dir  = moma
giop_dir  =
soap_dir  = 
srp_dir   =

SUBDIRS=  polyorb @PROTO_DIRS@ @APPLI_DIRS@

all: orb 

orb:
	(cd $(top_builddir)/src && $(MAKE) all)


