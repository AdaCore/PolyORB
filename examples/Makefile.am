corba_dir = corba polyorb
dsa_dir   = dsa
moma_dir  = moma
giop_dir  =
soap_dir  = 
srp_dir   =

SUBDIRS= @PROTO_DIRS@ @APPLI_DIRS@

corba_lib="../src/corba/libpolyorb-corba.la"
giop_lib="../src/giop/libpolyorb-giop.la"
moma_lib="../src/moma/libpolyorb-moma.la"
soap_lib="../src/soap/libpolyorb-soap.la"
srp_lib="../src/srp/libpolyorb-srp.la"

POLYORB_LIBS = ../src/libpolyorb.la @PROTO_LIBS@ @APPLI_LIBS@

COS=cos

LINKER=	../support/linker

all: orb 

orb:	force
	(cd $(top_builddir)/src && $(MAKE) all)

force:
