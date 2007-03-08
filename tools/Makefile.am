corba_dir = 
dsa_dir   = 
moma_dir  = 
aws_dir   = 
giop_dir  =
soap_dir  = 
srp_dir   =

ir_dir = po_dumpir po_ir
naming_dir = po_cos_naming

SUBDIRS = po_catref po_createref po_names @SERVICE_DIRS@

all: orb 

orb:
	(cd $(top_builddir)/src && $(MAKE) all)
