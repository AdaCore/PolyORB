if ENABLE_CORBA
CORBA_DIR = echo all_types all_functions
CORBA_LIB="../src/corba/libpolyorb-corba.la"

TESTS=  polyorb-test-no_tasking \
        polyorb-test-thread_pool \
        polyorb-test-no_tasking_poa \
        polyorb-test-thread_pool_poa
COS=cos
endif

if ENABLE_GIOP
GIOP_LIB=
endif

if ENABLE_MOMA
MOMA_DIR = moma
MOMA_LIB="../src/moma/libpolyorb-moma.la"
endif

if ENABLE_DSA
DSA_DIR = dsa
endif

if ENABLE_SOAP
SOAP_LIB="../src/soap/libpolyorb-soap.la"
endif

if ENABLE_SOAP
SRP_LIB="../src/srp/libpolyorb-srp.la"
endif

SUBDIRS = . $(CORBA_DIR) $(DSA_DIR) $(MOMA_DIR)

POLYORB_LIBS = \
../src/libpolyorb.la \
$(CORBA_LIB) \
$(GIOP_LIB) \
$(MOMA_LIB) \
$(SOAP_LIB) \
$(SRP_LIB)

LINKER=	../support/linker

CLEANFILES= *.o *.ali

all: orb $(TESTS)

all-local: $(TESTS)

$(TESTS): force
	$(GNATMAKE_FOR_TARGET) \
	-I$(srcdir) \
	-I$(srcdir)/../src \
	-I$(srcdir)/../src/corba \
	-I$(srcdir)/../src/soap \
	-I$(srcdir)/../src/srp \
	-I../src \
	-I../src/corba \
	-I../src/soap \
	-I../src/srp \
	$(XMLADA_INCLUDE) \
	-m $@ \
	-cargs $(GNATFLAGS) -bargs $(BARGS) \
	-largs $(LDFLAGS) --LINK=$(LINKER) \
	$(POLYORB_LIBS) $(XMLADA_LIBS)

orb:	force
	(cd ../src && $(MAKE) all)

force:

clean:
