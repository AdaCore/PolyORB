SUBDIRS=	echo all_types all_functions moma

LINKER=	../support/linker

TESTS=  polyorb-test-no_tasking \
        polyorb-test-thread_pool \
        polyorb-test-no_tasking_poa \
        polyorb-test-thread_pool_poa

CLEANFILES=	*.o *.ali

all: orb $(TESTS)

all-local: $(TESTS)

$(TESTS): force
	$(GNATMAKE) \
	-I$(srcdir) \
	-I$(srcdir)/../src \
	-I$(srcdir)/../src/corba \
	-I$(srcdir)/../src/soap \
	-I../src \
	-I../src/corba \
	-I../src/soap \
	$(XMLADA_INCLUDE) \
	-m $@ \
	-cargs $(GNATFLAGS) -bargs $(BARGS) \
	-largs $(LDFLAGS) --LINK=$(LINKER) \
	../src/libpolyorb.la \
	../src/corba/libpolyorb-corba.la \
	../src/soap/libpolyorb-soap.la \
	$(XMLADA_LIBS)

orb:	force
	(cd ../src && $(MAKE) all)

force:

clean:
