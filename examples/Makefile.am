SUBDIRS=	echo

TESTS=  polyorb-test-no_tasking \
        polyorb-test-thread_pool \
        polyorb-test-no_tasking_poa \
        polyorb-test-thread_pool_poa

all: $(TESTS)

$(TESTS): force
	$(GNATMAKE) -I../src -I../src/corba -I../src/soap $(XMLADA_INCLUDE) $@ \
	-cargs $(GNATFLAGS) \
	-largs $(LDFLAGS) \
	-L../src/.libs -lpolyorb \
	-L../src/corba/.libs -lpolyorb-corba \
	-L../src/soap/.libs -lpolyorb-soap \
	-bargs $(BARGS) -largs $(XMLADA_LIBS)

force:
	(cd ../src && $(MAKE) all)

clean:
