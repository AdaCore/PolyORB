if ENABLE_CORBA
TESTS=  polyorb-test-no_tasking \
        polyorb-test-thread_pool \
        polyorb-test-no_tasking_poa \
        polyorb-test-thread_pool_poa
endif

LINKER=	../../support/linker

CLEANFILES= *.o *.ali

all: $(TESTS)

all-local: $(TESTS)

$(TESTS): force
	$(GNATMAKE_FOR_TARGET) \
	-I$(srcdir) \
	-I$(srcdir)/../../src \
	-I$(srcdir)/../../src/corba \
	-I$(srcdir)/../../src/giop \
	-I$(srcdir)/../../src/soap \
	-I$(srcdir)/../../src/srp \
	-I../../src \
	-I../../src/corba \
	-I../../src/giop \
	-I../../src/soap \
	-I../../src/srp \
	$(XMLADA_INCS) \
	-m $@ \
	-cargs $(GNATFLAGS) -bargs $(BARGS) \
	-largs $(LDFLAGS) --LINK=$(LINKER) \
	$(POLYORB_LIBS) $(XMLADA_LIBS)

force:
	(cd ../../src && $(MAKE) all)
clean:
