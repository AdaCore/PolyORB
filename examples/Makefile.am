TESTS=  polyorb-test-no_tasking \
        polyorb-test-thread_pool \
        polyorb-test-no_tasking_poa \
        polyorb-test-thread_pool_poa \
        dynclient \
        server

all: $(TESTS)

$(TESTS):
	(cd ../src && $(MAKE) all)
	$(GNATMAKE) -c \
	-I../src -I../src/corba -I../src/soap $(XMLADA_INCLUDE) echo-impl.adb \
	-cargs -g -O2 -Wall -gnatfa -gnatwu -gnatwe -gnatwl -bargs $(BARGS) \
	-largs $(XMLADA_LIBS)
	$(GNATMAKE) -I../src -I../src/corba -I../src/soap $(XMLADA_INCLUDE) $@ \
	-cargs $(GNATFLAGS) \
	-largs $(LDFLAGS) \
	-L../src/.libs -lpolyorb \
	-L../src/corba/.libs -lpolyorb-corba \
	-L../src/soap/.libs -lpolyorb-soap \
	-bargs $(BARGS) -largs $(XMLADA_LIBS)
