all:: idlac/configure config-stamp
	cd idlac && $(MAKE) all
	cd adasockets && $(MAKE) all
	cd src && $(MAKE) all

autoconf::
	cd idlac && (aclocal -I ../support && autoconf && automake)
	cd idlac/testsuite && (aclocal -I ../../support && autoconf && automake)
	cd adasockets && (aclocal -I ../support && autoconf && automake)
	cd src && (aclocal -I ../support && autoconf && automake)

idlac/configure:
	$(MAKE) autoconf

config-stamp:
	cd idlac && ./configure --cache-file=../config.cache
	cd idlac/testsuite && ./configure --cache-file=../../config.cache
	cd adasockets && ./configure --cache-file=../config.cache
	cd src && ./configure --cache-file=../config.cache
	touch config-stamp

clean::
	rm -f config-stamp
	cd idlac && $(MAKE) clean
	cd idlac/testsuite && $(MAKE) clean
	cd adasockets && $(MAKE) clean
	cd src && $(MAKE) clean

distclean::
	rm -f config-stamp
	-cd idlac && $(MAKE) distclean
	-cd idlac/testsuite && $(MAKE) distclean
	-cd adasockets && $(MAKE) distclean
	-cd src && $(MAKE) distclean
	rm -f config.cache
