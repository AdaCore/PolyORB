all:: config-stamp
	cd idlac && $(MAKE) all
	cd adasockets && $(MAKE) all
	cd src && $(MAKE) all

config-stamp:
	cd idlac && ./configure --cache-file=../config.cache
	cd adasockets && ./configure --cache-file=../config.cache
	cd src && ./configure --cache-file=../config.cache
	touch config-stamp

clean::
	rm -f config-stamp
	cd idlac && $(MAKE) clean
	cd adasockets && $(MAKE) clean
	cd src && $(MAKE) clean

distclean::
	rm -f config-stamp
	-cd idlac && $(MAKE) distclean
	-cd adasockets && $(MAKE) distclean
	-cd src && $(MAKE) distclean
	rm -f config.cache
