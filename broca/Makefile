all::
	cd adabroker && \
		$(MAKE) all
	cd adasockets && \
		./configure --cache-file=../config.cache && \
		$(MAKE) all
	cd src && \
		./configure --cache-file=../config.cache && \
		$(MAKE) all

clean::
	cd adabroker && $(MAKE) clean
	cd adasockets && $(MAKE) clean
	cd src && $(MAKE) clean

distclean::
	cd adabroker && $(MAKE) clean
	cd adasockets && $(MAKE) distclean
	cd src && $(MAKE) distclean
