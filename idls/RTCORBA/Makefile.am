IDLdir = $(includedir)/polyorb/corba/idls

IDL =	$(srcdir)/RTCORBA.idl \
	$(srcdir)/RTCosScheduling.idl \
	$(srcdir)/RTPortableServer.idl

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(IDLdir)
	for f in $(IDL); do \
	  if test -f $$f; then \
	    $(INSTALL) -m 444 $$f $(DESTDIR)$(IDLdir); \
	  else \
	    $(INSTALL) -m 444  $(srcdir)/$$f $(DESTDIR)$(IDLdir); \
	  fi \
	done
