IDLdir = $(includedir)/polyorb/corba/idls

IDL =	$(srcdir)/DynamicAny.idl \
	$(srcdir)/Dynamic.idl \
	$(srcdir)/FT.idl \
	$(srcdir)/MessageRouting.idl \
	$(srcdir)/Messaging.idl \
	$(srcdir)/PortableInterceptor.idl \
	$(srcdir)/PortableServer.idl

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(IDLdir)
	for f in $(IDL); do \
	  if test -f $$f; then \
	    $(INSTALL) -m 644 $$f $(DESTDIR)$(IDLdir); \
	  else \
	    $(INSTALL) -m 644  $(srcdir)/$$f $(DESTDIR)$(IDLdir); \
	  fi \
	done
