IDLdir = $(includedir)/polyorb/corba/idls

IDL =	$(srcdir)/DynamicAny.idl \
	$(srcdir)/Dynamic.idl \
	$(srcdir)/FT.idl \
	$(srcdir)/MessageRouting.idl \
	$(srcdir)/Messaging.idl \
	$(srcdir)/MGM.idl \
	$(srcdir)/MIOP.idl \
	$(srcdir)/PortableGroup.idl \
	$(srcdir)/PortableInterceptor.idl \
	$(srcdir)/PortableServer.idl

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(IDLdir)
	for f in $(IDL); do \
	  if test -f $$f; then \
	    $(INSTALL) -m 444 $$f $(DESTDIR)$(IDLdir); \
	  else \
	    $(INSTALL) -m 444  $(srcdir)/$$f $(DESTDIR)$(IDLdir); \
	  fi \
	done
