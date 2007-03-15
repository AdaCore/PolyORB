IDLdir = $(includedir)/polyorb/corba/idls

IDL =	$(srcdir)/CORBA_Context.idl \
	$(srcdir)/CORBA_NVList.idl \
	$(srcdir)/CORBA_Object.idl \
	$(srcdir)/CORBA_ORB.idl \
	$(srcdir)/CORBA_ORB_init.idl \
	$(srcdir)/CORBA_Request.idl \
	$(srcdir)/CORBA_ServerRequest.idl \
	$(srcdir)/CORBA_ValueBase.idl \
	$(srcdir)/pseudo_orb.idl

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(IDLdir)
	for f in $(IDL); do \
	  if test -f $$f; then \
	    $(INSTALL) -m 444 $$f $(DESTDIR)$(IDLdir); \
	  else \
	    $(INSTALL) -m 444  $(srcdir)/$$f $(DESTDIR)$(IDLdir); \
	  fi \
	done
