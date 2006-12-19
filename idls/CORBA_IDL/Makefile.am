IDLdir = $(includedir)/polyorb/corba/idls

IDL =	$(srcdir)/orb.idl \
	$(srcdir)/CORBA_Current.idl \
	$(srcdir)/CORBA_DomainManager.idl \
	$(srcdir)/CORBA_InterfaceRepository.idl \
	$(srcdir)/CORBA_Policy.idl \
	$(srcdir)/CORBA_StandardExceptions.idl \
	$(srcdir)/CORBA_Stream.idl \
	$(srcdir)/CORBA_TypeCode.idl

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(IDLdir)
	for f in $(IDL); do \
	  if test -f $$f; then \
	    $(INSTALL) -m 444 $$f $(DESTDIR)$(IDLdir); \
	  else \
	    $(INSTALL) -m 444  $(srcdir)/$$f $(DESTDIR)$(IDLdir); \
	  fi \
	done
