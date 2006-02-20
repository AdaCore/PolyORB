SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/setup

ADA_SPECS_WITH_BODY = \
	polyorb-setup-client.ads \
	polyorb-setup-no_tasking_server.ads \
	polyorb-setup-ravenscar_tp_server.ads \
	polyorb-setup-server.ads \
	polyorb-setup-thread_per_request_server.ads \
	polyorb-setup-thread_per_session_server.ads \
	polyorb-setup-thread_pool_server.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY)

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)

ADA_LIBADD = $(ADA_SPECS:.ads=.lo)

lib_LTLIBRARIES = libpolyorb-setup.la
libpolyorb_setup_la_SOURCES =
libpolyorb_setup_la_LIBADD = $(ADA_LIBADD)
libpolyorb_setup_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

corba_inc=-I$(top_srcdir)/src/corba -I$(top_builddir)/src/corba -I$(top_srcdir)/src/corba/rtcorba -I$(top_builddir)/src/corba/rtcorba
aws_inc=-I$(top_srcdir)/src/aws \
	-I$(top_builddir)/src/aws \
	-I$(top_srcdir)/src/aws_orig \
	-I$(top_builddir)/src/aws_orig
giop_inc=-I$(top_srcdir)/src/giop -I$(top_builddir)/src/giop -I$(top_srcdir)/src/giop/iiop -I$(top_builddir)/src/giop/iiop -I$(top_srcdir)/src/giop/diop -I$(top_builddir)/src/giop/diop -I$(top_srcdir)/src/giop/miop -I$(top_builddir)/src/giop/miop
if HAVE_SSL
giop_inc+=-I$(top_srcdir)/src/giop/iiop/ssliop -I$(top_builddir)/src/giop/iiop/ssliop
endif
moma_inc=-I$(top_srcdir)/src/moma -I$(top_builddir)/src/moma
soap_inc=-I$(top_srcdir)/src/soap \
	-I$(top_builddir)/src/soap
srp_inc=-I$(top_srcdir)/src/srp -I$(top_builddir)/src/srp
web_common_inc=-I$(top_srcdir)/src/web_common \
	-I$(top_builddir)/src/web_common 

POLYORB_INCS = -I$(srcdir) -I$(top_srcdir)/src \
               -I$(top_builddir)/src \
               $(naming_inc) \
	       @APPLI_INCS@ @PROTO_INCS@ @SERVICE_INCS@ @LIBS_INCS@
if HAVE_SSL
POLYORB_INCS += -I$(top_srcdir)/src/ssl -I$(top_builddir)/src/ssl
endif

ADAFLAGS+= -i -I. -I$(srcdir) $(POLYORB_INCS) $(STD_ADAFLAGS)

install-data-local:
	$(INSTALL) -d $(DESTDIR)$(alidir)
	for f in $(srcdir)/*.ad[bs] *.ad[bs] *.ali; do $(INSTALL) -m 444 $$f $(DESTDIR)$(alidir); done

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
