include $(top_srcdir)/idls/Makefile.common

alidir = $(includedir)/polyorb/corba/cos
IDLdir = $(includedir)/polyorb/corba/idls/cos/event

IDL=$(srcdir)/CosEventChannelAdmin.idl \
    $(srcdir)/CosEventComm.idl \
    $(srcdir)/CosTypedEventChannelAdmin.idl \
    $(srcdir)/CosTypedEventComm.idl

COS_ADAFLAGS = -I$(top_srcdir)/cos/event

GEN_SPECS_WITH_BODY= \
  coseventchanneladmin-consumeradmin-helper.ads \
  coseventchanneladmin-consumeradmin-skel.ads \
  coseventchanneladmin-consumeradmin.ads \
  coseventchanneladmin-eventchannel-helper.ads \
  coseventchanneladmin-eventchannel-skel.ads \
  coseventchanneladmin-eventchannel.ads \
  coseventchanneladmin-helper.ads \
  coseventchanneladmin-proxypullconsumer-helper.ads \
  coseventchanneladmin-proxypullconsumer-skel.ads \
  coseventchanneladmin-proxypullconsumer.ads \
  coseventchanneladmin-proxypullsupplier-helper.ads \
  coseventchanneladmin-proxypullsupplier-skel.ads \
  coseventchanneladmin-proxypullsupplier.ads \
  coseventchanneladmin-proxypushconsumer-helper.ads \
  coseventchanneladmin-proxypushconsumer-skel.ads \
  coseventchanneladmin-proxypushconsumer.ads \
  coseventchanneladmin-proxypushsupplier-helper.ads \
  coseventchanneladmin-proxypushsupplier-skel.ads \
  coseventchanneladmin-proxypushsupplier.ads \
  coseventchanneladmin-supplieradmin-helper.ads \
  coseventchanneladmin-supplieradmin-skel.ads \
  coseventchanneladmin-supplieradmin.ads \
  coseventchanneladmin.ads \
  coseventcomm-helper.ads \
  coseventcomm-pullconsumer-helper.ads \
  coseventcomm-pullconsumer-skel.ads \
  coseventcomm-pullconsumer.ads \
  coseventcomm-pullsupplier-helper.ads \
  coseventcomm-pullsupplier-skel.ads \
  coseventcomm-pullsupplier.ads \
  coseventcomm-pushconsumer-helper.ads \
  coseventcomm-pushconsumer-skel.ads \
  coseventcomm-pushconsumer.ads \
  coseventcomm-pushsupplier-helper.ads \
  coseventcomm-pushsupplier-skel.ads \
  coseventcomm-pushsupplier.ads \
  coseventcomm.ads \
  costypedeventchanneladmin-helper.ads \
  costypedeventchanneladmin-typedconsumeradmin-helper.ads \
  costypedeventchanneladmin-typedconsumeradmin-skel.ads \
  costypedeventchanneladmin-typedconsumeradmin.ads \
  costypedeventchanneladmin-typedeventchannel-helper.ads \
  costypedeventchanneladmin-typedeventchannel-skel.ads \
  costypedeventchanneladmin-typedeventchannel.ads \
  costypedeventchanneladmin-typedproxypullsupplier-helper.ads \
  costypedeventchanneladmin-typedproxypullsupplier-skel.ads \
  costypedeventchanneladmin-typedproxypullsupplier.ads \
  costypedeventchanneladmin-typedproxypushconsumer-helper.ads \
  costypedeventchanneladmin-typedproxypushconsumer-skel.ads \
  costypedeventchanneladmin-typedproxypushconsumer.ads \
  costypedeventchanneladmin-typedsupplieradmin-helper.ads \
  costypedeventchanneladmin-typedsupplieradmin-skel.ads \
  costypedeventchanneladmin-typedsupplieradmin.ads \
  costypedeventchanneladmin.ads \
  costypedeventcomm-typedpullsupplier-helper.ads \
  costypedeventcomm-typedpullsupplier-skel.ads \
  costypedeventcomm-typedpullsupplier.ads \
  costypedeventcomm-typedpushconsumer-helper.ads \
  costypedeventcomm-typedpushconsumer-skel.ads \
  costypedeventcomm-typedpushconsumer.ads

GEN_BODIES = $(GEN_SPECS_WITH_BODY:.ads=.adb)

GEN_SPECS_NO_BODY= \
  costypedeventcomm.ads

GEN= \
        $(GEN_SPECS_NO_BODY) \
        $(GEN_SPECS_WITH_BODY)

$(GEN): $(IDL) $(IDLAC)
	for i in $(IDL); do \
	    $(IDLAC) $$i -I$(srcdir); \
	done
	touch $@

$(GEN_BODIES): $(GEN)

$(ADA_LIBADD): $(GEN)

DISTCLEANFILES = $(GEN) $(GEN_BODIES)

#
# Building libpolyorb-corba-cos-event library
#

ADA_SPECS_WITH_BODY = \
        $(GEN_SPECS_WITH_BODY)

ADA_SPECS_NO_BODY = \
        $(GEN_SPECS_NO_BODY)

lib_LTLIBRARIES = libpolyorb-corba-cos-event.la

libpolyorb_corba_cos_event_la_SOURCES = $(GEN_BODIES)
libpolyorb_corba_cos_event_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_event_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
