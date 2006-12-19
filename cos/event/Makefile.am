include $(top_srcdir)/cos/Makefile.common

alidir = $(includedir)/polyorb/corba/cos

COS_DIRS=$(top_builddir)/idls/cos/event

COS_ADAFLAGS = -I$(COS_DIRS)

IDLS = \
	CosEventChannelAdmin.idl \
	CosEventComm.idl \
	CosTypedEventChannelAdmin.idl \
	CosTypedEventComm.idl

IDLAC_FLAGS = -s -I$(top_srcdir)/idls/cos/event

vpath %.idl $(top_srcdir)/idls/cos/event
vpath %.ads $(COS_DIRS)
vpath %.adb $(COS_DIRS)

#
# Building libpolyorb-corba-cos-event-impl library
#

GEN_SPECS_WITH_BODY = \
	coseventchanneladmin-consumeradmin-skel.ads \
	coseventchanneladmin-eventchannel-skel.ads \
	coseventchanneladmin-proxypullconsumer-skel.ads \
	coseventchanneladmin-proxypullsupplier-skel.ads \
	coseventchanneladmin-proxypushconsumer-skel.ads \
	coseventchanneladmin-proxypushsupplier-skel.ads \
	coseventchanneladmin-supplieradmin-skel.ads \
	coseventcomm-pullconsumer-skel.ads \
	coseventcomm-pullsupplier-skel.ads \
	coseventcomm-pushconsumer-skel.ads \
	coseventcomm-pushsupplier-skel.ads \
	costypedeventchanneladmin-typedconsumeradmin-skel.ads \
	costypedeventchanneladmin-typedeventchannel-skel.ads \
	costypedeventchanneladmin-typedproxypullsupplier-skel.ads \
	costypedeventchanneladmin-typedproxypushconsumer-skel.ads \
	costypedeventchanneladmin-typedsupplieradmin-skel.ads \
	costypedeventcomm-typedpullsupplier-skel.ads \
	costypedeventcomm-typedpushconsumer-skel.ads

ADA_SPECS_WITH_BODY = \
	$(GEN_SPECS_WITH_BODY) \
	coseventchanneladmin-consumeradmin-impl.ads \
	coseventchanneladmin-eventchannel-impl.ads \
	coseventchanneladmin-proxypullconsumer-impl.ads \
	coseventchanneladmin-proxypullsupplier-impl.ads \
	coseventchanneladmin-proxypushconsumer-impl.ads \
	coseventchanneladmin-proxypushsupplier-impl.ads \
	coseventchanneladmin-supplieradmin-impl.ads \
	coseventcomm-pullconsumer-impl.ads \
	coseventcomm-pullsupplier-impl.ads \
	coseventcomm-pushconsumer-impl.ads \
	coseventcomm-pushsupplier-impl.ads \
	costypedeventchanneladmin-typedconsumeradmin-impl.ads \
	costypedeventchanneladmin-typedeventchannel-impl.ads \
	costypedeventchanneladmin-typedproxypullsupplier-impl.ads \
	costypedeventchanneladmin-typedproxypushconsumer-impl.ads \
	costypedeventchanneladmin-typedsupplieradmin-impl.ads \
	costypedeventcomm-typedpullsupplier-impl.ads \
	costypedeventcomm-typedpushconsumer-impl.ads

lib_LTLIBRARIES = libpolyorb-corba-cos-event-impl.la

libpolyorb_corba_cos_event_impl_la_SOURCES =
libpolyorb_corba_cos_event_impl_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_event_impl_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
