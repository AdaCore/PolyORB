include $(top_srcdir)/cos/Makefile.common

alidir = $(includedir)/polyorb/corba/cos

COS_DIRS=$(top_builddir)/idls/cos/event

COS_ADAFLAGS = -I$(COS_DIRS)

#
# Building libpolyorb-corba-cos-event-impl library
#

ADA_SPECS_WITH_BODY = \
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
	costypedeventcomm-typedpullsupplier-impl.ads

lib_LTLIBRARIES = libpolyorb-corba-cos-event-impl.la

libpolyorb_corba_cos_event_impl_la_SOURCES =
libpolyorb_corba_cos_event_impl_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_event_impl_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
