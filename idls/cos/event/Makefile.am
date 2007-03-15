include $(top_srcdir)/idls/Makefile.common

alidir = $(includedir)/polyorb/corba/cos
IDLdir = $(includedir)/polyorb/corba/idls/cos/event

IDLS = \
	CosEventChannelAdmin.idl \
	CosEventComm.idl \
	CosTypedEventChannelAdmin.idl \
	CosTypedEventComm.idl

COS_ADAFLAGS = -I$(top_srcdir)/cos/event

IDLAC_FLAGS = -c

GEN_SPECS_WITH_BODY= \
	coseventchanneladmin-consumeradmin-helper.ads \
	coseventchanneladmin-consumeradmin.ads \
	coseventchanneladmin-eventchannel-helper.ads \
	coseventchanneladmin-eventchannel.ads \
	coseventchanneladmin-helper.ads \
	coseventchanneladmin-proxypullconsumer-helper.ads \
	coseventchanneladmin-proxypullconsumer.ads \
	coseventchanneladmin-proxypullsupplier-helper.ads \
	coseventchanneladmin-proxypullsupplier.ads \
	coseventchanneladmin-proxypushconsumer-helper.ads \
	coseventchanneladmin-proxypushconsumer.ads \
	coseventchanneladmin-proxypushsupplier-helper.ads \
	coseventchanneladmin-proxypushsupplier.ads \
	coseventchanneladmin-supplieradmin-helper.ads \
	coseventchanneladmin-supplieradmin.ads \
	coseventchanneladmin.ads \
	coseventcomm-helper.ads \
	coseventcomm-pullconsumer-helper.ads \
	coseventcomm-pullconsumer.ads \
	coseventcomm-pullsupplier-helper.ads \
	coseventcomm-pullsupplier.ads \
	coseventcomm-pushconsumer-helper.ads \
	coseventcomm-pushconsumer.ads \
	coseventcomm-pushsupplier-helper.ads \
	coseventcomm-pushsupplier.ads \
	coseventcomm.ads \
	costypedeventchanneladmin-helper.ads \
	costypedeventchanneladmin-typedconsumeradmin-helper.ads \
	costypedeventchanneladmin-typedconsumeradmin.ads \
	costypedeventchanneladmin-typedeventchannel-helper.ads \
	costypedeventchanneladmin-typedeventchannel.ads \
	costypedeventchanneladmin-typedproxypullsupplier-helper.ads \
	costypedeventchanneladmin-typedproxypullsupplier.ads \
	costypedeventchanneladmin-typedproxypushconsumer-helper.ads \
	costypedeventchanneladmin-typedproxypushconsumer.ads \
	costypedeventchanneladmin-typedsupplieradmin-helper.ads \
	costypedeventchanneladmin-typedsupplieradmin.ads \
	costypedeventchanneladmin.ads \
	costypedeventcomm-typedpullsupplier-helper.ads \
	costypedeventcomm-typedpullsupplier.ads \
	costypedeventcomm-typedpushconsumer-helper.ads \
	costypedeventcomm-typedpushconsumer.ads

GEN_BODIES = $(GEN_SPECS_WITH_BODY:.ads=.adb)

GEN_SPECS_NO_BODY= \
	costypedeventcomm.ads

GEN= \
        $(GEN_SPECS_NO_BODY) \
        $(GEN_SPECS_WITH_BODY)

$(GEN_BODIES): $(GEN)

DISTCLEANFILES = $(GEN) $(GEN_BODIES)

#
# Building libpolyorb-corba-cos-event library
#

ADA_SPECS_WITH_BODY = \
        $(GEN_SPECS_WITH_BODY)

ADA_SPECS_NO_BODY = \
        $(GEN_SPECS_NO_BODY)

lib_LTLIBRARIES = libpolyorb-corba-cos-event.la

libpolyorb_corba_cos_event_la_SOURCES = $(ADA_SOURCES)
libpolyorb_corba_cos_event_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
