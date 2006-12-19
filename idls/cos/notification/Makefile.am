include $(top_srcdir)/idls/Makefile.common

alidir = $(includedir)/polyorb/corba/cos
IDLdir = $(includedir)/polyorb/corba/idls/cos/notification

IDLS = \
	CosNotification.idl \
	CosNotifyChannelAdmin.idl \
	CosNotifyComm.idl \
	CosNotifyFilter.idl

IDLAC_FLAGS = \
	-c \
	-I$(top_srcdir)/idls/CORBA_IDL \
	-I$(top_srcdir)/idls/CORBA_PIDL \
	-I$(top_srcdir)/idls/Interop \
	-I$(top_srcdir)/idls/Misc \
	-I$(top_srcdir)/idls/cos/event

COS_ADAFLAGS = -I$(top_builddir)/idls/cos/event -I$(top_srcdir)/cos/notification

GEN_SPECS_WITH_BODY= \
	cosnotification-adminpropertiesadmin.ads \
	cosnotification-adminpropertiesadmin-helper.ads \
	cosnotification.ads \
	cosnotification-helper.ads \
	cosnotification-qosadmin.ads \
	cosnotification-qosadmin-helper.ads \
	cosnotifychanneladmin.ads \
	cosnotifychanneladmin-consumeradmin.ads \
	cosnotifychanneladmin-consumeradmin-helper.ads \
	cosnotifychanneladmin-eventchannel.ads \
	cosnotifychanneladmin-eventchannelfactory.ads \
	cosnotifychanneladmin-eventchannelfactory-helper.ads \
	cosnotifychanneladmin-eventchannel-helper.ads \
	cosnotifychanneladmin-helper.ads \
	cosnotifychanneladmin-proxyconsumer.ads \
	cosnotifychanneladmin-proxyconsumer-helper.ads \
	cosnotifychanneladmin-proxypullconsumer.ads \
	cosnotifychanneladmin-proxypullconsumer-helper.ads \
	cosnotifychanneladmin-proxypullsupplier.ads \
	cosnotifychanneladmin-proxypullsupplier-helper.ads \
	cosnotifychanneladmin-proxypushconsumer.ads \
	cosnotifychanneladmin-proxypushconsumer-helper.ads \
	cosnotifychanneladmin-proxypushsupplier.ads \
	cosnotifychanneladmin-proxypushsupplier-helper.ads \
	cosnotifychanneladmin-proxysupplier.ads \
	cosnotifychanneladmin-proxysupplier-helper.ads \
	cosnotifychanneladmin-sequenceproxypullconsumer.ads \
	cosnotifychanneladmin-sequenceproxypullconsumer-helper.ads \
	cosnotifychanneladmin-sequenceproxypullsupplier.ads \
	cosnotifychanneladmin-sequenceproxypullsupplier-helper.ads \
	cosnotifychanneladmin-sequenceproxypushconsumer.ads \
	cosnotifychanneladmin-sequenceproxypushconsumer-helper.ads \
	cosnotifychanneladmin-sequenceproxypushsupplier.ads \
	cosnotifychanneladmin-sequenceproxypushsupplier-helper.ads \
	cosnotifychanneladmin-structuredproxypullconsumer.ads \
	cosnotifychanneladmin-structuredproxypullconsumer-helper.ads \
	cosnotifychanneladmin-structuredproxypullsupplier.ads \
	cosnotifychanneladmin-structuredproxypullsupplier-helper.ads \
	cosnotifychanneladmin-structuredproxypushconsumer.ads \
	cosnotifychanneladmin-structuredproxypushconsumer-helper.ads \
	cosnotifychanneladmin-structuredproxypushsupplier.ads \
	cosnotifychanneladmin-structuredproxypushsupplier-helper.ads \
	cosnotifychanneladmin-supplieradmin.ads \
	cosnotifychanneladmin-supplieradmin-helper.ads \
	cosnotifycomm.ads \
	cosnotifycomm-helper.ads \
	cosnotifycomm-notifypublish.ads \
	cosnotifycomm-notifypublish-helper.ads \
	cosnotifycomm-notifysubscribe.ads \
	cosnotifycomm-notifysubscribe-helper.ads \
	cosnotifycomm-pullconsumer.ads \
	cosnotifycomm-pullconsumer-helper.ads \
	cosnotifycomm-pullsupplier.ads \
	cosnotifycomm-pullsupplier-helper.ads \
	cosnotifycomm-pushconsumer.ads \
	cosnotifycomm-pushconsumer-helper.ads \
	cosnotifycomm-pushsupplier.ads \
	cosnotifycomm-pushsupplier-helper.ads \
	cosnotifycomm-sequencepullconsumer.ads \
	cosnotifycomm-sequencepullconsumer-helper.ads \
	cosnotifycomm-sequencepullsupplier.ads \
	cosnotifycomm-sequencepullsupplier-helper.ads \
	cosnotifycomm-sequencepushconsumer.ads \
	cosnotifycomm-sequencepushconsumer-helper.ads \
	cosnotifycomm-sequencepushsupplier.ads \
	cosnotifycomm-sequencepushsupplier-helper.ads \
	cosnotifycomm-structuredpullconsumer.ads \
	cosnotifycomm-structuredpullconsumer-helper.ads \
	cosnotifycomm-structuredpullsupplier.ads \
	cosnotifycomm-structuredpullsupplier-helper.ads \
	cosnotifycomm-structuredpushconsumer.ads \
	cosnotifycomm-structuredpushconsumer-helper.ads \
	cosnotifycomm-structuredpushsupplier.ads \
	cosnotifycomm-structuredpushsupplier-helper.ads \
	cosnotifyfilter.ads \
	cosnotifyfilter-filteradmin.ads \
	cosnotifyfilter-filteradmin-helper.ads \
	cosnotifyfilter-filter.ads \
	cosnotifyfilter-filterfactory.ads \
	cosnotifyfilter-filterfactory-helper.ads \
	cosnotifyfilter-filter-helper.ads \
	cosnotifyfilter-helper.ads \
	cosnotifyfilter-mappingfilter.ads \
	cosnotifyfilter-mappingfilter-helper.ads

GEN_BODIES = $(GEN_SPECS_WITH_BODY:.ads=.adb)

GEN_SPECS_NO_BODY= 


GEN= \
        $(GEN_SPECS_NO_BODY) \
        $(GEN_SPECS_WITH_BODY)

$(GEN_BODIES): $(GEN)

DISTCLEANFILES = $(GEN) $(GEN_BODIES)

#
# Building libpolyorb-corba-cos-notification library
#

ADA_SPECS_WITH_BODY = \
        $(GEN_SPECS_WITH_BODY)

ADA_SPECS_NO_BODY = \
        $(GEN_SPECS_NO_BODY)

lib_LTLIBRARIES = libpolyorb-corba-cos-notification.la

libpolyorb_corba_cos_notification_la_SOURCES = $(ADA_SOURCES)
libpolyorb_corba_cos_notification_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
