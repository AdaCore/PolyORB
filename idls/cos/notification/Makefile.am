include $(top_srcdir)/idls/Makefile.common

alidir = $(includedir)/polyorb/corba/cos
IDLdir = $(includedir)/polyorb/corba/idls/cos/notification

IDLS = \
	CosNotification.idl \
	CosNotifyChannelAdmin.idl \
	CosNotifyComm.idl \
	CosNotifyFilter.idl

IDLAC_FLAGS = \
	-I$(top_srcdir)/idls/CORBA_IDL \
	-I$(top_srcdir)/idls/CORBA_PIDL \
	-I$(top_srcdir)/idls/Interop \
	-I$(top_srcdir)/idls/Misc \
	-I$(top_srcdir)/idls/cos/event

COS_ADAFLAGS = -I$(top_builddir)/idls/cos/event -I$(top_srcdir)/cos/notification

GEN_SPECS_WITH_BODY= \
	cosnotification-adminpropertiesadmin.ads \
	cosnotification-adminpropertiesadmin-helper.ads \
	cosnotification-adminpropertiesadmin-skel.ads \
	cosnotification.ads \
	cosnotification-helper.ads \
	cosnotification-qosadmin.ads \
	cosnotification-qosadmin-helper.ads \
	cosnotification-qosadmin-skel.ads \
	cosnotifychanneladmin.ads \
	cosnotifychanneladmin-consumeradmin.ads \
	cosnotifychanneladmin-consumeradmin-helper.ads \
	cosnotifychanneladmin-consumeradmin-skel.ads \
	cosnotifychanneladmin-eventchannel.ads \
	cosnotifychanneladmin-eventchannelfactory.ads \
	cosnotifychanneladmin-eventchannelfactory-helper.ads \
	cosnotifychanneladmin-eventchannelfactory-skel.ads \
	cosnotifychanneladmin-eventchannel-helper.ads \
	cosnotifychanneladmin-eventchannel-skel.ads \
	cosnotifychanneladmin-helper.ads \
	cosnotifychanneladmin-proxyconsumer.ads \
	cosnotifychanneladmin-proxyconsumer-helper.ads \
	cosnotifychanneladmin-proxyconsumer-skel.ads \
	cosnotifychanneladmin-proxypullconsumer.ads \
	cosnotifychanneladmin-proxypullconsumer-helper.ads \
	cosnotifychanneladmin-proxypullconsumer-skel.ads \
	cosnotifychanneladmin-proxypullsupplier.ads \
	cosnotifychanneladmin-proxypullsupplier-helper.ads \
	cosnotifychanneladmin-proxypullsupplier-skel.ads \
	cosnotifychanneladmin-proxypushconsumer.ads \
	cosnotifychanneladmin-proxypushconsumer-helper.ads \
	cosnotifychanneladmin-proxypushconsumer-skel.ads \
	cosnotifychanneladmin-proxypushsupplier.ads \
	cosnotifychanneladmin-proxypushsupplier-helper.ads \
	cosnotifychanneladmin-proxypushsupplier-skel.ads \
	cosnotifychanneladmin-proxysupplier.ads \
	cosnotifychanneladmin-proxysupplier-helper.ads \
	cosnotifychanneladmin-proxysupplier-skel.ads \
	cosnotifychanneladmin-sequenceproxypullconsumer.ads \
	cosnotifychanneladmin-sequenceproxypullconsumer-helper.ads \
	cosnotifychanneladmin-sequenceproxypullconsumer-skel.ads \
	cosnotifychanneladmin-sequenceproxypullsupplier.ads \
	cosnotifychanneladmin-sequenceproxypullsupplier-helper.ads \
	cosnotifychanneladmin-sequenceproxypullsupplier-skel.ads \
	cosnotifychanneladmin-sequenceproxypushconsumer.ads \
	cosnotifychanneladmin-sequenceproxypushconsumer-helper.ads \
	cosnotifychanneladmin-sequenceproxypushconsumer-skel.ads \
	cosnotifychanneladmin-sequenceproxypushsupplier.ads \
	cosnotifychanneladmin-sequenceproxypushsupplier-helper.ads \
	cosnotifychanneladmin-sequenceproxypushsupplier-skel.ads \
	cosnotifychanneladmin-structuredproxypullconsumer.ads \
	cosnotifychanneladmin-structuredproxypullconsumer-helper.ads \
	cosnotifychanneladmin-structuredproxypullconsumer-skel.ads \
	cosnotifychanneladmin-structuredproxypullsupplier.ads \
	cosnotifychanneladmin-structuredproxypullsupplier-helper.ads \
	cosnotifychanneladmin-structuredproxypullsupplier-skel.ads \
	cosnotifychanneladmin-structuredproxypushconsumer.ads \
	cosnotifychanneladmin-structuredproxypushconsumer-helper.ads \
	cosnotifychanneladmin-structuredproxypushconsumer-skel.ads \
	cosnotifychanneladmin-structuredproxypushsupplier.ads \
	cosnotifychanneladmin-structuredproxypushsupplier-helper.ads \
	cosnotifychanneladmin-structuredproxypushsupplier-skel.ads \
	cosnotifychanneladmin-supplieradmin.ads \
	cosnotifychanneladmin-supplieradmin-helper.ads \
	cosnotifychanneladmin-supplieradmin-skel.ads \
	cosnotifycomm.ads \
	cosnotifycomm-helper.ads \
	cosnotifycomm-notifypublish.ads \
	cosnotifycomm-notifypublish-helper.ads \
	cosnotifycomm-notifypublish-skel.ads \
	cosnotifycomm-notifysubscribe.ads \
	cosnotifycomm-notifysubscribe-helper.ads \
	cosnotifycomm-notifysubscribe-skel.ads \
	cosnotifycomm-pullconsumer.ads \
	cosnotifycomm-pullconsumer-helper.ads \
	cosnotifycomm-pullconsumer-skel.ads \
	cosnotifycomm-pullsupplier.ads \
	cosnotifycomm-pullsupplier-helper.ads \
	cosnotifycomm-pullsupplier-skel.ads \
	cosnotifycomm-pushconsumer.ads \
	cosnotifycomm-pushconsumer-helper.ads \
	cosnotifycomm-pushconsumer-skel.ads \
	cosnotifycomm-pushsupplier.ads \
	cosnotifycomm-pushsupplier-helper.ads \
	cosnotifycomm-pushsupplier-skel.ads \
	cosnotifycomm-sequencepullconsumer.ads \
	cosnotifycomm-sequencepullconsumer-helper.ads \
	cosnotifycomm-sequencepullconsumer-skel.ads \
	cosnotifycomm-sequencepullsupplier.ads \
	cosnotifycomm-sequencepullsupplier-helper.ads \
	cosnotifycomm-sequencepullsupplier-skel.ads \
	cosnotifycomm-sequencepushconsumer.ads \
	cosnotifycomm-sequencepushconsumer-helper.ads \
	cosnotifycomm-sequencepushconsumer-skel.ads \
	cosnotifycomm-sequencepushsupplier.ads \
	cosnotifycomm-sequencepushsupplier-helper.ads \
	cosnotifycomm-sequencepushsupplier-skel.ads \
	cosnotifycomm-structuredpullconsumer.ads \
	cosnotifycomm-structuredpullconsumer-helper.ads \
	cosnotifycomm-structuredpullconsumer-skel.ads \
	cosnotifycomm-structuredpullsupplier.ads \
	cosnotifycomm-structuredpullsupplier-helper.ads \
	cosnotifycomm-structuredpullsupplier-skel.ads \
	cosnotifycomm-structuredpushconsumer.ads \
	cosnotifycomm-structuredpushconsumer-helper.ads \
	cosnotifycomm-structuredpushconsumer-skel.ads \
	cosnotifycomm-structuredpushsupplier.ads \
	cosnotifycomm-structuredpushsupplier-helper.ads \
	cosnotifycomm-structuredpushsupplier-skel.ads \
	cosnotifyfilter.ads \
	cosnotifyfilter-filteradmin.ads \
	cosnotifyfilter-filteradmin-helper.ads \
	cosnotifyfilter-filteradmin-skel.ads \
	cosnotifyfilter-filter.ads \
	cosnotifyfilter-filterfactory.ads \
	cosnotifyfilter-filterfactory-helper.ads \
	cosnotifyfilter-filterfactory-skel.ads \
	cosnotifyfilter-filter-helper.ads \
	cosnotifyfilter-filter-skel.ads \
	cosnotifyfilter-helper.ads \
	cosnotifyfilter-mappingfilter.ads \
	cosnotifyfilter-mappingfilter-helper.ads \
	cosnotifyfilter-mappingfilter-skel.ads

GEN_BODIES = $(GEN_SPECS_WITH_BODY:.ads=.adb)

GEN_SPECS_NO_BODY= 


GEN= \
        $(GEN_SPECS_NO_BODY) \
        $(GEN_SPECS_WITH_BODY)

$(GEN_BODIES): $(GEN)

$(ADA_LIBADD): $(GEN)

DISTCLEANFILES = $(GEN) $(GEN_BODIES)

#
# Building libpolyorb-corba-cos-notification library
#

ADA_SPECS_WITH_BODY = \
        $(GEN_SPECS_WITH_BODY)

ADA_SPECS_NO_BODY = \
        $(GEN_SPECS_NO_BODY)

lib_LTLIBRARIES = libpolyorb-corba-cos-notification.la

libpolyorb_corba_cos_notification_la_SOURCES = $(GEN_BODIES)
libpolyorb_corba_cos_notification_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_notification_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
