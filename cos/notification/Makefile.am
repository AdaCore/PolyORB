include $(top_srcdir)/cos/Makefile.common

alidir = $(includedir)/polyorb/corba/cos

COS_DIRS=$(top_builddir)/idls/cos/notification

COS_ADAFLAGS=-I$(COS_DIRS) -I$(top_builddir)/idls/cos/event -I$(top_srcdir)/cos/event -I$(top_builddir)/cos/event

IDLS = \
	CosNotification.idl \
	CosNotifyChannelAdmin.idl \
	CosNotifyComm.idl \
	CosNotifyFilter.idl

IDLAC_FLAGS = -s \
	-I$(top_srcdir)/idls/CORBA_IDL \
	-I$(top_srcdir)/idls/CORBA_PIDL \
	-I$(top_srcdir)/idls/Interop \
	-I$(top_srcdir)/idls/Misc \
	-I$(top_srcdir)/idls/cos/event \
	-I$(top_srcdir)/idls/cos/notification

vpath %.idl $(top_srcdir)/idls/cos/notification
vpath %.ads $(COS_DIRS)
vpath %.adb $(COS_DIRS)

#
# Building libpolyorb-corba-cos-notification-impl library
#

GEN_SPECS_WITH_BODY = \
	cosnotification-adminpropertiesadmin-skel.ads \
	cosnotification-qosadmin-skel.ads \
	cosnotifychanneladmin-consumeradmin-skel.ads \
	cosnotifychanneladmin-eventchannelfactory-skel.ads \
	cosnotifychanneladmin-eventchannel-skel.ads \
	cosnotifychanneladmin-proxyconsumer-skel.ads \
	cosnotifychanneladmin-proxypullconsumer-skel.ads \
	cosnotifychanneladmin-proxypullsupplier-skel.ads \
	cosnotifychanneladmin-proxypushconsumer-skel.ads \
	cosnotifychanneladmin-proxypushsupplier-skel.ads \
	cosnotifychanneladmin-proxysupplier-skel.ads \
	cosnotifychanneladmin-sequenceproxypullconsumer-skel.ads \
	cosnotifychanneladmin-sequenceproxypullsupplier-skel.ads \
	cosnotifychanneladmin-sequenceproxypushconsumer-skel.ads \
	cosnotifychanneladmin-sequenceproxypushsupplier-skel.ads \
	cosnotifychanneladmin-structuredproxypullconsumer-skel.ads \
	cosnotifychanneladmin-structuredproxypullsupplier-skel.ads \
	cosnotifychanneladmin-structuredproxypushconsumer-skel.ads \
	cosnotifychanneladmin-structuredproxypushsupplier-skel.ads \
	cosnotifychanneladmin-supplieradmin-skel.ads \
	cosnotifycomm-notifypublish-skel.ads \
	cosnotifycomm-notifysubscribe-skel.ads \
	cosnotifycomm-pullconsumer-skel.ads \
	cosnotifycomm-pullsupplier-skel.ads \
	cosnotifycomm-pushconsumer-skel.ads \
	cosnotifycomm-pushsupplier-skel.ads \
	cosnotifycomm-sequencepullconsumer-skel.ads \
	cosnotifycomm-sequencepullsupplier-skel.ads \
	cosnotifycomm-sequencepushconsumer-skel.ads \
	cosnotifycomm-sequencepushsupplier-skel.ads \
	cosnotifycomm-structuredpullconsumer-skel.ads \
	cosnotifycomm-structuredpullsupplier-skel.ads \
	cosnotifycomm-structuredpushconsumer-skel.ads \
	cosnotifycomm-structuredpushsupplier-skel.ads \
	cosnotifyfilter-filteradmin-skel.ads \
	cosnotifyfilter-filterfactory-skel.ads \
	cosnotifyfilter-filter-skel.ads \
	cosnotifyfilter-mappingfilter-skel.ads

ADA_SPECS_WITH_BODY = \
	$(GEN_SPECS_WITH_BODY) \
	cosnotification-adminpropertiesadmin-impl.ads \
	cosnotification-qosadmin-impl.ads \
	cosnotifychanneladmin-consumeradmin-impl.ads \
	cosnotifychanneladmin-eventchannelfactory-impl.ads \
	cosnotifychanneladmin-eventchannel-impl.ads \
	cosnotifychanneladmin-proxyconsumer-impl.ads \
	cosnotifychanneladmin-proxypullconsumer-impl.ads \
	cosnotifychanneladmin-proxypullsupplier-impl.ads \
	cosnotifychanneladmin-proxypushconsumer-impl.ads \
	cosnotifychanneladmin-proxypushsupplier-impl.ads \
	cosnotifychanneladmin-proxysupplier-impl.ads \
	cosnotifychanneladmin-sequenceproxypullconsumer-impl.ads \
	cosnotifychanneladmin-sequenceproxypullsupplier-impl.ads \
	cosnotifychanneladmin-sequenceproxypushconsumer-impl.ads \
	cosnotifychanneladmin-sequenceproxypushsupplier-impl.ads \
	cosnotifychanneladmin-structuredproxypullconsumer-impl.ads \
	cosnotifychanneladmin-structuredproxypullsupplier-impl.ads \
	cosnotifychanneladmin-structuredproxypushconsumer-impl.ads \
	cosnotifychanneladmin-structuredproxypushsupplier-impl.ads \
	cosnotifychanneladmin-supplieradmin-impl.ads \
	cosnotifycomm-notifypublish-impl.ads \
	cosnotifycomm-notifysubscribe-impl.ads \
	cosnotifycomm-pullconsumer-impl.ads \
	cosnotifycomm-pullsupplier-impl.ads \
	cosnotifycomm-pushconsumer-impl.ads \
	cosnotifycomm-pushsupplier-impl.ads \
	cosnotifycomm-sequencepullconsumer-impl.ads \
	cosnotifycomm-sequencepullsupplier-impl.ads \
	cosnotifycomm-sequencepushconsumer-impl.ads \
	cosnotifycomm-sequencepushsupplier-impl.ads \
	cosnotifycomm-structuredpullconsumer-impl.ads \
	cosnotifycomm-structuredpullsupplier-impl.ads \
	cosnotifycomm-structuredpushconsumer-impl.ads \
	cosnotifycomm-structuredpushsupplier-impl.ads \
	cosnotifyfilter-filteradmin-impl.ads \
	cosnotifyfilter-filterfactory-impl.ads \
	cosnotifyfilter-filter-impl.ads \
	cosnotifyfilter-mappingfilter-impl.ads

lib_LTLIBRARIES = libpolyorb-corba-cos-notification-impl.la

libpolyorb_corba_cos_notification_impl_la_SOURCES =
libpolyorb_corba_cos_notification_impl_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_notification_impl_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

