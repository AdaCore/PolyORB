include $(top_srcdir)/cos/Makefile.common

alidir = $(includedir)/polyorb/corba/cos

COS_DIRS=$(top_builddir)/idls/cos/notification

COS_ADAFLAGS=-I$(COS_DIRS) -I$(top_builddir)/idls/cos/event -I$(top_srcdir)/cos/event

#
# Building libpolyorb-corba-cos-notification-impl library
#

ADA_SPECS_WITH_BODY = \
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

