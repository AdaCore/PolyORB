include $(top_srcdir)/cos/Makefile.common

alidir = $(includedir)/polyorb/corba/cos

COS_DIRS=$(top_builddir)/idls/cos/time

COS_ADAFLAGS = -I$(COS_DIRS)

#
# Building libpolyorb-corba-cos-time-impl library
#

ADA_SPECS_WITH_BODY = \
	costime-timeservice-impl.ads \
	costime-tio-impl.ads \
	costime-uto-impl.ads \
	time_utils.ads

lib_LTLIBRARIES = libpolyorb-corba-cos-time-impl.la

libpolyorb_corba_cos_time_impl_la_SOURCES = $(ADA_SPECS) $(ADA_BODIES)
libpolyorb_corba_cos_time_impl_la_LIBADD  =
libpolyorb_corba_cos_time_impl_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
