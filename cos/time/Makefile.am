include $(top_srcdir)/cos/Makefile.common

alidir = $(includedir)/polyorb/corba/cos

COS_DIRS=$(top_builddir)/idls/cos/time

COS_ADAFLAGS = -I$(COS_DIRS)

IDLAC_FLAGS = -s -I$(top_srcdir)/idls/cos/time

IDLS = CosTime.idl

vpath %.idl $(top_srcdir)/idls/cos/time
vpath %.ads $(COS_DIRS)
vpath %.adb $(COS_DIRS)

#
# Building libpolyorb-corba-cos-time-impl library
#

GEN_SPECS_WITH_BODY = \
	costime-timeservice-skel.ads \
	costime-tio-skel.ads \
	costime-uto-skel.ads

ADA_SPECS_WITH_BODY = \
	$(GEN_SPECS_WITH_BODY) \
	costime-timeservice-impl.ads \
	costime-tio-impl.ads \
	costime-uto-impl.ads \
	time_utils.ads

lib_LTLIBRARIES = libpolyorb-corba-cos-time-impl.la

libpolyorb_corba_cos_time_impl_la_SOURCES =
libpolyorb_corba_cos_time_impl_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_time_impl_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
