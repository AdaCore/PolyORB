include $(top_srcdir)/idls/Makefile.common

alidir = $(includedir)/polyorb/corba/cos
IDLdir = $(includedir)/polyorb/corba/idls/cos/time

IDLS = \
	CosTime.idl \
	TimeBase.idl

COS_ADAFLAGS = -I$(top_srcdir)/cos/time

GEN_SPECS_WITH_BODY= \
  costime-helper.ads \
  costime-timeservice-helper.ads \
  costime-timeservice-skel.ads \
  costime-timeservice.ads \
  costime-tio-helper.ads \
  costime-tio-skel.ads \
  costime-tio.ads \
  costime-uto-helper.ads \
  costime-uto-skel.ads \
  costime-uto.ads \
  costime.ads \
  timebase-helper.ads

GEN_BODIES = $(GEN_SPECS_WITH_BODY:.ads=.adb)

GEN_SPECS_NO_BODY= \
  timebase.ads

GEN= \
        $(GEN_SPECS_NO_BODY) \
        $(GEN_SPECS_WITH_BODY)

$(GEN_BODIES): $(GEN)

DISTCLEANFILES = $(GEN) $(GEN_BODIES)

#
# Building libpolyorb-corba-cos-time library
#

ADA_SPECS_WITH_BODY = \
        $(GEN_SPECS_WITH_BODY)

ADA_SPECS_NO_BODY = \
        $(GEN_SPECS_NO_BODY)

lib_LTLIBRARIES = libpolyorb-corba-cos-time.la

libpolyorb_corba_cos_time_la_SOURCES = $(GEN_BODIES)
libpolyorb_corba_cos_time_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_time_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
