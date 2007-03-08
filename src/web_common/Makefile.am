SUBDIRS = .

include $(srcdir)/../Makefile.common

alidir = $(includedir)/polyorb/web_common

ADA_SPECS_WITH_BODY = \
	aws-url.ads \
	aws-url-raise_url_error.ads \
	aws-mime.ads \
	aws-utils.ads \
	md5.ads \
	soap-utils.ads

ADA_SPECS = $(ADA_SPECS_WITH_BODY) \
	aws.ads \
	soap.ads

ADA_BODIES = $(ADA_SPECS_WITH_BODY:.ads=.adb)
ADA_LIBADD = $(ADA_SPECS:.ads=.lo)
ADA_ALIS = $(ADA_SPECS:.ads=.ali)

ali_HEADERS = $(ADA_SPECS) $(ADA_BODIES)
ali_DATA = $(ADA_ALIS)
lib_LTLIBRARIES = libpolyorb-web_common.la

libpolyorb_web_common_la_SOURCES =
libpolyorb_web_common_la_LIBADD = $(ADA_LIBADD)
libpolyorb_web_common_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

CLEANFILES = *.ali

ADAFLAGS+= -i -I. -I$(srcdir) -I.. -I$(srcdir)/.. $(STD_ADAFLAGS)

### DO NOT REMOVE THIS LINE, IT IS USED BY MAKEDEPEND ###
aws-mime.lo: $(top_srcdir)/src/web_common/aws-mime.adb $(top_srcdir)/src/web_common/aws-mime.ads $(top_srcdir)/src/web_common/aws.ads
aws-url-raise_url_error.lo: $(top_srcdir)/src/web_common/aws-url-raise_url_error.adb $(top_srcdir)/src/web_common/aws-url-raise_url_error.ads $(top_srcdir)/src/web_common/aws-url.ads $(top_srcdir)/src/web_common/aws.ads
aws-url.lo: $(top_srcdir)/src/polyorb-utils.adb $(top_srcdir)/src/polyorb-utils.ads $(top_srcdir)/src/polyorb.ads $(top_srcdir)/src/web_common/aws-url-raise_url_error.ads $(top_srcdir)/src/web_common/aws-url.adb $(top_srcdir)/src/web_common/aws-url.ads $(top_srcdir)/src/web_common/aws-utils.ads $(top_srcdir)/src/web_common/aws.ads $(top_srcdir)/src/web_common/md5.ads
aws-utils.lo: $(top_srcdir)/src/web_common/aws-utils.adb $(top_srcdir)/src/web_common/aws-utils.ads $(top_srcdir)/src/web_common/aws.ads $(top_srcdir)/src/web_common/md5.ads
aws.lo: $(top_srcdir)/src/web_common/aws.ads
md5.lo: $(top_srcdir)/src/web_common/md5.adb $(top_srcdir)/src/web_common/md5.ads
soap-utils.lo: $(top_srcdir)/src/web_common/soap-utils.adb $(top_srcdir)/src/web_common/soap-utils.ads $(top_srcdir)/src/web_common/soap.ads
soap.lo: $(top_srcdir)/src/web_common/soap.ads
