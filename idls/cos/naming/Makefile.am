include $(top_srcdir)/idls/Makefile.common

alidir = $(includedir)/polyorb/corba/cos
IDLdir = $(includedir)/polyorb/corba/idls/cos/naming

IDL=$(srcdir)/CosNaming.idl

COS_ADAFLAGS = -I$(top_srcdir)/cos/naming

GEN_SPECS_WITH_BODY= \
  cosnaming-bindingiterator-helper.ads \
  cosnaming-bindingiterator-skel.ads \
  cosnaming-bindingiterator.ads \
  cosnaming-helper.ads \
  cosnaming-namingcontext-helper.ads \
  cosnaming-namingcontext-skel.ads \
  cosnaming-namingcontext.ads

GEN_BODIES = $(GEN_SPECS_WITH_BODY:.ads=.adb)

GEN_SPECS_NO_BODY= \
  cosnaming.ads

GEN= \
        $(GEN_SPECS_NO_BODY) \
        $(GEN_SPECS_WITH_BODY)

$(GEN): $(IDL) $(IDLAC)
	$(IDLAC) $(IDL) -I$(srcdir)
	touch $@

$(GEN_BODIES): $(GEN)

DISTCLEANFILES = $(GEN) $(GEN_BODIES)

#
# Building libpolyorb-corba-cos-naming library
#

ADA_SPECS_WITH_BODY = \
        $(GEN_SPECS_WITH_BODY)

ADA_SPECS_NO_BODY = \
        $(GEN_SPECS_NO_BODY)

lib_LTLIBRARIES = libpolyorb-corba-cos-naming.la

libpolyorb_corba_cos_naming_la_SOURCES = $(GEN_BODIES)
libpolyorb_corba_cos_naming_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_naming_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
