include $(top_srcdir)/idls/Makefile.common

alidir = $(includedir)/polyorb/corba/cos
IDLdir = $(includedir)/polyorb/corba/idls/cos/naming

IDLS=CosNaming.idl

COS_ADAFLAGS = -I$(top_srcdir)/cos/naming

# Do not include skels in libpolyorb-corba-cos-naming (client-side library)
# skels are meant to be compiled along with the impl.

GEN_SPECS_WITH_BODY= \
  cosnaming-bindingiterator-helper.ads \
  cosnaming-bindingiterator.ads \
  cosnaming-helper.ads \
  cosnaming-namingcontext-helper.ads \
  cosnaming-namingcontext.ads \
  cosnaming-namingcontextext-helper.ads \
  cosnaming-namingcontextext.ads       

GEN_BODIES = $(GEN_SPECS_WITH_BODY:.ads=.adb)

GEN_SPECS_NO_BODY= \
  cosnaming.ads

GEN= \
        $(GEN_SPECS_NO_BODY) \
        $(GEN_SPECS_WITH_BODY)

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

libpolyorb_corba_cos_naming_la_SOURCES = $(ADA_SOURCES)
libpolyorb_corba_cos_naming_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
