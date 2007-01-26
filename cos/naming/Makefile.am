include $(top_srcdir)/cos/Makefile.common

alidir = $(includedir)/polyorb/corba/cos

COS_DIRS=$(top_builddir)/idls/cos/naming

COS_ADAFLAGS=-I$(COS_DIRS)

#
# Building libpolyorb-corba-cos-naming-impl library
#

vpath %.ads $(COS_DIRS)
vpath %.adb $(COS_DIRS)

GEN_SPECS_WITH_BODY = \
	cosnaming-bindingiterator-skel.ads \
	cosnaming-namingcontext-skel.ads \
	cosnaming-namingcontextext-skel.ads

ADA_SPECS_WITH_BODY = \
	cosnaming-bindingiterator-impl.ads \
	cosnaming-namingcontext-impl.ads \
	cosnaming-namingcontextext-impl.ads \
	polyorb-corba_p-naming_tools.ads

lib_LTLIBRARIES = libpolyorb-corba-cos-naming-impl.la

libpolyorb_corba_cos_naming_impl_la_SOURCES =
libpolyorb_corba_cos_naming_impl_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_naming_impl_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)

IDL=$(srcdir)/File.idl

GEN=\
file-helper.adb \
file-helper.ads \
file-ir_info.adb \
file-ir_info.ads \
file_idl_file-helper.adb \
file_idl_file-helper.ads \
file_idl_file-ir_info.adb \
file_idl_file-ir_info.ads \
file-skel.adb \
file-skel.ads \
file-stream.adb \
file-stream.ads \
file.adb \
file.ads \
file_idl_file.ads

IMPL=\
file-impl.adb \
file-impl.ads

CLEANFILES += $(GEN) stamp-gen

$(GEN): stamp-gen
stamp-gen: $(IDL) $(IDLAC_bin)
	$(IDLAC_WRAPPER) --idlac=$(IDLAC_bin) $(IDL)
	touch stamp-gen
