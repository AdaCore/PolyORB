include $(top_srcdir)/cos/Makefile.common

# Override initial ADAFLAGS value to disable optimization if required

if DISABLE_IR_OPTIMIZATION
  ADAFLAGS+=-O0
endif

alidir = $(includedir)/polyorb/corba/cos

IMPL_SPECS_WITH_BODY= \
  corba-repository_root-abstractinterfacedef-impl.ads \
  corba-repository_root-aliasdef-impl.ads \
  corba-repository_root-arraydef-impl.ads \
  corba-repository_root-attributedef-impl.ads \
  corba-repository_root-constantdef-impl.ads \
  corba-repository_root-contained-impl.ads \
  corba-repository_root-container-impl.ads \
  corba-repository_root-enumdef-impl.ads \
  corba-repository_root-exceptiondef-impl.ads \
  corba-repository_root-extabstractinterfacedef-impl.ads \
  corba-repository_root-extattributedef-impl.ads \
  corba-repository_root-extinterfacedef-impl.ads \
  corba-repository_root-extlocalinterfacedef-impl.ads \
  corba-repository_root-fixeddef-impl.ads \
  corba-repository_root-idltype-impl.ads \
  corba-repository_root-interfacedef-impl.ads \
  corba-repository_root-interfaceattrextension-impl.ads \
  corba-repository_root-irobject-impl.ads \
  corba-repository_root-localinterfacedef-impl.ads \
  corba-repository_root-moduledef-impl.ads \
  corba-repository_root-nativedef-impl.ads \
  corba-repository_root-operationdef-impl.ads \
  corba-repository_root-primitivedef-impl.ads \
  corba-repository_root-repository-impl.ads \
  corba-repository_root-sequencedef-impl.ads \
  corba-repository_root-stringdef-impl.ads \
  corba-repository_root-structdef-impl.ads \
  corba-repository_root-typedefdef-impl.ads \
  corba-repository_root-valueboxdef-impl.ads \
  corba-repository_root-valuedef-impl.ads \
  corba-repository_root-valuememberdef-impl.ads \
  corba-repository_root-uniondef-impl.ads \
  corba-repository_root-wstringdef-impl.ads

GEN_SPECS_WITH_BODY= \
  corba-repository_root-abstractinterfacedef-helper.ads \
  corba-repository_root-abstractinterfacedef-skel.ads \
  corba-repository_root-abstractinterfacedef.ads \
  corba-repository_root-aliasdef-helper.ads \
  corba-repository_root-aliasdef-skel.ads \
  corba-repository_root-aliasdef.ads \
  corba-repository_root-arraydef-helper.ads \
  corba-repository_root-arraydef-skel.ads \
  corba-repository_root-arraydef.ads \
  corba-repository_root-attributedef-helper.ads \
  corba-repository_root-attributedef-skel.ads \
  corba-repository_root-attributedef.ads \
  corba-repository_root-constantdef-helper.ads \
  corba-repository_root-constantdef-skel.ads \
  corba-repository_root-constantdef.ads \
  corba-repository_root-contained-helper.ads \
  corba-repository_root-contained-skel.ads \
  corba-repository_root-contained.ads \
  corba-repository_root-container-helper.ads \
  corba-repository_root-container-skel.ads \
  corba-repository_root-container.ads \
  corba-repository_root-enumdef-helper.ads \
  corba-repository_root-enumdef-skel.ads \
  corba-repository_root-enumdef.ads \
  corba-repository_root-exceptiondef-helper.ads \
  corba-repository_root-exceptiondef-skel.ads \
  corba-repository_root-exceptiondef.ads \
  corba-repository_root-extabstractinterfacedef-helper.ads \
  corba-repository_root-extabstractinterfacedef-skel.ads \
  corba-repository_root-extabstractinterfacedef.ads \
  corba-repository_root-extattributedef-helper.ads \
  corba-repository_root-extattributedef-skel.ads \
  corba-repository_root-extattributedef.ads \
  corba-repository_root-extinterfacedef-helper.ads \
  corba-repository_root-extinterfacedef-skel.ads \
  corba-repository_root-extinterfacedef.ads \
  corba-repository_root-extlocalinterfacedef-helper.ads \
  corba-repository_root-extlocalinterfacedef-skel.ads \
  corba-repository_root-extlocalinterfacedef.ads \
  corba-repository_root-fixeddef-helper.ads \
  corba-repository_root-fixeddef-skel.ads \
  corba-repository_root-fixeddef.ads \
  corba-repository_root-helper.ads \
  corba-repository_root-idltype-helper.ads \
  corba-repository_root-idltype-skel.ads \
  corba-repository_root-idltype.ads \
  corba-repository_root-interfacedef-helper.ads \
  corba-repository_root-interfacedef-skel.ads \
  corba-repository_root-interfacedef.ads \
  corba-repository_root-interfaceattrextension-helper.ads \
  corba-repository_root-interfaceattrextension-skel.ads \
  corba-repository_root-interfaceattrextension.ads \
  corba-repository_root-irobject-helper.ads \
  corba-repository_root-irobject-skel.ads \
  corba-repository_root-irobject.ads \
  corba-repository_root-localinterfacedef-helper.ads \
  corba-repository_root-localinterfacedef-skel.ads \
  corba-repository_root-localinterfacedef.ads \
  corba-repository_root-moduledef-helper.ads \
  corba-repository_root-moduledef-skel.ads \
  corba-repository_root-moduledef.ads \
  corba-repository_root-nativedef-helper.ads \
  corba-repository_root-nativedef-skel.ads \
  corba-repository_root-nativedef.ads \
  corba-repository_root-operationdef-helper.ads \
  corba-repository_root-operationdef-skel.ads \
  corba-repository_root-operationdef.ads \
  corba-repository_root-primitivedef-helper.ads \
  corba-repository_root-primitivedef-skel.ads \
  corba-repository_root-primitivedef.ads \
  corba-repository_root-repository-helper.ads \
  corba-repository_root-repository-skel.ads \
  corba-repository_root-repository.ads \
  corba-repository_root-sequencedef-helper.ads \
  corba-repository_root-sequencedef-skel.ads \
  corba-repository_root-sequencedef.ads \
  corba-repository_root-stringdef-helper.ads \
  corba-repository_root-stringdef-skel.ads \
  corba-repository_root-stringdef.ads \
  corba-repository_root-structdef-helper.ads \
  corba-repository_root-structdef-skel.ads \
  corba-repository_root-structdef.ads \
  corba-repository_root-typedefdef-helper.ads \
  corba-repository_root-typedefdef-skel.ads \
  corba-repository_root-typedefdef.ads \
  corba-repository_root-uniondef-helper.ads \
  corba-repository_root-uniondef-skel.ads \
  corba-repository_root-uniondef.ads \
  corba-repository_root-valueboxdef-helper.ads \
  corba-repository_root-valueboxdef-skel.ads \
  corba-repository_root-valueboxdef.ads \
  corba-repository_root-valuedef-helper.ads \
  corba-repository_root-valuedef-skel.ads \
  corba-repository_root-valuedef.ads \
  corba-repository_root-valuememberdef-helper.ads \
  corba-repository_root-valuememberdef-skel.ads \
  corba-repository_root-valuememberdef.ads \
  corba-repository_root-wstringdef-helper.ads \
  corba-repository_root-wstringdef-skel.ads \
  corba-repository_root-wstringdef.ads

GEN= \
	$(GEN_SPECS_WITH_BODY)

$(GEN): orb.idl-stamp

orb.idl-stamp: $(top_srcdir)/idls/CORBA_IDL/orb.idl $(top_srcdir)/idls/CORBA_IDL/CORBA_InterfaceRepository.idl $(top_srcdir)/idls/CORBA_IDL/CORBA_TypeCode.idl $(IDLAC_bin)
	@chmod a+x $(IDLAC_WRAPPER)
	$(IDLAC_WRAPPER) --idlac=$(IDLAC_bin) \
		-I$(top_srcdir)/idls/CORBA_IDL \
		-I$(top_srcdir)/idls/CORBA_PIDL \
		$(IDLAC_FLAGS) $<
	$(RM) corba-repository_root.ads
	touch $@

$(GEN_BODIES): $(GEN)

DISTCLEANFILES += $(GEN) $(GEN_BODIES)

#
# Building libpolyorb-corba-cos-ir-impl library
#

ADA_SPECS_WITH_BODY = \
	$(IMPL_SPECS_WITH_BODY) \
	$(GEN_SPECS_WITH_BODY) \
	corba-orb-typecode.ads \
	polyorb-corba_p-ir_tools.ads

ADA_SPECS_NO_BODY = \
	$(GEN_SPECS_NO_BODY)

#
# Note that the generated sources must be available before we recompile
# the impl sources
#

IMPL_OBJS=$(IMPL_SPECS_WITH_BODY:.ads=.lo) $(IMPL_SPECS_NO_BODY:.ads=.lo)
$(IMPL_OBJS): $(GEN)

lib_LTLIBRARIES = libpolyorb-corba-cos-ir-impl.la

libpolyorb_corba_cos_ir_impl_la_SOURCES = $(GEN_BODIES)
libpolyorb_corba_cos_ir_impl_la_LIBADD  = $(ADA_LIBADD)
libpolyorb_corba_cos_ir_impl_la_LDFLAGS = -no-undefined -version-info $(LIBVERSIONINFO)
