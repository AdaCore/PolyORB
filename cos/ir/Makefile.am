AUTOMAKE_OPTIONS = no-dependencies

include $(srcdir)/../Makefile.common

COS_DIRS=../../idls/cos/naming
COS_GNATMAKE_FLAGS=-I../../idls/cos/naming -I$(srcdir)/../naming

IMPL= corba-repository_root-container-impl.adb \
  corba-repository_root-irobject-impl.adb \
  corba-repository_root-contained-impl.adb \
  corba-repository_root-moduledef-impl.adb \
  corba-repository_root-interfacedef-impl.adb \
  corba-repository_root-valuedef-impl.adb \
  corba-repository_root-exceptiondef-impl.adb \
  corba-repository_root-typedefdef-impl.adb \
  corba-repository_root-uniondef-impl.adb \
  corba-repository_root-structdef-impl.adb \
  corba-repository_root-repository-impl.adb \
  corba-repository_root-idltype-impl.adb \
  corba-repository_root-primitivedef-impl.adb \
  corba-repository_root-stringdef-impl.adb \
  corba-repository_root-wstringdef-impl.adb \
  corba-repository_root-sequencedef-impl.adb \
  corba-repository_root-fixeddef-impl.adb \
  corba-repository_root-arraydef-impl.adb \
  corba-repository_root-enumdef-impl.adb \
  corba-repository_root-aliasdef-impl.adb \
  corba-repository_root-constantdef-impl.adb \
  corba-repository_root-attributedef-impl.adb \
  corba-repository_root-operationdef-impl.adb \
  corba-repository_root-valuememberdef-impl.adb \
  corba-repository_root-nativedef-impl.adb 

GEN_DIR=../../idls/cos/ir

GEN= \
  corba-repository_root-aliasdef-helper.adb \
  corba-repository_root-aliasdef-helper.ads \
  corba-repository_root-aliasdef-skel.adb \
  corba-repository_root-aliasdef-skel.ads \
  corba-repository_root-aliasdef.adb \
  corba-repository_root-aliasdef.ads \
  corba-repository_root-arraydef-helper.adb \
  corba-repository_root-arraydef-helper.ads \
  corba-repository_root-arraydef-skel.adb \
  corba-repository_root-arraydef-skel.ads \
  corba-repository_root-arraydef.adb \
  corba-repository_root-arraydef.ads \
  corba-repository_root-attributedef-helper.adb \
  corba-repository_root-attributedef-helper.ads \
  corba-repository_root-attributedef-skel.adb \
  corba-repository_root-attributedef-skel.ads \
  corba-repository_root-attributedef.adb \
  corba-repository_root-attributedef.ads \
  corba-repository_root-constantdef-helper.adb \
  corba-repository_root-constantdef-helper.ads \
  corba-repository_root-constantdef-skel.adb \
  corba-repository_root-constantdef-skel.ads \
  corba-repository_root-constantdef.adb \
  corba-repository_root-constantdef.ads \
  corba-repository_root-contained-helper.adb \
  corba-repository_root-contained-helper.ads \
  corba-repository_root-contained-skel.adb \
  corba-repository_root-contained-skel.ads \
  corba-repository_root-contained.adb \
  corba-repository_root-contained.ads \
  corba-repository_root-container-helper.adb \
  corba-repository_root-container-helper.ads \
  corba-repository_root-container-skel.adb \
  corba-repository_root-container-skel.ads \
  corba-repository_root-container.adb \
  corba-repository_root-container.ads \
  corba-repository_root-enumdef-helper.adb \
  corba-repository_root-enumdef-helper.ads \
  corba-repository_root-enumdef-skel.adb \
  corba-repository_root-enumdef-skel.ads \
  corba-repository_root-enumdef.adb \
  corba-repository_root-enumdef.ads \
  corba-repository_root-exceptiondef-helper.adb \
  corba-repository_root-exceptiondef-helper.ads \
  corba-repository_root-exceptiondef-skel.adb \
  corba-repository_root-exceptiondef-skel.ads \
  corba-repository_root-exceptiondef.adb \
  corba-repository_root-exceptiondef.ads \
  corba-repository_root-fixeddef-helper.adb \
  corba-repository_root-fixeddef-helper.ads \
  corba-repository_root-fixeddef-skel.adb \
  corba-repository_root-fixeddef-skel.ads \
  corba-repository_root-fixeddef.adb \
  corba-repository_root-fixeddef.ads \
  corba-repository_root-helper.adb \
  corba-repository_root-helper.ads \
  corba-repository_root-idltype-helper.adb \
  corba-repository_root-idltype-helper.ads \
  corba-repository_root-idltype-skel.adb \
  corba-repository_root-idltype-skel.ads \
  corba-repository_root-idltype.adb \
  corba-repository_root-idltype.ads \
  corba-repository_root-interfacedef-helper.adb \
  corba-repository_root-interfacedef-helper.ads \
  corba-repository_root-interfacedef-skel.adb \
  corba-repository_root-interfacedef-skel.ads \
  corba-repository_root-interfacedef.adb \
  corba-repository_root-interfacedef.ads \
  corba-repository_root-irobject-helper.adb \
  corba-repository_root-irobject-helper.ads \
  corba-repository_root-irobject-skel.adb \
  corba-repository_root-irobject-skel.ads \
  corba-repository_root-irobject.adb \
  corba-repository_root-irobject.ads \
  corba-repository_root-moduledef-helper.adb \
  corba-repository_root-moduledef-helper.ads \
  corba-repository_root-moduledef-skel.adb \
  corba-repository_root-moduledef-skel.ads \
  corba-repository_root-moduledef.adb \
  corba-repository_root-moduledef.ads \
  corba-repository_root-nativedef-helper.adb \
  corba-repository_root-nativedef-helper.ads \
  corba-repository_root-nativedef-skel.adb \
  corba-repository_root-nativedef-skel.ads \
  corba-repository_root-nativedef.ads \
  corba-repository_root-nativedef.adb \
  corba-repository_root-operationdef-helper.adb \
  corba-repository_root-operationdef-helper.ads \
  corba-repository_root-operationdef-skel.adb \
  corba-repository_root-operationdef-skel.ads \
  corba-repository_root-operationdef.adb \
  corba-repository_root-operationdef.ads \
  corba-repository_root-primitivedef-helper.adb \
  corba-repository_root-primitivedef-helper.ads \
  corba-repository_root-primitivedef-skel.adb \
  corba-repository_root-primitivedef-skel.ads \
  corba-repository_root-primitivedef.adb \
  corba-repository_root-primitivedef.ads \
  corba-repository_root-repository-helper.adb \
  corba-repository_root-repository-helper.ads \
  corba-repository_root-repository-skel.adb \
  corba-repository_root-repository-skel.ads \
  corba-repository_root-repository.adb \
  corba-repository_root-repository.ads \
  corba-repository_root-sequencedef-helper.adb \
  corba-repository_root-sequencedef-helper.ads \
  corba-repository_root-sequencedef-skel.adb \
  corba-repository_root-sequencedef-skel.ads \
  corba-repository_root-sequencedef.adb \
  corba-repository_root-sequencedef.ads \
  corba-repository_root-stringdef-helper.adb \
  corba-repository_root-stringdef-helper.ads \
  corba-repository_root-stringdef-skel.adb \
  corba-repository_root-stringdef-skel.ads \
  corba-repository_root-stringdef.adb \
  corba-repository_root-stringdef.ads \
  corba-repository_root-structdef-helper.adb \
  corba-repository_root-structdef-helper.ads \
  corba-repository_root-structdef-skel.adb \
  corba-repository_root-structdef-skel.ads \
  corba-repository_root-structdef.adb \
  corba-repository_root-structdef.ads \
  corba-repository_root-typedefdef-helper.adb \
  corba-repository_root-typedefdef-helper.ads \
  corba-repository_root-typedefdef-skel.adb \
  corba-repository_root-typedefdef-skel.ads \
  corba-repository_root-typedefdef.adb \
  corba-repository_root-typedefdef.ads \
  corba-repository_root-uniondef-helper.adb \
  corba-repository_root-uniondef-helper.ads \
  corba-repository_root-uniondef-skel.adb \
  corba-repository_root-uniondef-skel.ads \
  corba-repository_root-uniondef.adb \
  corba-repository_root-uniondef.ads \
  corba-repository_root-valueboxdef-helper.adb \
  corba-repository_root-valueboxdef-helper.ads \
  corba-repository_root-valueboxdef-skel.adb \
  corba-repository_root-valueboxdef-skel.ads \
  corba-repository_root-valueboxdef.adb \
  corba-repository_root-valueboxdef.ads \
  corba-repository_root-valuedef-helper.adb \
  corba-repository_root-valuedef-helper.ads \
  corba-repository_root-valuedef-skel.adb \
  corba-repository_root-valuedef-skel.ads \
  corba-repository_root-valuedef.adb \
  corba-repository_root-valuedef.ads \
  corba-repository_root-valuememberdef-helper.adb \
  corba-repository_root-valuememberdef-helper.ads \
  corba-repository_root-valuememberdef-skel.adb \
  corba-repository_root-valuememberdef-skel.ads \
  corba-repository_root-valuememberdef.adb \
  corba-repository_root-valuememberdef.ads \
  corba-repository_root-wstringdef-helper.adb \
  corba-repository_root-wstringdef-helper.ads \
  corba-repository_root-wstringdef-skel.adb \
  corba-repository_root-wstringdef-skel.ads \
  corba-repository_root-wstringdef.adb \
  corba-repository_root-wstringdef.ads \
  corba-repository_root.ads


ir_dir = server client print

PROGS =	@TOOLS_DIR@

$(GEN): $(srcdir)/ir.idl $(srcdir)/typecode.sh $(IDLAC)
	$(IDLAC) $(srcdir)/ir.idl
	$(SH) $(srcdir)/typecode.sh $(SED) $(RM) $(GEN)

CLEANFILES=	*.o *.ali
DISTCLEANFILES=	client server print ${GEN}

server: server.adb ${IMPL} ${GEN} force
	${GNATMAKE_FOR_TARGET} $@ ${GNATMAKE_FLAGS}

client: client.adb ${IMPL} ${GEN} force
	${GNATMAKE_FOR_TARGET} $@ ${GNATMAKE_FLAGS} 

print: print.adb ${IMPL} ${GEN} force
	${GNATMAKE_FOR_TARGET} $@ ${GNATMAKE_FLAGS} 

