SHELL=/bin/sh
SOURCE = corba-repository_root-container-impl.adb \
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


DEP = corba-repository_root-irobject.adb \
corba-repository_root-contained.adb \
corba-repository_root-container.adb \
corba-repository_root-moduledef.adb \
corba-repository_root-interfacedef.adb \
corba-repository_root-valuedef.adb \
corba-repository_root-exceptiondef.adb \
corba-repository_root-typedefdef.adb \
corba-repository_root-uniondef.adb \
corba-repository_root-structdef.adb \
corba-repository_root-repository.adb \
corba-repository_root-idltype.adb \
corba-repository_root-primitivedef.adb \
corba-repository_root-stringdef.adb \
corba-repository_root-wstringdef.adb \
corba-repository_root-sequencedef.adb \
corba-repository_root-fixeddef.adb \
corba-repository_root-arraydef.adb \
corba-repository_root-enumdef.adb \
corba-repository_root-aliasdef.adb \
corba-repository_root-constantdef.adb \
corba-repository_root-attributedef.adb \
corba-repository_root-operationdef.adb \
corba-repository_root-valuememberdef.adb \
corba-repository_root-nativedef.adb 


CC = gnatmake
FLAGS = -c -g -gnata -gnatf 
LIB = -I. -I/home/niebel/adabroker/example/generic -I/home/niebel/adabroker/src -I/home/niebel/adabroker/adasockets/src

first: all server client print

all: ${SOURCE} ${DEP}
	for file in ${SOURCE}; do ${CC} ${FLAGS} ${LIB} $$file; done

%.adb: ir.idl
	idlac ir.idl; typecode.sh

impl:
	idlac ir.idl; idlac -i ir.idl; typecode.sh

clean :
	rm -f *.o *.ali client server

server: server.adb ${SOURCE}
	${CC}  -g -gnata -gnatf  ${LIB} server.adb

client: client.adb ${SOURCE}
	${CC}  -g -gnata -gnatf  ${LIB} client.adb

print: print.adb ${SOURCE}
	${CC}  -g -gnata -gnatf  ${LIB} print.adb



