SHELL=/bin/sh
SOURCE = corba-repository_root-irobject-impl.adb \
corba-repository_root-contained-impl.adb \
corba-repository_root-container-impl.adb \
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






CC = gnatmake
FLAGS = -c -g -gnata -gnatf 
LIB = -I. -I/home/niebel/adabroker/example/generic -I/home/niebel/adabroker/src -I/home/niebel/adabroker/adasockets/src

first: all server client

all: ${SOURCE}
	for file in ${SOURCE}; do ${CC} ${FLAGS} ${LIB} $$file; done

%.ad?: ir.idl
	idlac ir.idl; typecode.sh

impl:
	idlac ir.idl; idlac -i ir.idl; typecode.sh

clean :
	rm -f *.ads *.adb *.o *.ali

server: server.adb ${SOURCE}
	${CC}  -g -gnata -gnatf  ${LIB} server.adb

client: client.adb ${SOURCE}
	${CC}  -g -gnata -gnatf  ${LIB} client.adb


