GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatoa -fstack-check
GNATMAKE = gnatmake
INCLUDE = -I../common_files/
IAC_PATH = ../iac/
CMD = ${GNATMAKE} ${GNATFLAGS} ${INCLUDE}
NODESTEMP = nodes.tmp

all: axil

axil: lexer parser
	${CMD} axil

parser: nodes errormsg
	${CMD} parser

errormsg:
	${CMD} errormsg

lexer:
	${CMD} lexer

nutils:
	${CMD} nutils

mknodes: ../common_files/mknodes.adb
	$(GNATMAKE) -I. ../common_files/mknodes.adb

nodes: nodes.ads nodes.adb debug
	${CMD} nodes

nodes.ads nodes.adb : nodes.idl
	$(IAC_PATH)/mknodes nodes.idl > $(NODESTEMP)
	-gnatchop -w ${NODESTEMP}
	-rm -f ${NODESTEMP}

debug:
	${CMD} debug

clean:
	-rm -f *.ali *.o
	-rm -f *~
	-rm -f b~axil.ad*
	-rm -f *.tmp

allclean: clean
	-rm -f axil mknodes
	-rm -f nodes.ads
	-rm -f nodes.adb
