GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatoa -fstack-check
GNATMAKE = gnatmake
INCLUDE = -I../common_files/
CMD = ${GNATMAKE} ${GNATFLAGS} ${INCLUDE}
NODESTEMP = nodes.tmp

all: axil

axil: lexer parser
	${CMD} axil

parser: nodes
	${CMD} parser

lexer:
	${CMD} lexer

nutils:
	${CMD} nutils

createnodes:
	-../common_files/mknodes nodes.idl > ${NODESTEMP}
	-gnatchop -w ${NODESTEMP}
	-rm -f ${NODESTEMP}

nodes: createnodes debug
	${CMD} nodes

debug:
	${CMD} debug

clean:
	-rm -f *.ali *.o
	-rm -f *~
	-rm -f b~axil.ad*
	-rm -f *.tmp
	-rm -f nodes.ads
	-rm -f nodes.adb

realclean: clean
	-rm -f axil
