GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatoa -fstack-check
GNATMAKE = gnatmake
INCLUDE = -I../common_files/
CMD = ${GNATMAKE} ${GNATFLAGS} ${INCLUDE}

all: axil

axil: lexer parser FORCE
	${CMD} axil

parser: FORCE
	${CMD} parser

lexer: FORCE
	${CMD} lexer

nutils: FORCE
	${CMD} nutils

nodes: debug FORCE
	${CMD} nodes

debug: FORCE
	${CMD} debug

clean: FORCE
	-rm -f *.ali *.o
	-rm -f *~
	-rm -f b~axil.ad*

allclean: clean
	-rm -f axil

FORCE:
