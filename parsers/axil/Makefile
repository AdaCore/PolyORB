GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatoa -fstack-check
GNATMAKE = gnatmake
INCLUDE = -I../common_files/
CMD = ${GNATMAKE} ${GNATFLAGS} ${INCLUDE}

all: axil

axil: lexer parser
	${CMD} axil

parser:
	${CMD} parser

lexer:
	${CMD} lexer

nutils:
	${CMD} nutils

nodes: debug
	${CMD} nodes

debug:
	${CMD} debug

clean:
	-rm -f *.ali *.o
	-rm -f *~
	-rm -f b~axil.ad*

realclean: clean
	-rm -f axil
