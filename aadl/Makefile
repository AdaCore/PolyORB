GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatoa -fstack-check
GNATMAKE = gnatmake
CMD = ${GNATMAKE} ${GNATFLAGS}

all: aadl

aadl: lexer
	${CMD} aadl

lexer:
	gnatmake lexer

clean:
	-rm -f *.ali *.o
	-rm -f *~

realclean: clean
	-rm -f aadl
