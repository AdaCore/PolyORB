GNATFLAGS = -g -O2 -gnatfy -gnatwae -gnatoa -fstack-check
GNATMAKE = gnatmake
INCLUDE = -I../lexer_files/
CMD = ${GNATMAKE} ${GNATFLAGS} ${INCLUDE}

all: aadl

aadl: lexer
	${CMD} aadl

lexer:
	${CMD} lexer

clean:
	-rm -f *.ali *.o
	-rm -f *~

realclean: clean
	-rm -f aadl
