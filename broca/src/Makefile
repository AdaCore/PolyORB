GNATMAKE=gnatmake
#ADASOCKETS_PATH=$(HOME)/devel/adasockets-0.1.3/src
ADASOCKETS_PATH=$(HOME)/adasockets-0.1.3/src
GNATMAKE_FLAGS=-I$(ADASOCKETS_PATH) -g -gnatf -gnatwu -gnatwl -gnatg

all:
	$(GNATMAKE) -c $(GNATMAKE_FLAGS) allsrc

clean:
	$(RM) -f *.o *.ali *~ allsrc *.c *.aux *.dvi *.log
