GNATMAKE=gnatmake
ADASOCKETS_PATH=/home/gingold/src/adasockets-0.1.3/src
GNATMAKE_FLAGS=-I$(ADASOCKETS_PATH) -g -gnatf -gnatwu -gnatwl -gnatg
#BIND_FLAGS=-largs gccchecker.o
#BIND_FLAGS=-largs free.o
#BIND_FLAGS=-largs -lefence
CFLAGS=-g

ALL_FLAGS = GNATMAKE="$(GNATMAKE)" GNATMAKE_FLAGS="$(GNATMAKE_FLAGS)"

SUBDIRS=src examples/poa doc

all:
	HERE=$$PWD; \
	for i in $(SUBDIRS); do \
	  cd $$i; $(MAKE) all $(ALL_FLAGS) || exit 1; cd $$HERE; \
	done

clean:
	for i in $(SUBDIRS); do $(MAKE) -C $$i clean $(ALL_FLAGS); done

dist:
	tar zcvf broca.tgz $(ALL_SOURCES)

