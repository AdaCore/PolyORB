# ----------------------------------------
# --                                    --
# --       ----  ---     --  ----       --
# --       -      -     - -  -  -       --
# --       -      -    ----  -  -       --
# --       ----  ---  -   -  ----       --
# --                                    --
# ----------------------------------------
# --  CORBA                             --
# --  Interface for                     --
# --  Ada'95 distributed systems annex  --
# --  Objects                           --
# ----------------------------------------
# --  Copyright (c) 1999                --
# --  École nationale supérieure des    --
# --  télécommunications                --
# ----------------------------------------

# $Id: //depot/ciao/main/Makefile#17 $

EXEC    = ciao

GMFLAGS = -I. -I/usr/lib/asis
CARGS   = -gnatfao -g
LARGS   = -lasis

all:	${EXEC} allruntime

$(EXEC): force
	gnatmake ciao-driver.adb -o $(EXEC) $(GMFLAGS) -cargs $(CARGS) -largs $(LARGS)

allruntime: force
	gnatmake allruntime.adb $(GMFLAGS)  `adabroker-config --cflags` $(CARGS) -largs $(LARGS) `adabroker-config --libs`

force:
	true

clean:
	-rm -f *.ali *.o *.at? *~

distclean: clean
	-rm -f ${EXEC}
