SHELL           = /bin/sh
GNATFLAGS       = -g -gnatwA  
GNATINCLUDE     = -I`gnatls -v | grep adainclude | sed 's/ *//g'`  
FLAGS           = -I. $(GNATINCLUDE)


all : stubs 

force:

stubs: 
	gnatmake $(GNATFLAGS) *.adb $(FLAGS) `polyorb-config`


clean:
	@-rm -f *.ali *.o
	@-rm -f GNAT* *~
	@-rm -f core*
	@-rm -f b~*

