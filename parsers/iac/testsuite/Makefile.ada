SHELL           = /bin/sh
GNATFLAGS       = -g -gnatwA  
FLAGS           = -I. 


all : stubs 

force:

stubs: 
	gnatmake $(GNATFLAGS) *.adb $(FLAGS) `polyorb-config`


clean:
	@-rm -f *.ali *.o
	@-rm -f GNAT* *~
	@-rm -f core*
	@-rm -f b~*

