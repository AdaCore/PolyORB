SHELL           = /bin/sh
GNATFLAGS       = -g -gnatwA  
FLAGS           = -I. 


all : souches 

force:

souches: 
	rm -f *ir_info*
	gnatmake $(GNATFLAGS) *.adb $(FLAGS) `polyorb-config`


clean:
	@-rm -f *.ali *.o
	@-rm -f GNAT* *~
	@-rm -f core*
	@-rm -f b~*

