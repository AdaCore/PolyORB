ADB =test_client_server.adb  
OADB=test_client_server_ork.adb  
EXE= $(patsubst %.adb,%, $(ADB))
ORK_EXE= $(patsubst %.adb,%.ork, $(OADB))


#srcdir= path/to/polyorb/src
#ork_srcdir= path/to/polyorb_compiled_for_ORK/src

#	Need to precise the PolyORB src directory path (put it in srcdir) 
#	ork_srcdir is used if you want to compile for ORK, compile PolyORB
#	for ORK platforms and initialize this variable with the src 
#	directory of PolyORB compiled for ORK


giopdir=$(srcdir)/giop
corbadir=$(srcdir)/corba

ork_giopdir=$(ork_srcdir)/giop
ork_corbadir=$(ork_srcdir)/corba

GNATFLAGS = -g -O2 -gnatfy -gnatoa -fstack-check -gnatwa #-a

GNATMAKE = gnatmake
ORK_GNATMAKE = i386-ork-gnatmake

INCLUDE = -I../protocol -I$(srcdir) -I$(giopdir) -I$(corbadir) \
-L$(srcdir)/.libs -L$(giopdir)/.libs -L$(corbadir)/.libs

ORK_INCLUDE =  -I../protocol \
-I$(ork_srcdir) -I$(ork_giopdir)  -I$(ork_corbadir) \
-L$(ork_srcdir)/.libs -L$(ork_giopdir)/.libs -L$(ork_corbadir)/.libs


LARGS=
ORK_LARGS = -largs -k -specs ork_specs 

CMD = ${GNATMAKE} ${GNATFLAGS} ${INCLUDE}


all: #compile ork_compile
	@-echo "make compile     : Compile for Linux"
	@-echo "make ork_compile : Compile for ORK"	

compile :${ADB} echo-skel.ads
	${CMD} ${ADB} ${LARGS}

ork_compile : echo-skel.ads ${ORK_EXE} 

%.ork:%.adb
	${ORK_GNATMAKE} -o $@ $< ${GNATFLAGS} ${ORK_INCLUDE} ${ORK_LARGS} 

echo-skel.ads : echo.idl
	      idlac echo.idl

force:	
	
clean:
	@-rm -f *.ali *.o *~ b~* *.gz *helper* *skel* *echo.ad* GNAT*

distclean: clean
	@-rm -f $(EXE) ${ORK_EXE}
