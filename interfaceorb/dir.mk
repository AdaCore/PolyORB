CXXSRCS       = \
		Ada_Corba_Exceptions.cc \
		Ada_Corba_Orb.cc \
		Ada_Corba_Boa.cc \
		Ada_exceptions.cc \
		Ada_Giop_c.cc \
		Ada_Giop_s.cc \
		Ada_Iop.cc \
		Ada_memBufferedStream.cc \
		Ada_netBufferedStream.cc \
		Ada_OmniObject.cc \
		Ada_OmniRopeAndKey.cc \
		omniObject_C2Ada.cc \
		proxyObjectFactory_C2Ada.cc \
		omniObject_C2Ada.cc \
		proxyObjectFactory_C2Ada.cc  

CXXOBJS       = $(CXXSRCS:.cc=.o)

ADASSRCS      = \
		adabroker-constants.ads \
		adabroker-debug.ads \
		adabroker-exceptions.ads \
		adabroker-giop.ads \
		adabroker-giop_c.ads \
		adabroker-giop_s.ads \
		adabroker-iop.ads \
		adabroker-key.ads \
		adabroker-membufferedstream.ads \
		adabroker-netbufferedstream.ads \
		adabroker-omni.ads \
		adabroker-omniorb.ads \
		adabroker-omniproxycalldesc.ads \
		adabroker-omniproxycallwrapper.ads \
		adabroker-omniropeandkey.ads \
		adabroker-rope.ads \
		adabroker-sysdep.ads \
		adabroker.ads \
		corba-boa.ads \
		corba-command_line.ads \
		corba-context.ads \
		corba-forward.ads \
		corba-nvlist.ads \
		corba-object-omniorb.ads \
		corba-object.ads \
		corba-orb-omniorb.ads \
		corba-orb.ads \
		corba-request.ads \
		corba-sequences-bounded.ads \
		corba-sequences-unbounded.ads \
		corba-sequences.ads \
		corba.ads

ADABSRCS      = \
		corba-forward.adb \
		corba-sequences-bounded.adb \
		corba-sequences-unbounded.adb

ADAOBJS = $(ADASSRCS:.ads=.o)
ADAALIS = $(ADASSRCS:.ads=.ali)

DIR_CPPFLAGS = -I. -I$(TOP)/include
DIR_CPPFLAGS += $(CORBA_CPPFLAGS) 
DIR_CPPFLAGS += -g

lib = $(patsubst %,$(LibPattern),adabroker)

all:: previous_check $(lib)


$(lib): $(CXXOBJS) $(ADAOBJS)
	@$(StaticLinkLibrary)

clean::
	-rm -f *.ali *.o Ada_Sys_Dep adabroker-sysdep.ads *~ libadabroker.a

previous_check: Ada_Sys_Dep
	@if [ `./Ada_Sys_Dep` ]; then \
	   echo "size of long has to be 4"; \
	   exit 1; \
	fi

Ada_Sys_Dep: Ada_Sys_Dep.cc
	$(CXX) $(DIR_CPPFLAGS) Ada_Sys_Dep.cc -o Ada_Sys_Dep 


$(ADAOBJS): adabroker-sysdep.ads
	gnatmake -g -gnata -i all_adabroker.ads -gnatg

adabroker-sysdep.ads : adabroker-sysdep.ads.in
	sed -f $(ADABROKER_PLATFORM)/$(platform) \
	  adabroker-sysdep.ads.in > adabroker-sysdep.ads

export:: $(lib)
	@$(ExportLibrary)
	@(dir=$(EXPORT_TREE)/$(LIBDIR); \
	 for file in $(ADASSRCS) $(ADABSRCS); do \
	   $(ExportFileToDir); \
	 done; \
	 for file in $(ADAALIS); do \
	   $(ExportFileToDir); \
	 done; \
	)
