###################################################################
##                    compilers                                  ##
###################################################################
##                                                               ##

# comment this line to remove debugging code in the executable
ADA_DEBUG = -gnata

# Ada compiler
# as for now, it only works with gnat
ADA_COMP = gnatgcc -gnatf $(ADA_DEBUG) 

# Ada preprocessor
ADA_PREP = gnatprep

# C++ compiler
CPP_COMP = g++

# compilation flag
DO_NOT_LINK = -c


##                                                               ##
###################################################################


###################################################################
##              Dependance on omniORB
###################################################################
##                                                               ##

# write here a path to omniORB :
OMNIORB_PATH = ../omniORB_2.7.0

# the following flags are system-dependant,
# they must be set to compile a program which uses omiORB
# see omniORB's user's guide chapter one for information

#linux
OMNIORB_SYSDEP_FLAGS = -D __x86__ -D __linux__ -D __OSVERSION__=2

# paths to omniORB's libraries
OMNIORB_LIBS = -I $(OMNIORB_PATH)/include/
OMNIORB_LIBS += -I $(OMNIORB_PATH)/src/lib/omniORB2/
OMNIORB_LIBS += -I $(OMNIORB_PATH)/src/lib/omniORB2/orbcore/

##                                                               ##
###################################################################


main: c2ada adaonly


c2ada : Ada_Sys_Dep sys_dep.o Ada_Rope.o rope.o Ada_OmniRopeAndKey.o \
omniropeandkey.o omniobjectmanager.o omniObject_C2Ada.o Ada_OmniObject.o \
omniobject.o giop.o Ada_Giop_s.o giop_s.o Ada_Giop_c.o giop_c.o \
Ada_memBufferedStream.o membufferedstream.o Ada_netBufferedStream.o \
netbufferedstream.o corba-object.o corba-boa.o membufferedstream.o \
iop.o proxyObjectFactory_C2Ada.o  corba-orb.o corba-exceptions.o \
exceptions.o
#omni_C2Ada.o omni.o 


Ada_Sys_Dep : Ada_Sys_Dep.cc
	$(CPP_COMP) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_Sys_Dep.cc -o Ada_Sys_Dep
	Ada_Sys_Dep

sys_dep.o : sys_dep.ads sys_dep.adb
	$(ADA_COMP) $(DO_NOT_LINK) sys_dep.adb

sys_dep.adb : sys_dep_before_preprocessor.adb
	$(ADA_PREP) sys_dep_before_preprocessor.adb sys_dep.adb def_file -u

Ada_Rope.o : Ada_Rope.cc Ada_Rope.hh
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_Rope.cc

rope.o : rope.ads
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) rope.ads

Ada_OmniRopeAndKey.o : Ada_OmniRopeAndKey.hh Ada_OmniRopeAndKey.cc
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_OmniRopeAndKey.cc

omniropeandkey.o : omniropeandkey.ads omniropeandkey.adb corba.ali rope.ali
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) omniropeandkey.adb

omniobjectmanager.o : omniobjectmanager.ads omniobjectmanager.adb giop_s.ali \
sys_dep.ali iop.ali rope.ali omniropeandkey.ali netbufferedstream.ali \
membufferedstream.ali
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) omniobjectmanager.adb

omniObject_C2Ada.o : omniObject_C2Ada.hh omniObject_C2Ada.cc
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) omniObject_C2Ada.cc

Ada_OmniObject.o : Ada_OmniObject.hh Ada_OmniObject.cc
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_OmniObject.cc

omniobject.o : omniobject.ads omniobject.adb corba.ali omni.ali 
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) omniobject.adb

giop.o : giop.ads giop.adb
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) giop.adb

Ada_Giop_s.o : Ada_Giop_s.hh Ada_Giop_s.cc
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_Giop_s.cc

giop_s.o : giop_s.ads giop_s.adb corba.ali giop.ali sys_dep.ali
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) giop_s.adb

Ada_Giop_c.o : Ada_Giop_c.hh Ada_Giop_c.cc
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_Giop_c.cc

giop_c.o : giop_c.ads giop_c.adb Ada_Giop_c.o corba.ali giop.ali sys_dep.ali rope.ali
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) giop_c.adb

Ada_netBufferedStream.o : Ada_netBufferedStream.hh Ada_netBufferedStream.cc
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_netBufferedStream.cc

netbufferedstream.o : netbufferedstream.ads netbufferedstream.adb corba.ali sys_dep.ali rope.ali
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) netbufferedstream.adb

Ada_memBufferedStream.o : Ada_memBufferedStream.hh Ada_memBufferedStream.cc
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_memBufferedStream.cc

membufferedstream.o : membufferedstream.ads membufferedstream.adb corba.ali sys_dep.ali
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) membufferedstream.adb

corba-object.o : corba-object.ads corba-object.adb omniobject.o omniropeandkey.o \
netbufferedstream.o membufferedstream.o iop.o
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) corba-object.adb

corba-boa.o : corba-boa.ads corba-boa.adb corba-object.ali omniobject.ali
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) corba-boa.adb

corba-orb.o: corba-orb.ads corba-orb.adb corba-object.ali corba-object.o corba-boa.o \
omniobject.o corba-command_line.ali adabroker_debug.ali
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) corba-orb.adb

iop.o : iop.ads iop.adb netbufferedstream.ali membufferedstream.ali corba.ali
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) iop.adb

proxyObjectFactory_C2Ada.o: proxyObjectFactory_C2Ada.hh proxyObjectFactory_C2Ada.cc
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) proxyObjectFactory_C2Ada.cc

#omni_C2Ada.o: omni_C2Ada.hh omni_C2Ada.cc
#	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) omni_C2Ada.cc


Ada_exceptions.o : Ada_exceptions.cc Ada_exceptions.hh
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_exceptions.cc

exceptions.o : exceptions.ads exceptions.adb
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) exceptions.adb

corba-exceptions.o : corba-exceptions.ads corba-exceptions.adb
	$(ADA_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) corba-exceptions.adb


################################################
# compilation of files that do not depend on C++
################################################
adaonly: corba.ali corba-object.ali giop_s.ali giop.ali omni.ali \
netbufferedstream.ali corba-command_line.ali  \
omniproxycalldesc.ali omniproxycallwrapper.ali \
adabroker_debug.ali corba-sequences.o corba-sequences-bounded.ali \
corba-sequences-unbounded.ali

.SUFFIXES: .adb .ali

.adb.ali:
	$(ADA_COMP) $(DO_NOT_LINK) $<

corba.ali: corba.ads corba.adb

corba-object.ali: corba-object.ads corba-object.adb omniobject.ali corba.ali

giop.ali: giop.ads giop.o

omni.ali: omni.ads omni.adb omniobject.ali corba.ali

netbufferedstream.ali: netbufferedstream.ads netbufferedstream.adb

membufferedstream.ali: membufferedstream.ads membufferedstream.adb

omniproxycalldesc.ali: omniproxycalldesc.ads omniproxycalldesc.adb corba.ali giop_c.ali

omniproxycallwrapper.ali: omniproxycallwrapper.ads omniproxycallwrapper.adb \
omniproxycalldesc.ali corba-object.ali corba.ali corba-object.ali \
omniropeandkey.ali giop_c.ali netbufferedstream.ali 

adabroker_debug.ali: adabroker_debug.ads adabroker_debug.adb

corba-forward.ali: corba-forward.ads corba-forward.adb
# not implemented yet, does not compile

corba-sequences.o: corba-sequences.ads
	$(ADA_COMP) $(DO_NOT_LINK) corba-sequences.ads


corba-sequences-bounded.ali: corba-sequences-bounded.ads corba-sequences-bounded.adb \
corba-sequences.o

corba-sequences-unbounded.ali: corba-sequences-unbounded.ads \
corba-sequences-unbounded.adb corba-sequences.o


##############################################################
clean :
	rm *.ali *.o *~





