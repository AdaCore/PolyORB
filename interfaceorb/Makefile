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


main: C_files Ada_adb_files Ada_ads_files Sys_Dep


##########################
# compilation of C++ files
##########################
C_files : \
Ada_Corba_Exceptions.o Ada_Corba_Orb.o Ada_exceptions.o \
Ada_Giop_c.o Ada_Giop_s.o Ada_memBufferedStream.o Ada_netBufferedStream.o \
Ada_OmniObject.o Ada_OmniRopeAndKey.o omniObject_C2Ada.o \
proxyObjectFactory_C2Ada.o omniObject_C2Ada.o proxyObjectFactory_C2Ada.o  

.SUFFIXES: .cc .o

.cc.o:
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) $<

Ada_Corba_Exceptions.o : Ada_Corba_Exceptions.hh Ada_Corba_Exceptions.cc

Ada_Corba_Orb.o : Ada_Corba_Orb.hh Ada_Corba_Orb.cc

Ada_exceptions.o : Ada_exceptions.cc Ada_exceptions.hh exceptions.ads \
exceptions.adb

Ada_Giop_c.o : Ada_Giop_c.hh Ada_Giop_c.cc Ada_netBufferedStream.o

Ada_Giop_s.o : Ada_Giop_s.hh Ada_Giop_s.cc Ada_netBufferedStream.o

Ada_memBufferedStream.o : Ada_memBufferedStream.hh Ada_memBufferedStream.cc

Ada_netBufferedStream.o : Ada_netBufferedStream.hh Ada_netBufferedStream.cc

Ada_OmniObject.o : Ada_OmniObject.hh Ada_OmniObject.cc omniObject_C2Ada.o \
omniobject.ads omniobject.adb

Ada_OmniRopeAndKey.o : Ada_OmniRopeAndKey.hh Ada_OmniRopeAndKey.cc

omniObject_C2Ada.o : omniObject_C2Ada.hh omniObject_C2Ada.cc

proxyObjectFactory_C2Ada.o: proxyObjectFactory_C2Ada.hh \
proxyObjectFactory_C2Ada.cc omniObject_C2Ada.o Ada_OmniObject.o




##########################
# compilation of Ada files
##########################

# from adb files
################
Ada_adb_files: \
adabroker_debug.ali corba.ali corba-boa.ali corba-command_line.ali  \
corba-exceptions.ali corba-object.ali corba-orb.ali  \
corba-sequences-bounded.ali corba-sequences-unbounded.ali exceptions.ali \
giop.ali giop_c.ali giop_s.ali iop.ali membufferedstream.ali \
netbufferedstream.ali omni.ali omniobject.ali omniproxycalldesc.ali \
omniproxycallwrapper.ali omniropeandkey.ali rope.ali sys_dep.ali

.SUFFIXES: .adb .ali

.adb.ali:
	$(ADA_COMP) $(DO_NOT_LINK) $<


adabroker_debug.ali: adabroker_debug.ads adabroker_debug.adb

corba.ali: corba.ads corba.adb corba-exceptions.ali

corba-boa.ali : corba-boa.ads corba-boa.adb corba-object.ali omniobject.ali \
corba.ali

corba-command_line.ali : corba-command_line.ads corba-command_line.adb \
corba.ali

corba-exceptions.ali : corba-exceptions.ads corba-exceptions.adb \
corba.ali Ada_Corba_Exceptions.o omniobject.ali sys_dep.ali

corba-forward.ali: corba-forward.ads corba-forward.adb corba.ali

corba-object.ali : corba-object.ads corba-object.adb omniobject.ali \
omniropeandkey.ali netbufferedstream.ali membufferedstream.ali \
iop.ali corba.ali

corba-orb.ali : corba-orb.ads corba-orb.adb corba-object.ali \
corba-object.ali corba-boa.ali omniobject.ali corba-command_line.ali \
adabroker_debug.ali corba.ali

corba-sequences-bounded.ali: corba-sequences-bounded.ads \
corba-sequences-bounded.adb corba-sequences.ali corba.ali

corba-sequences-unbounded.ali: corba-sequences-unbounded.ads \
corba-sequences-unbounded.adb corba-sequences.ali corba.ali

exceptions.ali : exceptions.ads exceptions.adb

giop.ali : giop.ads giop.adb

giop_c.ali : giop_c.ads giop_c.adb Ada_Giop_c.o corba.ali giop.ali \
sys_dep.ali rope.ali netbufferedstream.ali

giop_s.ali : giop_s.ads giop_s.adb Ada_Giop_s.o corba.ali giop.ali \
sys_dep.ali netbufferedstream.ali

iop.ali : iop.ads iop.adb netbufferedstream.ali membufferedstream.ali \
corba.ali

membufferedstream.ali : membufferedstream.ads membufferedstream.adb \
corba.ali sys_dep.ali Ada_memBufferedStream.o

netbufferedstream.ali : netbufferedstream.ads netbufferedstream.adb \
corba.ali sys_dep.ali rope.ali Ada_netBufferedStream.o

omni.ali: omni.ads omni.adb corba.ali

omniobject.ali : omniobject.ads omniobject.adb corba.ali omni.ali \
membufferedstream.ali netbufferedstream.ali

omniproxycalldesc.ali: omniproxycalldesc.ads omniproxycalldesc.adb \
corba.ali giop_c.ali

omniproxycallwrapper.ali: omniproxycallwrapper.ads omniproxycallwrapper.adb \
omniproxycalldesc.ali corba-object.ali corba.ali omniobject.ali \
omniropeandkey.ali giop_c.ali netbufferedstream.ali giop.ali \
corba-exceptions.ali

omniropeandkey.ali : omniropeandkey.ads omniropeandkey.adb corba.ali \
rope.ali Ada_OmniRopeAndKey.o sys_dep.ali

rope.ali : rope.ads

sys_dep.ali : sys_dep.ads sys_dep.adb


# from ads files
################
Ada_ads_files: \
corba-sequences.ali omniowproxycalldesc.ali

.SUFFIXES: .ads .ali

.ads.ali:
	$(ADA_COMP) $(DO_NOT_LINK) $<


corba-sequences.ali : corba-sequences.ads
	$(ADA_COMP) $(DO_NOT_LINK) corba-sequences.ads

omniowproxycalldesc.ali: omniowproxycalldesc.ads corba.ali giop_c.ali



#######################################
# compilation of system dependant files
#######################################

Sys_Dep : \
Ada_Sys_Dep sys_dep.adb

Ada_Sys_Dep : Ada_Sys_Dep.cc
	$(CPP_COMP) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_Sys_Dep.cc \
-o Ada_Sys_Dep 

sys_dep.adb : sys_dep_before_preprocessor.adb
	$(ADA_PREP) sys_dep_before_preprocessor.adb sys_dep.adb def_file -u


##############################################################
clean :
	rm *.ali *.o *~
	rm Ada_Sys_Dep
	rm sys_dep.adb


toto: toto.o
	g++ -o toto -Wl,-rpath,../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
-L../omniORB_2.7.0/lib/i586_linux_2.0_glibc \
toto.o $(CPP_LINK_FILES) \
-lomniORB2 -lomniDynamic2 -lomnithread -lpthread -ltcpwrapGK






