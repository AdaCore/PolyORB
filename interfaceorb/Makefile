###################################################################
##                    compilers                                  ##
###################################################################
##                                                               ##

# comment this line to remove debugging code in the executable
ADA_DEBUG = -gnata


# Ada preprocessor
ADA_PREP = gnatprep

# Ada make
GNATMAKE = gnatmake -gnatf $(ADA_DEBUG)

# C++ compiler
CPP_COMP = g++ -g -Wall -ansi -pedantic

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

#This file should contain system dependant variables to
# compile omniORB2 programs
include system_dependant.mk

# paths to omniORB's libraries
OMNIORB_LIBS = -I $(OMNIORB_PATH)/include/
OMNIORB_LIBS += -I $(OMNIORB_PATH)/src/lib/omniORB2/
OMNIORB_LIBS += -I $(OMNIORB_PATH)/src/lib/omniORB2/orbcore/

##                                                               ##
###################################################################


main: C_files #Sys_Dep
	$(GNATMAKE) adabroker.ads

##########################
# compilation of C++ files
##########################
C_files : \
Ada_Corba_Exceptions.o Ada_Corba_Orb.o Ada_Corba_Boa.o Ada_exceptions.o \
Ada_Giop_c.o Ada_Giop_s.o Ada_Iop.o Ada_memBufferedStream.o \
Ada_netBufferedStream.o Ada_OmniObject.o Ada_OmniRopeAndKey.o \
omniObject_C2Ada.o proxyObjectFactory_C2Ada.o omniObject_C2Ada.o \
proxyObjectFactory_C2Ada.o  

.SUFFIXES: .cc .o

.cc.o:
	$(CPP_COMP) $(DO_NOT_LINK) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) $<

Ada_Corba_Exceptions.o : Ada_Corba_Exceptions.hh Ada_Corba_Exceptions.cc \
Ada_exceptions.hh Ada_OmniObject.hh

Ada_Corba_Boa.o : Ada_Corba_Boa.hh Ada_Corba_Boa.cc Ada_exceptions.hh

Ada_Corba_Orb.o : Ada_Corba_Orb.hh Ada_Corba_Orb.cc Ada_exceptions.hh

Ada_exceptions.o : Ada_exceptions.cc Ada_exceptions.hh

Ada_Giop_c.o : Ada_Giop_c.hh Ada_Giop_c.cc Ada_netBufferedStream.hh \
Ada_exceptions.hh

Ada_Giop_s.o : Ada_Giop_s.hh Ada_Giop_s.cc Ada_netBufferedStream.hh \
Ada_exceptions.hh

Ada_Iop.o : Ada_Iop.hh Ada_Iop.cc Ada_exceptions.hh Ada_netBufferedStream.hh \
Ada_memBufferedStream.hh

Ada_memBufferedStream.o : Ada_memBufferedStream.hh Ada_memBufferedStream.cc \
Ada_exceptions.hh

Ada_netBufferedStream.o : Ada_netBufferedStream.hh Ada_netBufferedStream.cc \
Ada_exceptions.hh

Ada_OmniObject.o : Ada_OmniObject.hh Ada_OmniObject.cc omniObject_C2Ada.hh \
 Ada_OmniRopeAndKey.hh Ada_exceptions.hh

Ada_OmniRopeAndKey.o : Ada_OmniRopeAndKey.hh Ada_OmniRopeAndKey.cc \
Ada_exceptions.hh

omniObject_C2Ada.o : omniObject_C2Ada.hh omniObject_C2Ada.cc

proxyObjectFactory_C2Ada.o: proxyObjectFactory_C2Ada.hh \
proxyObjectFactory_C2Ada.cc omniObject_C2Ada.hh Ada_OmniObject.hh \
Ada_exceptions.hh



#######################################
# compilation of system dependant files
#######################################

Sys_Dep : Ada_Sys_Dep sys_dep.o
	./Ada_Sys_Dep

Ada_Sys_Dep : Ada_Sys_Dep.cc
	$(CPP_COMP) $(OMNIORB_LIBS) $(OMNIORB_SYSDEP_FLAGS) Ada_Sys_Dep.cc \
-o Ada_Sys_Dep 

sys_dep.o: sys_dep.ads sys_dep.adb
	$(GNATMAKE) sys_dep.adb

sys_dep.ads : sys_dep_before_preprocessor.ads sys_dep_definition_file.txt
	$(ADA_PREP) sys_dep_before_preprocessor.ads sys_dep.ads \
sys_dep_definition_file.txt -u


##############################################################
clean :
	rm *.ali *.o *~ Ada_Sys_Dep sys_dep.ads

