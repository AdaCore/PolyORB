#######################################################################
# Shell setup

DEL = rm -f

#######################################################################
# C/C++ compiler to use

CXX = g++-2.96
LD  = g++-2.96

#######################################################################
# Setup for omniORB 4.0

#OMNIORB_PATH    = <path_to_omniORB_install_dir>
OMNIORB_INC     = $(OMNIORB_PATH)/include 
OMNIORB_LIBPATH = $(OMNIORB_PATH)/lib
OMNIORB_LIB     = -lomniORB4 -lomniDynamic4

# IDL Compiler setup
IDL = omniidl
IDL_FLAGS = -bcxx

#######################################################################
# Makefile configured for omniORB 4.0

CXXFLAGS = -c -Wall -I$(OMNIORB_INC) -g
LD_FLAGS = -g
LIBS = -L$(OMNIORB_LIBPATH) $(OMNIORB_LIB) -pthread

PROG= client server dynclient dynserver

all: $(PROG)

client: client.o all_typesSK.o
	$(LD) client.o all_typesSK.o $(LD_FLAGS) $(LIBS) -o client

server: server.o all_typesSK.o
	$(LD) server.o all_typesSK.o $(LD_FLAGS) $(LIBS) -o server

dynserver: dynserver.o
	$(LD) dynserver.o $(LD_FLAGS) $(LIBS) -o dynserver

dynclient: dynclient.o
	$(LD) dynclient.o $(LD_FLAGS) $(LIBS) -o dynclient

client.o: client.cc all_types.hh all_typesSK.cc
	$(CXX) $(CXXFLAGS) client.cc

server.o: all_types.hh all_typesSK.cc server.cc all_types_imp.cc
	$(CXX) $(CXXFLAGS) server.cc

all_typesSK.o: all_types.hh all_typesSK.cc
	$(CXX) $(CXXFLAGS) all_typesSK.cc

all_types.hh all_typesSK.cc: ../../all_types.idl
	$(IDL) $(IDL_FLAGS) ../../all_types.idl

clean:
	$(DEL) *.o
	$(DEL) $(PROG)

veryclean: clean
	$(DEL) all_types.hh all_typesSK.cc
