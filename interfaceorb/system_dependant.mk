# the following flags are system-dependant,
# they must be set to compile a program which uses omiORB
# see omniORB's user's guide chapter one for information
# All the following lines must be commented but one.


INTERFACEORB_PATH = $(ADABROKER_PATH)/InterfaceORB
OMNIORB_INCLUDES = $(ADABROKER_PATH)/omniORB_2.7.0/include

CPP_LINK_FILES = $(INTERFACEORB_PATH)/Ada_Giop_c.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_Giop_s.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/omniObject_C2Ada.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_OmniObject.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_OmniRopeAndKey.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_netBufferedStream.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_memBufferedStream.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/proxyObjectFactory_C2Ada.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_Corba_Orb.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_Corba_Exceptions.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_exceptions.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_Iop.o
CPP_LINK_FILES += $(INTERFACEORB_PATH)/Ada_Corba_Boa.o


#linux
OMNIORB_SYSDEP_FLAGS = -D __x86__ -D __linux__ -D __OSVERSION__=2
OMNIORB_LIBS = -L$(ADABROKER_PATH)/omniORB_2.7.0/lib/i586_linux_2.0_glibc -lomniORB2 -lomniDynamic2 -lomnithread -lpthread -ltcpwrapGK

#Solaris
#OMNIORB_SYSDEP_FLAGS = -D __sparc__ -D __sunos__ -D __OSVERSION__=5
#OMNIORB_LIBS = -L$(ADABROKER_PATH)/omniORB_2.7.0/lib/sun4_sosV_5.5 -lomniORB2 -lomniDynamic2 -lomnithread -lpthread -lposix4 -mt -lsocket -lnsl -ltcpwrapGK -lstdc++

