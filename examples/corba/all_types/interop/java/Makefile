#######################################################################
# Generic Makefile to compile Java CORBA client/server using
#  - Blackdown Linux Java SDK 1.4.1
# support the following ORBs :
#  - OpenORB 1.3.0
#  - Jonathan 3.0
#
# $Id: //droopi/main/examples/corba/all_types/interop/java/Makefile#4 $

#######################################################################
# Shell setup

DEL   = rm -rf

#######################################################################
# Java SDK and RE
# (configured for Blackdown Java on Linux, v 1.4.1-01)

JAVA  = java
JAVAC = javac

CLASSPATH_FLAG = -classpath
CLASSPATH      = $(ORB_CLASSPATH):.:./..

COMPILE = $(JAVAC) $(CLASSPATH_FLAG) $(CLASSPATH) 
RUN     = $(JAVA)  $(CLASSPATH_FLAG) $(CLASSPATH) $(OPTIONS)

#######################################################################
# Configuration for OpenORB 1.3.0

OPENORB_JARPATH   = /home/hugues/local/OpenORB-1.3.0/lib
OPENORB           = $(OPENORB_JARPATH)/openorb-1.3.0.jar
OPENORB_TOOLS     = $(OPENORB_JARPATH)/openorb_tools-1.3.0.jar
LOGKIT            = $(OPENORB_JARPATH)/logkit.jar
AVALON            = $(OPENORB_JARPATH)/avalon-framework.jar

OPENORB_CLASSPATH = $(OPENORB):$(OPENORB_TOOLS):$(LOGKIT):$(AVALON):$(GENERATED_FILES)

OPENORB_OPTIONS   =

OPENORB_GENERATED_FILES = ./generated

OPENORB_IDL       = $(RUN) org.openorb.compiler.IdlCompiler

#######################################################################
# Configuration for Jonathan 3.0 alpha 10

JONATHAN_JARPATH = /home/hugues/local/Jonathan_3_0_a10/libs
JONATHAN         = $(JONATHAN_JARPATH)/Jonathan_3_0_a10.jar
KILIM            = $(JONATHAN_JARPATH)/kilim.jar
KILIM_TOOLS      = $(JONATHAN_JARPATH)/kilim-tools.jar
NANOXML          = $(JONATHAN_JARPATH)/nanoxml-lite-2.2.1.jar

JONATHAN_CLASSPATH =$(JONATHAN):$(KILIM):$(KILIM_TOOLS):$(NANOXML)

JONATHAN_OPTION = \
	-Dorg.omg.CORBA.ORBClass=org.objectweb.david.libs.binding.orbs.iiop.IIOPORB \
	-Dorg.omg.CORBA.ORBSingletonClass=org.objectweb.david.libs.binding.orbs.ORBSingletonClass \
	-Ddavid.naming.default_method=2 \
	-Ddavid.naming.default_file=ns.ior

JONATHAN_GENERATED_FILES = ./idl

JONATHAN_IDL = $(RUN) org.objectweb.david.tools.idlcompiler.Idl2Java -p idl -poa

#######################################################################
# Generic rules

.SUFFIXES:
.SUFFIXES:      .java .class  
.SUFFIXES:      .idl  

.java.class:
	$(COMPILE) $<

.idl:
	$(IDL) $<

#######################################################################
# Define the ORB for which this makefile is configured

# For OpenORB 1.3.0
#ORB_CLASSPATH   = $(OPENORB_CLASSPATH)
#ORB_OPTIONS     = $(OPENORB_OPTIONS)
#GENERATED_FILES = $(OPENORB_GENERATED_FILES)
#IDL             = $(OPENORB_IDL)

ORB_CLASSPATH   = $(JONATHAN_CLASSPATH)
ORB_OPTIONS     = $(JONATHAN_OPTIONS)
GENERATED_FILES = $(JONATHAN_GENERATED_FILES)
IDL             = $(JONATHAN_IDL)

#######################################################################

IDL_FILE    = all_types.idl
CLASS_FILES = DynSkeleton.class DynServer.class Server.class \
	DynClient.class Client.class

all: class idl

idl : $(IDL_FILE:.idl=)

class : idl $(CLASS_FILES:.java=.class)

run_server : Server.class
	$(RUN) Server

run_client : Client.class
	$(RUN) Client

run_dynserver : DynServer.class
	$(RUN) DynServer

run_dynclient : DynClient.class
	$(RUN) DynClient

clean:
	$(DEL) *~ *\# IOR
	$(DEL) *.class

veryclean: clean
	$(DEL) $(GENERATED_FILES)