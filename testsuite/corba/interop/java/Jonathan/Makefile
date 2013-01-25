#######################################################################
# $Id$

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
OPTIONS        = -d .

COMPILE = $(JAVAC) $(CLASSPATH_FLAG) $(CLASSPATH) $(OPTIONS)
RUN     = $(JAVA)  $(CLASSPATH_FLAG) $(CLASSPATH) $(ORB_OPTIONS)

#######################################################################
# Configuration for Jonathan 3.0 alpha 10

#JONATHAN_JARPATH = <path_to_Jonathan_library>
JONATHAN_JARPATH = $HOME/local/Jonathan_3_0_a10/libs

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

ORB_CLASSPATH   = $(JONATHAN_CLASSPATH)
ORB_OPTIONS     = $(JONATHAN_OPTIONS)
GENERATED_FILES = $(JONATHAN_GENERATED_FILES)
IDL             = $(JONATHAN_IDL)

#######################################################################

IDL_FILE    = all_types.idl
CLASS_FILES = DynSkeleton.class DynServer.class \
	DynClient.class

all: class

idl : $(IDL_FILE:.idl=)
class : $(CLASS_FILES:.java=.class)
DynClient.class: ../common/DynClient.java
		 $(COMPILE) ../common/DynClient.java

DynServer.class: ../common/DynServer.java
		 $(COMPILE) ../common/DynServer.java

DynSkeleton.class: ../common/DynSkeleton.java
		 $(COMPILE) ../common/DynSkeleton.java

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
