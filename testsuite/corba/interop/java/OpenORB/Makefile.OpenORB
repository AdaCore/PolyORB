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
CLASSPATH      = $(ORB_CLASSPATH):.:./..:./../common
OPTIONS        = -d .

COMPILE = $(JAVAC) $(CLASSPATH_FLAG) $(CLASSPATH) $(OPTIONS)
RUN     = $(JAVA)  $(CLASSPATH_FLAG) $(CLASSPATH) $(ORB_OPTIONS)

#######################################################################
# Configuration for OpenORB 1.3.0

#OPENORB_JARPATH = <path_to_OpenORB_library>
OPENORB_JARPATH = $HOME/local/OpenORB-1.3.0/lib
OPENORB           = $(OPENORB_JARPATH)/openorb-1.3.0.jar
OPENORB_TOOLS     = $(OPENORB_JARPATH)/openorb_tools-1.3.0.jar
LOGKIT            = $(OPENORB_JARPATH)/logkit.jar
AVALON            = $(OPENORB_JARPATH)/avalon-framework.jar

OPENORB_CLASSPATH = $(OPENORB):$(OPENORB_TOOLS):$(LOGKIT):$(AVALON):$(GENERATED_FILES)

OPENORB_OPTIONS   =

OPENORB_GENERATED_FILES = ./generated

OPENORB_IDL       = $(RUN) org.openorb.compiler.IdlCompiler

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
ORB_CLASSPATH   = $(OPENORB_CLASSPATH)
ORB_OPTIONS     = $(OPENORB_OPTIONS)
GENERATED_FILES = $(OPENORB_GENERATED_FILES)
IDL             = $(OPENORB_IDL)

#######################################################################

CLASS_FILES = DynSkeleton.class DynServer.class \
	DynClient.class

all: class

idl : $(IDL_FILE:.idl=)

DynClient.class: ../common/DynClient.java
		 $(COMPILE) ../common/DynClient.java

DynServer.class: ../common/DynServer.java
		 $(COMPILE) ../common/DynServer.java

DynSkeleton.class: ../common/DynSkeleton.java
		 $(COMPILE) ../common/DynSkeleton.java

class : $(CLASS_FILES:.java=.class)

run_dynserver : DynServer.class
	$(RUN) DynServer

run_dynclient : DynClient.class
	$(RUN) DynClient

clean:
	$(DEL) *~ *\# IOR
	$(DEL) *.class

veryclean: clean
	$(DEL) $(GENERATED_FILES)
