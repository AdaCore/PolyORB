#######################################################################
# Shell setup

DEL   = rm -rf

#######################################################################
# Java SDK and RE
# (configured for Blackdown Java on Linux, v 1.4.1-01)

JAVA  = java
JAVAC = javac

CLASSPATH_FLAG = -classpath
CLASSPATH      = $(ORB_CLASSPATH):.

COMPILE = $(JAVAC)  $(CLASSPATH_FLAG) $(CLASSPATH)
RUN     = $(JAVA) $(CLASSPATH_FLAG) $(CLASSPATH)

#######################################################################
# Configuration for OpenORB 1.3.0

OPENORB_JARPATH = /home/hugues/local/OpenORB-1.3.0/lib
OPENORB         = $(OPENORB_JARPATH)/openorb-1.3.0.jar
OPENORB_TOOLS   = $(OPENORB_JARPATH)/openorb_tools-1.3.0.jar
LOGKIT          = $(OPENORB_JARPATH)/logkit.jar
AVALON          = $(OPENORB_JARPATH)/avalon-framework.jar

GENERATED_FILES = ./generated

OPENORB_CLASSPATH = $(OPENORB):$(OPENORB_TOOLS):$(LOGKIT):$(AVALON):$(GENERATED_FILES)

IDL             = $(RUN) org.openorb.compiler.IdlCompiler

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
# Makefile configured for OpenORB 1.3.0

ORB_CLASSPATH = $(OPENORB_CLASSPATH)

#######################################################################

IDL_FILE    = ../../all_types.idl
CLASS_FILES = DynSkeleton.class DynServer.class DynClient.class

all: class idl

idl : $(IDL_FILE:.idl=)

class : idl $(CLASS_FILES:.java=.class)

run_client : Client.class
	$(RUN) Client

run_dynserver : DynServer.class
	$(RUN) DynServer

run_dynclient : DynClient.class
	$(RUN) DynClient

clean:
	$(DEL) *~ IOR
	$(DEL) *.class

veryclean: clean
	$(DEL) generated