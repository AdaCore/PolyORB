#
# java.mk - platform-independent make variables and rules for Java.
#

#
# Exported .jar files are put in the "jar" directory of an import/export tree.
#

JARDIR = jar
IMPORT_JAR_DIRS = $(patsubst %,%/$(JARDIR),$(IMPORT_TREES))

#
# CORBA Java variables.
#

CORBA_JAVA_STUB_DIR    = $(TOP)/java_stub
CORBA_STUB_JAR_PATTERN = $(CORBA_JAVA_STUB_DIR)/%.jar
CORBA_STUB_JARS        = $(CORBA_UNIQUE_INTERFACES:%=$(CORBA_STUB_JAR_PATTERN))
CORBA_JAVA_STUB_FILES  = $(CORBA_STUB_JARS) \
		      $(CORBA_UNIQUE_INTERFACES:%=$(CORBA_JAVA_STUB_DIR)/%.idl)

#
# CleanJavaClassFiles removes all .class files from this directory and its
# subdirectories.  Each dir.mk which builds Java code should have a "clean"
# rule which does this.
#

define CleanJavaClassFiles
 classfiles=`find . -name '*.class' -print`;	\
 if [ "$$classfiles" != "" ]; then		\
   echo "Removing .class files";		\
   $(RM) $$classfiles;				\
 fi
endef

#
# CleanJavaStubFiles removes all java stub files which may have been generated
# from this directory.  This is used in the rule for "veryclean".
#

define CleanJavaStubFiles
 echo $(RM) $(CORBA_JAVA_STUB_FILES);		\
 $(RM) $(CORBA_JAVA_STUB_FILES);		\
 for dir in $(CORBA_JAVA_STUB_DIR)/*; do	\
   if [ -d $$dir ]; then			\
     echo $(RMDIRHIER) $$dir;			\
     $(RMDIRHIER) $$dir;			\
   fi;						\
 done;						\
 $(CleanJavaClassFiles);
endef

#
# CompileJavaSourcesToJar is the main Java compilation rule.  It compiles a
# list of .java files and creates a .jar file containing all the .class files
# which are generated.  Most of the arguments should be self-explanatory and
# have the obvious defaults.  If $$remove_class_files is non-empty then the
# intermediate .class files are removed.
#

define CompileJavaSourcesToJar
(set -e;								      \
 if [ "$$java_sources" = "" ]; then					      \
   java_sources='$(filter %.java,$^)';					      \
 fi;									      \
 if [ "$$java_classpath" = "" ]; then					      \
   java_classpath='$(subst $(space),:,. $(filter %.jar,$^) $(VPATH) $(JAVA_SYSTEM_CLASSES))'; \
 fi;									      \
 if [ "$$javac_flags" = "" ]; then					      \
   javac_flags='$(JAVAC_FLAGS)';					      \
 fi;									      \
 if [ "$$target" = "" ]; then						      \
   target="$@";								      \
 fi;									      \
 $(CleanJavaClassFiles);						      \
 echo;									      \
 echo $(JAVAC) $$javac_flags -d . -classpath $$java_classpath $$java_sources; \
 $(JAVAC) $$javac_flags -d . -classpath $$java_classpath $$java_sources;      \
 classfiles=`find * -name '*.class' -print`;				      \
 echo;									      \
 echo $(JAR) cMf $$target $$classfiles;					      \
 $(JAR) cMf $$target $$classfiles;					      \
 if [ "$$remove_class_files" ]; then					      \
   echo "Removing .class files";					      \
   $(RM) $$classfiles;							      \
 fi;									      \
)
endef

#
# ExportJar exports a .jar file to the export tree's jar directory.
#

ifdef EXPORT_TREE
define ExportJar
(set -e;					\
 dir="$(EXPORT_TREE)/$(JARDIR)";		\
 files="$^";					\
 for file in $$files; do			\
   $(ExportFileToDir);				\
 done;						\
)
endef
endif

#
# CombineJars combines several .jar files into a single .jar file.  This is
# done by simply unjarring and rejarring all the class files again.  It's done
# in /tmp so that it should be on a local disk and therefore faster.
#

define CombineJars
(set -e;					\
 if [ "$$jars" = "" ]; then			\
   jars='$(filter %.jar,$^)';			\
 fi;						\
 echo;						\
 echo "**** Combining $$jars to make $@";	\
 echo;						\
 cwd=`pwd`;					\
 case "$@" in					\
   /*) target="$@";;				\
   *)  target="$$cwd/$@";;			\
 esac;						\
 dir="/tmp/jar$$$$";				\
 $(RMDIRHIER) $$dir;				\
 $(CreateDir);					\
 (echo cd $$dir;				\
  cd $$dir;					\
  for jar in $$jars; do				\
    case "$$jar" in				\
      /*) ;;					\
      *)  jar="$$cwd/$$jar";;			\
    esac;					\
    echo $(JAR) xf $$jar;			\
    $(JAR) xf $$jar;				\
  done;						\
  echo $(JAR) cMf $$target .;			\
  $(JAR) cMf $$target .);			\
 echo $(RMDIRHIER) $$dir;			\
 $(RMDIRHIER) $$dir;				\
 echo;						\
 echo "**** Finished making $@";		\
 echo;						\
)
endef

#
# JAVA_FIND_ALL_SOURCES is a variable which behaves more like a function,
# finding all .java files in this directory, or its subdirectories, or the
# equivalent directories in all the source trees.  Note that the resulting path
# names aren't absolute path names, but are relative to the top of the Java
# package hierarchy.  The "sort" is simply to remove any duplicates.
#

JAVA_FIND_ALL_SOURCES = $(sort $(foreach dir,. $(VPATH), \
	         $(shell cd $(dir); find * -name '*.java' -print 2>/dev/null)))

#
# CORBA_ORB_JAR gets omniORB.jar from the appropriate import tree.
#

CORBA_ORB_JAR = $(firstword \
	      $(foreach dir,$(IMPORT_JAR_DIRS),$(wildcard $(dir)/omniORB.jar)))
