# Ce makefile permet de compiler les fichiers cc, hh, ads et adb
# generes par Adabroker.
# Il s'agit d'une version perso, experimentale

# compilateurs ada et c++
GNATGCC = gnatgcc
GCC = g++

#preprocesseur gnat
GNATPREP = gnatprep

# flag de compilation
DO_NOT_LINK = -c

# emplacements des librairies c++ de omniORB
ORB_LIBS = -I /home/Seb/Projet/omniORB_2.7.0/include/
ORB_LIBS += -I /home/Seb/Projet/omniORB_2.7.0/src/lib/omniORB2/
ORB_LIBS += -I /home/Seb/Projet/omniORB_2.7.0/src/lib/omniORB2/orbcore/

# variables a definir pour la compilation
CORBA_FLAGS = -D __x86__ -D __linux__ -D __OSVERSION__=2
OMNI_ORB_FLAGS = 
FLAGS = $(CORBA_FLAGS) $(OMNI_ORB_FLAGS)

# liste des librairies c++ a compiler
CC_LIBS_NAME = omniObject_C2Ada Ada_OmniObject Ada_Rope

# liste des librairies Ada a compiler
ADA_LIBS_NAME =
ADA_LIBS_NAME_NO_ADB = rope

all : sys_dep.o omniObject_C2Ada.o Ada_OmniObject.o Ada_Rope.o rope.o \
Ada_OmniRopeAndKey.o omniropeandkey.o

sys_dep.o : sys_dep_before_preprocessor.ads sys_dep_before_preprocessor.adb
	$(GNATPREP) sys_dep_before_preprocessor.ads sys_dep.ads def_file -u
	$(GNATPREP) sys_dep_before_preprocessor.adb sys_dep.adb def_file -u
	$(GNATGCC) $(DO_NOT_LINK) sys_dep.adb

omniObject_C2Ada.o : omniObject_C2Ada.hh omniObject_C2Ada.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) omniObject_C2Ada.cc

Ada_OmniObject.o : Ada_OmniObject.hh Ada_OmniObject.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) Ada_OmniObject.cc

Ada_Rope.o : Ada_Rope.cc Ada_Rope.hh
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) Ada_Rope.cc

rope.o : rope.ads
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) rope.ads

Ada_OmniRopeAndKey.o : Ada_OmniRopeAndKey.hh Ada_OmniRopeAndKey.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) Ada_OmniRopeAndKey.cc

omniropeandkey.o : omniropeandkey.ads omniropeandkey.adb
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) omniropeandkey.adb
