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
# -D USE_omniORB_logStream -D _OMNIORB2_LIBRARY

FLAGS = $(CORBA_FLAGS) $(OMNI_ORB_FLAGS)

# liste des librairies c++ a compiler
CC_LIBS_NAME = omniObject_C2Ada Ada_OmniObject Ada_Rope

# liste des librairies Ada a compiler
ADA_LIBS_NAME =
ADA_LIBS_NAME_NO_ADB = rope


all : sys_dep.o Ada_Rope.o rope.o Ada_OmniRopeAndKey.o omniropeandkey.o \
omniobjectmanager.o omniObject_C2Ada.o Ada_OmniObject.o omniobject.o \
giop.o Ada_Giop_s.o giop_s.o Ada_Giop_c.o giop_c.o Ada_netBufferedStream.o \
netbufferedstream.o

clean :
	rm *.ali *.o 

sys_dep.o : sys_dep_before_preprocessor.ads sys_dep_before_preprocessor.adb
	$(GNATPREP) sys_dep_before_preprocessor.ads sys_dep.ads def_file -u
	$(GNATPREP) sys_dep_before_preprocessor.adb sys_dep.adb def_file -u
	$(GNATGCC) $(DO_NOT_LINK) sys_dep.adb

Ada_Rope.o : Ada_Rope.cc Ada_Rope.hh
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) Ada_Rope.cc

rope.o : rope.ads
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) rope.ads

Ada_OmniRopeAndKey.o : Ada_OmniRopeAndKey.hh Ada_OmniRopeAndKey.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) Ada_OmniRopeAndKey.cc

omniropeandkey.o : omniropeandkey.ads omniropeandkey.adb
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) omniropeandkey.adb

omniobjectmanager.o : omniobjectmanager.ads omniobjectmanager.adb
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) omniobjectmanager.adb

omniObject_C2Ada.o : omniObject_C2Ada.hh omniObject_C2Ada.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) omniObject_C2Ada.cc

Ada_OmniObject.o : Ada_OmniObject.hh Ada_OmniObject.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) Ada_OmniObject.cc

omniobject.o : omniobject.ads omniobject.adb
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) omniobject.adb

giop.o : giop.ads giop.adb
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) giop.adb

Ada_Giop_s.o : Ada_Giop_s.hh Ada_Giop_s.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) Ada_Giop_s.cc

giop_s.o : giop_s.ads giop_s.adb
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) giop_s.adb

Ada_Giop_c.o : Ada_Giop_c.hh Ada_Giop_c.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) Ada_Giop_c.cc

giop_c.o : giop_c.ads giop_c.adb
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) giop_c.adb

Ada_netBufferedStream.o : Ada_netBufferedStream.hh Ada_netBufferedStream.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) Ada_netBufferedStream.cc

netbufferedstream.o : netbufferedstream.ads netbufferedstream.adb
	$(GNATGCC) $(DO_NOT_LINK) $(ORB_LIBS) $(FLAGS) netbufferedstream.adb


################################################
# compilation of files that do not depend on C++
################################################
adaonly: corba.ali corba-object.ali giop_s.ali giop.ali omni.ali \
bufferedstream.ali netbufferedstream.ali 

.SUFFIXES: .adb .ali

.adb.ali:
	$(GNATGCC) $(DO_NOT_LINK) $<

corba.ali: corba.ads corba.adb

corba-object.ali: corba-object.ads corba-object.adb

giop_s.ali: giop_s.ads giop_s.adb

giop.ali: giop.ads

omni.ali: omni.ads omni.adb

bufferedstream.ali: bufferedstream.ads bufferedstream.adb

netbufferedstream.ali: netbufferedstream.ads netbufferedstream.adb

membufferedstream.ali: membufferedstream.ads membufferedstream.adb



