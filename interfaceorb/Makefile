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

# variables a definir pour la compilation
CORBA_FLAGS = -D __x86__ -D __linux__ -D __OSVERSION__=2


all : omniObject_C2Ada.o Ada_OmniObject.o

omniObject_C2Ada.o : omniObject_C2Ada.hh omniObject_C2Ada.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(CORBA_FLAGS) omniObject_C2Ada.cc

Ada_OmniObject.o : Ada_OmniObject.hh Ada_OmniObject.cc
	$(GCC) $(DO_NOT_LINK) $(ORB_LIBS) $(CORBA_FLAGS) Ada_OmniObject.cc

sys_dep.o : sys_dep.ads
	$(GNATPREP) sys_dep.ads sys_dep2.ads def_file -u
	$(GNATGCC) $(DO_NOT_LINK) sys_dep2.ads -o sysdep.o