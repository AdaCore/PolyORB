///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////
////                                                               ////
////                         AdaBroker                             ////
////                                                               ////
////                 class Ada_OmniObject                          ////
////                                                               ////
////                                                               ////
////   Copyright (C) 1999 ENST                                     ////
////                                                               ////
////   This file is part of the AdaBroker library                  ////
////                                                               ////
////   The AdaBroker library is free software; you can             ////
////   redistribute it and/or modify it under the terms of the     ////
////   GNU Library General Public License as published by the      ////
////   Free Software Foundation; either version 2 of the License,  ////
////   or (at your option) any later version.                      ////
////                                                               ////
////   This library is distributed in the hope that it will be     ////
////   useful, but WITHOUT ANY WARRANTY; without even the implied  ////
////   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR     ////
////   PURPOSE.  See the GNU Library General Public License for    ////
////   more details.                                               ////
////                                                               ////
////   You should have received a copy of the GNU Library General  ////
////   Public License along with this library; if not, write to    ////
////   the Free Software Foundation, Inc., 59 Temple Place -       ////
////   Suite 330, Boston, MA 02111-1307, USA                       ////
////                                                               ////
////                                                               ////
////                                                               ////
////   Description                                                 ////
////   -----------                                                 ////
////     This class is is both a C class and an Ada Class (see     ////
////     omniObject.ads). It is wrapped around omniObject_C2Ada    ////
////     in order to avoid the presence of non default construc-   ////
////     tors.                                                     ////
////     So, it provides the same functions as omniObject_C2Ada    ////
////     except that constructors are replaced by Init functions.  ////
////     It has also a pointer on the underlying omniObject_C2Ada  ////
////     object                                                    ////
////                                                               ////
////                                                               ////
////                                                               ////
////   authors : Sebastien Ponce, Fabien Azavant                   ////
////   date    : 02/28/99                                          ////
////                                                               ////
///////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////


#ifndef __omniObject_C2Ada__
#define __Ada_OmniObject__
#include "omniObject_C2Ada.hh"
#endif


class omniObject_C2Ada ;


class Ada_OmniObject {

public:

  Ada_OmniObject (void) ;
  // default constructor
  
  virtual ~Ada_OmniObject() ;
  
  static void Destructor(Ada_OmniObject* o) ;
  // static destructor that will be called from the Ada code
  // because the virtual destructor cannot be called from tha Ada code

  void Init ();
  // Initialisation of a local object via call to the
  // omniObject_C2Ada constructor on C_OmniObject
  
  void Init (const char *repoId,
	     Rope *r,
	     _CORBA_Octet *key,
	     size_t keysize,
	     IOP::TaggedProfileList *profiles,
	     _CORBA_Boolean release); 
  // Initialisation of a proxy object via call to the
  // omniObject_C2Ada constructor on C_OmniObject
  
  void Init (omniObject_C2Ada *omniobj);
  //Initialisation by giving the underlying omniObject_C2Ada pointer
  
  void setRopeAndKey(const omniRopeAndKey& l,_CORBA_Boolean keepIOP=1);
  // calls the setRopeAndKey function of C_OmniObject

  void  getRopeAndKey(omniRopeAndKey& l, _CORBA_Boolean& success);
  // calls the getRopeAndKey function of C_OmniObject
  
  void assertObjectExistent();
  // calls the assertObjectExistent function of C_OmniObject
  
  _CORBA_Boolean is_proxy();
  // calls the is_proxy function of C_OmniObject
  
  virtual _CORBA_Boolean dispatch(GIOP_S &,
				  const char *operation,
				  _CORBA_Boolean response_expected);
  // default dispatch function for all the hierarchie of
  // Ada Objects. The implementation is made in Ada.
  // (see omniobject.adb)

  virtual _CORBA_Boolean Ada_Is_A(const char* repoId) ;
  // calls is_a on this omniobject
  
  void setRepositoryID(const char* repoId) ;
  // call the PR_IRRepositoryId of omniObject
  
  const char* getRepositoryID() ;
  // calls th NP_repositoryId of omniObject
  
  omniObject_C2Ada *getOmniObject() ;
  // returns the underlying omniObject_C2Ada
  
  static Ada_OmniObject* string_to_ada_object(const char *repoId) ;
  // this function executes omni::stringToObject,
  // and cast the result into an Ada_OmniObject.
  // it can only be called by Corba.Orb.String_To_Object
  
  static Ada_OmniObject* ada_create_objref(const char* repoId,
					   IOP::TaggedProfileList* profiles,
					   _CORBA_Boolean release) ;
  // this function is called by the Ada code
  // to create aCorba.Object.Ref when unmarshalling
  // out of a bufferedstream.
  // it calls omni:: createObjRef
  // in objectRef.cc L 391

  static char* ada_object_to_string(Ada_OmniObject* objptr) ;
  // this function calls omni::objectToString
  // on the underlying object
  
  IOP::TaggedProfileList* iopProfiles() ; 
  // this function calls omniobject::iopProfiles()
  // on the underlying object

  
private:
  
  void* Implobj ;
  // This pointer is only used by the Ada side of this object
  
  omniObject_C2Ada *C_OmniObject;
  // Pointer on the underlying omniObject_C2Ada object
  
  bool Init_Ok;
  // This flag tells if an init function was called or not

  void* VTable;
  // This field is only used by Ada. It is needed to interface C++ and Ada  
  
};

extern void raise_ada_exception (const char *msg) ;
  // this function allows C code to raise Ada exception
  // It is implemented in Ada and only raise a No_Initialisation
  // exception with the message msg. (see corba.ads)



