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
#include "Ada_OmniRopeAndKey.hh"


class omniObject_C2Ada ;


class Ada_OmniObject {

public:

  Ada_OmniObject (void) ;
  // default constructor
  
  virtual ~Ada_OmniObject() ;

  static Ada_OmniObject *Constructor() ;
  // static constructor.
  // this is a workaround for gnat 3.11p where we cannot
  // write "new Object"
  
  static void Destructor(Ada_OmniObject* o) ;
  // static destructor that will be called from the Ada code
  // because the virtual destructor cannot be called from tha Ada code

  void initLocalObject (const char* repoID);
  // Initialisation of a local object via call to the
  // omniObject_C2Ada constructor on C_OmniObject
  // For a local object, we have to set the repository_id

  
  void initProxyObject (const char *repoId,
			  Rope *r,
			  _CORBA_Octet *key,
			  size_t keysize,
			  IOP::TaggedProfileList *profiles,
			  _CORBA_Boolean release); 
  // Initialisation of a proxy object via call to the
  // omniObject_C2Ada constructor on C_Object
  
  //void Init (omniObject_C2Ada *omniobj);
  //Initialisation by giving the underlying omniObject_C2Ada pointer
  // who uses it ??
  
  static Ada_OmniObject* objectDuplicate(Ada_OmniObject* omniobj) ;
  // Creation of an Ada_OmniObject referencing the same
  // omniObject ( used for Omniobject.Duplicate )
  
  void objectIsReady() ;
  // calls omni::objectIsReady on C_Object
  // to tell the ORB that this local object is
  // ready to accpet connexions
  
  void disposeObject() ;
  // calls omni::disposeObject on C_OmniObject
  // it has to be done only for local object
  // to tell the ORB they cannot receive connexions any longer

  void setRopeAndKey(const Ada_OmniRopeAndKey& l,_CORBA_Boolean keepIOP=1);
  // calls the setRopeAndKey function of C_Object

  void  getRopeAndKey(Ada_OmniRopeAndKey& l, _CORBA_Boolean& success);
  // calls the getRopeAndKey function of C_Object

  void resetRopeAndKey();
  // calls the resetRopeAdnKey function of C_Object
  
  void assertObjectExistent();
  // calls the assertObjectExistent function of C_Object
  
  _CORBA_Boolean is_proxy();
  // calls the is_proxy function of C_Object
  
  virtual void dispatch(Ada_Giop_s &,
			const char *operation,
			_CORBA_Boolean response_expected,
			_CORBA_Boolean& success);
  // default dispatch function for all the hierarchie of
  // Ada Objects. The implementation is made in Ada.
  // (see omniobject.adb)
  // this function is made a procedure because it takes
  // arguments passed by reference

  virtual _CORBA_Boolean Ada_Is_A(const char* repoId) ;
  // calls is_a on this omniobject
  
  const char* getRepositoryID() ;
  // calls th NP_repositoryId of omniObject
  
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


  omniObject_C2Ada *getOmniObject() ;
  // returns the unserlying C_Object
  // used in proxyObjectFactory_C2Ada
  
private:

  void setRepositoryID(const char *repoId) ;
  // sets the repository id for a local object
  
  void* Implobj ;
  // This pointer is only used by the Ada side of this object

public:
  omniObject_C2Ada *C_Object;
  // Pointer on the underlying omniObject_C2Ada object

private:
  bool Init_Ok;
  // This flag tells if an init function was called or not

  void* VTable;
  // This field is only used by Ada. It is needed to interface C++ and Ada  
  
};

extern void raise_ada_exception (const char *msg) ;
  // this function allows C code to raise Ada exception
  // It is implemented in Ada and only raise a No_Initialisation
  // exception with the message msg. (see corba.ads)



