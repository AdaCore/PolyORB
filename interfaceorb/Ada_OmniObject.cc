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


#include "Ada_OmniObject.hh"

// Constructor
//------------
Ada_OmniObject::Ada_OmniObject()
{
  Init_Ok = false;
};


// ~Ada_OmniObject
//----------------
Ada_OmniObject::~Ada_OmniObject()
{
  delete C_OmniObject ;
} ;


// Destructor
//------------
void
Ada_OmniObject::Destructor(Ada_OmniObject* o) {
  delete o ;
}


// Init
//-----
void
Ada_OmniObject::Init ()
{
  // Creation of the underlying omniobject_C2Ada object
  C_OmniObject = new omniObject_C2Ada (this);
  // updating of Init_OK flag
  Init_Ok = true;
  return;
}


// Init
//-----
void
Ada_OmniObject::Init (const char *repoId,
		      Rope *r,
		      _CORBA_Octet *key,
		      size_t keysize,
		      IOP::TaggedProfileList *profiles,
		      _CORBA_Boolean release)
{
  // Creation of the underlying omniobject_C2Ada object
  C_OmniObject = new omniObject_C2Ada (repoId,
				       r,
				       key,
				       keysize,
				       profiles,
				       release,
				       this);
  // updating of Init_OK flag
  Init_Ok = true;
  return;
};


// Init
//-----
void
Ada_OmniObject::Init (omniObject_C2Ada *omniobj)
{
  C_OmniObject = omniobj;
  Init_Ok = true;
  return;
}

 
// setRopeAndKey
//--------------
void
Ada_OmniObject::setRopeAndKey(const omniRopeAndKey& l,_CORBA_Boolean keepIOP=1)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    C_OmniObject->setRopeAndKey(l,keepIOP);
    return;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniObject::setRopeAndKey without initialising object.");
  }
};


// getRopeAndKey
//--------------
void
Ada_OmniObject::getRopeAndKey(omniRopeAndKey& l, _CORBA_Boolean& success) {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    success = C_OmniObject->getRopeAndKey(l);
    return ;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniObject::getRopeAndKey without initialising object.");
  }
}
      
  
// assertObjectExistent
//---------------------
void
Ada_OmniObject::assertObjectExistent() {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    C_OmniObject->assertObjectExistent();
    return;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniObject::assertObjectExistent without initialising object.");    
  }
}


// is_proxy
//---------
_CORBA_Boolean
Ada_OmniObject::is_proxy() {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    return C_OmniObject->is_proxy();
  } else {
    // else raise an Ada Exception
   raise_ada_exception ("Call of Ada_OmniObject::is_proxy without initialising object.");    
  }
}


// dispatch
//---------
extern _CORBA_Boolean dispatch(GIOP_S &,
			       const char *operation,
			       _CORBA_Boolean response_expected);
// See implementation in omniobject.adb
  

// Ada_Is_A
//---------
extern _CORBA_Boolean Ada_Is_A(const char* repoId) ;
// See implementation in omniobject.adb


// getOmniObject
//--------------
omniObject_C2Ada *
Ada_OmniObject::getOmniObject() {
  return C_OmniObject ;
}



// setRepositoryID
//----------------
void
Ada_OmniObject::setRepositoryID(const char* repoId) {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    C_OmniObject->PR_IRRepositoryId(repoId);
    return;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniObject::setRepositoryId without initialising object.");
  }
}



// getRepositoryID
//----------------
const char*
Ada_OmniObject::getRepositoryID() {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    return C_OmniObject->NP_IRRepositoryId();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniObject::getRepositoryId without initialising object.");
  }
}
 

// string_to_ada_object
//---------------------
Ada_OmniObject*
Ada_OmniObject::string_to_ada_object(const char *repoId) {
  omniObject *objptr = omni::stringToObject(repoId) ;
  omniObject_C2Ada *adaobj = dynamic_cast<omniObject_C2Ada*>(objptr) ;

  if (adaobj == 0) {
    return 0 ;
  } else {
    return adaobj->Ada_OmniObject_Pointer ;
  }
}


// ada_object_to_string
//---------------------
char*
Ada_OmniObject::ada_object_to_string(Ada_OmniObject* objptr) {
  if ( objptr == 0 ) {
    return omni::objectToString(0) ;
  } else {
    return omni::objectToString(objptr->C_OmniObject) ;
  }
}


// iopProfiles
//------------
IOP::TaggedProfileList*
Ada_OmniObject::iopProfiles() {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_OmniObject
    return C_OmniObject->iopProfiles();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniObject::getRepositoryId without initialising object.");
  }
}


// ada_create_objref
//---------------------
Ada_OmniObject*
Ada_OmniObject::ada_create_objref(const char* repoId,
				  IOP::TaggedProfileList* profiles,
				  _CORBA_Boolean release) {
  omniObject *objptr = omni::createObjRef(repoId,
					  0, // omniORB believes that we just
					  // want to cast the result into a
					  // CORBA::Object_ptr
					  profiles,
					  release) ;
  omniObject_C2Ada *adaobj = dynamic_cast<omniObject_C2Ada*>(objptr) ;
  
  if (adaobj == 0) {
    return 0 ;
  } else {
    return adaobj->Ada_OmniObject_Pointer ;
  }
}
