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


// DEBUG is defined at the beginning of each file
// and undefined at the end of each file
#define DEBUG


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
  if (Init_Ok) {
    omni::objectRelease(C_Object) ;
    // we must not delete C_Object because there might be other
    // refernces to this object. omniORB's objectRelease is
    // here to handle memory
  } else {
    raise_ada_exception("Ada_OmniObject::~Ada_OmniObject cannot be called on a non-initialized object") ;
  }
} ;


// Constructor
//------------
Ada_OmniObject*
Ada_OmniObject::Constructor() {
  return new Ada_OmniObject() ;
}


// Destructor
//------------
void
Ada_OmniObject::Destructor(Ada_OmniObject* o) {
  if (o->Init_Ok) {
    delete o ;
  } else {
    raise_ada_exception("Ada_OmniObject::Destructor cannot be called on a non-initialized object") ;
  }
}


// InitLocalObject
//----------------
void
Ada_OmniObject::initLocalObject (const char* repoid)
{
  // Creation of the underlying omniobject_C2Ada object
  C_Object = new omniObject_C2Ada (this);
  // setting its repository ID
  C_Object->PR_IRRepositoryId(repoid) ;
  // updating of Init_OK flag
  Init_Ok = true;
  return;
}


 
// InitProxyObject
//----------------
void
Ada_OmniObject::initProxyObject (const char *repoId,
				   Rope *r,
				   _CORBA_Octet *key,
				   size_t keysize,
				   IOP::TaggedProfileList *profiles,
				   _CORBA_Boolean release)
{
  // Creation of the underlying omniobject_C2Ada object
  C_Object = new omniObject_C2Ada (repoId,
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
/*void
  Ada_OmniObject::Init (omniObject_C2Ada *omniobj)
  {
  C_Object = omniobj;
  Init_Ok = true;
  return;
  }*/

 
// objectDuplicate
//-----------------
Ada_OmniObject*
Ada_OmniObject::objectDuplicate(Ada_OmniObject* same) {
  if (same->Init_Ok) {
    omni::objectDuplicate(same->C_Object) ;
    // register a new pointer to this object
    
    Ada_OmniObject *result = new Ada_OmniObject() ;
    result->C_Object = same->C_Object ;
    result->Init_Ok = true ;
  } else {
    raise_ada_exception("Ada_OmniObject::objectDuplicate cannot be called on a non-initialized object") ;
  }
}


// objectIsReady
//--------------
void
Ada_OmniObject::objectIsReady() {
  if (Init_Ok) {
    omni::objectIsReady(C_Object) ;
  } else {
    raise_ada_exception("Ada_OmniObject::objectIsReady cannot be called on a non-initialized object") ;
  }
}


// disposeObject
//--------------
void
Ada_OmniObject::disposeObject() {
  if (Init_Ok) {
  omni::disposeObject(C_Object) ;
  } else {
    raise_ada_exception("Ada_OmniObject::disposeObject cannot be called on a non-initialized object") ;
  }
}


// setRopeAndKey
//--------------
void
Ada_OmniObject::setRopeAndKey(const omniRopeAndKey& l,_CORBA_Boolean keepIOP=1)
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    C_Object->setRopeAndKey(l,keepIOP);
    return;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniObject::setRopeAndKey without initialising object.");
  }
};


// resetRopeAndKey
//----------------
void
Ada_OmniObject::resetRopeAndKey ()
{
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    C_Object->resetRopeAndKey();
    return;
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniObject::resetRopeAndKey without initialising object.");
  }
};
  


// getRopeAndKey
//--------------
void
Ada_OmniObject::getRopeAndKey(omniRopeAndKey& l, _CORBA_Boolean& success) {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    success = C_Object->getRopeAndKey(l);
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
    // function on C_Object
    C_Object->assertObjectExistent();
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
    // function on C_Object
    return C_Object->is_proxy();
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


// setRepositoryID
//----------------
void
Ada_OmniObject::setRepositoryID(const char* repoId) {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    C_Object->PR_IRRepositoryId(repoId);
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
    // function on C_Object
    const char *result = C_Object->NP_IRRepositoryId();

#ifdef DEBUG
    cerr << "Ada_OmniObject::getRepositoryID : " << result << endl ;
#endif

    return result ;
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
    if (objptr->Init_Ok) {
      return omni::objectToString(objptr->C_Object) ;
    } else {
      raise_ada_exception("Ada_OmniObject::ada_object_to_string cannot be called on a non-initialized object") ;
    }
  }
}


// iopProfiles
//------------
IOP::TaggedProfileList*
Ada_OmniObject::iopProfiles() {
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    return C_Object->iopProfiles();
  } else {
    // else raise an Ada Exception
    raise_ada_exception ("Call of Ada_OmniObject::getRepositoryId without initialising object.");
  }
}


// getOmniObject
//--------------
omniObject_C2Ada*
Ada_OmniObject::getOmniObject() {
  if (Init_Ok) {
    return C_Object ;
  } else {
    raise_ada_exception("Ada_OmniObject::getOmniObject cannot be called on a non-initialized object") ;
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


#undef DEBUG
