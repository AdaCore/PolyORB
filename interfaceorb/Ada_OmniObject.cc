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
#include "Ada_exceptions.hh"
#include <omniORB2/CORBA.h>

// DEBUG is defined at the beginning of each file
// and undefined at the end of each file
//#define DEBUG


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
    Init_Ok = false ;
  } else {
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::~Ada_OmniObject without initialising object.") ;
  }
} ;


// Constructor
//------------
Ada_OmniObject*
Ada_OmniObject::Constructor() {
ADABROKER_TRY
  return new Ada_OmniObject() ;
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  Ada_OmniObject* default_result = NULL;
  return default_result; 
}


// Destructor
//------------
void
Ada_OmniObject::Destructor(Ada_OmniObject* o) {
ADABROKER_TRY

#ifdef DEBUG
  cerr << "Ada_OmniObject::Destructor : entering...." << endl ;
#endif
  
  if (o->Init_Ok) {

#ifdef DEBUG
    cerr << "Ada_OmniObject::Destructor Init_Ok = true -> DESTROYING OBJECT !!" << endl ;
#endif
      delete o ;

#ifdef DEBUG
    cerr << "Ada_OmniObject::Destructor OBJECT DESTROYED !!!" << endl ;
#endif

  } else {
#ifdef DEBUG
    cerr << "Ada_OmniObject::Destructor Init_Ok = false -> I am raising a fatalError !!!" << endl ;
#endif
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::Destructor without initialising object.") ;
  }
ADABROKER_CATCH
}


// InitLocalObject
//----------------
void
Ada_OmniObject::initLocalObject (const char* repoid)
{
ADABROKER_TRY
  // Creation of the underlying omniobject_C2Ada object
  try {
     C_Object = new omniObject_C2Ada (this) ;
  } catch (...) {
    cerr << "Ada_OmniObject::initLocalObject : you cannot initialize an object before initializing the ORB and the BOA" << endl
	 << "     >   Orb : Corba.Orb.Object := Corba.Orb.Orb_Init(\"omniORB2\") ;" << endl
	 << "     >   Boa : Corba.Boa.Object := Corba.Orb.Boa_Init(Orb, \"omniORB2_BOA\") ;" << endl 
	 << "must be the FIRST lines of your program, just after"<< endl
	 << "     >   procedure my_procedure is" << endl << endl ;

  }
  // setting its repository ID
  C_Object->PR_IRRepositoryId(repoid) ;
  // updating of Init_OK flag
  Init_Ok = true;
ADABROKER_CATCH
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
};


// objectDuplicate
//-----------------
Ada_OmniObject*
Ada_OmniObject::objectDuplicate(Ada_OmniObject* same) {
#ifdef DEBUG
    cerr << "Ada_OmniObject::objectDuplicate : entering, checking Init_OK" << endl ;
#endif
ADABROKER_TRY
  if (same->Init_Ok) {
#ifdef DEBUG
    cerr << "Ada_OmniObject::objectDuplicate : Init_Ok = true" << endl ;
#endif
    omni::objectDuplicate(same->C_Object) ;
    // register a new pointer to this object
#ifdef DEBUG
    cerr << "Ada_OmniObject::objectDuplicate : omni::objectDuplicate called OK" << endl ;
#endif

    Ada_OmniObject *result = new Ada_OmniObject() ;
    result->C_Object = same->C_Object ;
    result->Init_Ok = true ;
#ifdef DEBUG
     cerr << "Ada_OmniObject::objectDuplicate : exiting ... OK" << endl ;
#endif
    return result ;
  } else {
#ifdef DEBUG
    cerr << "Ada_OmniObject::objectDuplicate : Init_Ok = FALSE !!!!!!!! raising fatalException" << endl ;
#endif
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::objectDuplicate without initialising object.") ;
  }
ADABROKER_CATCH 
  // never reach here just a default return for dummy compilers.
  Ada_OmniObject* default_result = NULL;
  return default_result; 
}


// objectIsReady
//--------------
void
Ada_OmniObject::objectIsReady() {
ADABROKER_TRY
  if (Init_Ok) {
    omni::objectIsReady(C_Object) ;
  } else {
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::objectIsReady without initialising object.") ;
  }
ADABROKER_CATCH
}


// disposeObject
//--------------
void
Ada_OmniObject::disposeObject() {
ADABROKER_TRY
  if (Init_Ok) {
#ifdef DEBUG
    cerr << "Ada_OmniObject::disposeObject : disposing ..." << endl ;
#endif
    omni::disposeObject(C_Object) ;
#ifdef DEBUG
    cerr << "Ada_OmniObject::disposeObject : C++ dispose OK" << endl ;
#endif
  } else {
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::disposeObject without initialising object.") ;
  }
ADABROKER_CATCH
}


// hash
//-----
_CORBA_ULong
Ada_OmniObject::hash(_CORBA_ULong maximum) {
ADABROKER_TRY
  if (Init_Ok) {
    CORBA::Object_ptr tmp = new CORBA::Object ;
    tmp->PR_setobj(C_Object) ;
    _CORBA_ULong result = tmp->_hash(maximum) ;
    delete tmp ;
    return result ;
  } else {
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::hash without initialising object.") ;
  }
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  _CORBA_ULong default_result = 0;
  return default_result; 
}


// non_existent
//-------------
bool
Ada_OmniObject::non_existent() {
ADABROKER_TRY
  if (Init_Ok) {
    CORBA::Object_ptr tmp = new CORBA::Object ;
    tmp->PR_setobj(C_Object) ;
    _CORBA_Boolean result = tmp->_non_existent() ;
    delete tmp ;
    return result ;
  } else {
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::non_existent without initialising object.") ;
  }
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  bool default_result = false;
  return default_result; 
}


// setRopeAndKey
//--------------
void
Ada_OmniObject::setRopeAndKey(const Ada_OmniRopeAndKey& l,
			      _CORBA_Boolean keepIOP)
{
ADABROKER_TRY
  if ( (Init_Ok) && (l.assertInit_Ok())) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    C_Object->setRopeAndKey(*(l.C_Object),keepIOP);
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::setRopeAndKey without initialising object.") ;
  }
ADABROKER_CATCH 
};


// resetRopeAndKey
//----------------
void
Ada_OmniObject::resetRopeAndKey ()
{
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    C_Object->resetRopeAndKey();
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::resetRopeAndKey without initialising object.") ;
  }
ADABROKER_CATCH 
};
  


// getRopeAndKey
//--------------
void
Ada_OmniObject::getRopeAndKey(Ada_OmniRopeAndKey& l, _CORBA_Boolean &success) {
ADABROKER_TRY
  if ((Init_Ok) && (l.assertInit_Ok())) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    success = C_Object->getRopeAndKey(*(l.C_Object));
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::getRopeAndKey without initialising object.") ;
  }
ADABROKER_CATCH 
}
      
  
// assertObjectExistent
//---------------------
void
Ada_OmniObject::assertObjectExistent() {
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    C_Object->assertObjectExistent();
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::assertObjectExistent without initialising object.") ;
  }
ADABROKER_CATCH 
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
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::is_proxy without initialising object.") ;
  }
}


// dispatch
//---------
extern _CORBA_Boolean dispatch(GIOP_S &,
			       const char *operation,
			       _CORBA_Boolean response_expected);
// See implementation in omniobject.adb
  

// setRepositoryID
//----------------
void
Ada_OmniObject::setRepositoryID(const char* repoId) {
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    C_Object->PR_IRRepositoryId(repoId);
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::setRepositoryId without initialising object.") ;
  }
ADABROKER_CATCH
}


// getRepositoryID
//----------------
const char*
Ada_OmniObject::getRepositoryID() {
ADABROKER_TRY
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
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::getRepositoryIdgetRepositoryId without initialising object.") ;
  }
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  char* default_result = NULL;
  return default_result; 
}
 

// string_to_ada_object
//---------------------
Ada_OmniObject*
Ada_OmniObject::string_to_ada_object(const char *repoId) {
ADABROKER_TRY
  omniObject *objptr = omni::stringToObject(repoId) ;
  omniObject_C2Ada *adaobj = dynamic_cast<omniObject_C2Ada*>(objptr) ;

  if (adaobj == 0) {
    return 0 ;
  } else {
    return adaobj->Ada_OmniObject_Pointer ;
  }
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  Ada_OmniObject* default_result = NULL;
  return default_result; 
}


// ada_object_to_string
//---------------------
char*
Ada_OmniObject::ada_object_to_string(Ada_OmniObject* objptr) {
ADABROKER_TRY
  if ( objptr == 0 ) {
    return omni::objectToString(0) ;
  } else {
    if (objptr->Init_Ok) {
      return omni::objectToString(objptr->C_Object) ;
    } else {
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::ada_object_to_string without initialising object.") ;
    }
  }
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  char* default_result = NULL;
  return default_result; 
}


// iopProfiles
//------------
IOP::TaggedProfileList*
Ada_OmniObject::iopProfiles() {
ADABROKER_TRY
  if (Init_Ok) {
    // if Initialisation was made then call the corresponding
    // function on C_Object
    return C_Object->iopProfiles();
  } else {
    // else raise an Ada Exception
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::getRepositoryId without initialising object.") ;
  }
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  IOP::TaggedProfileList* default_result = NULL;
  return default_result; 
}


// getOmniObject
//--------------
omniObject_C2Ada*
Ada_OmniObject::getOmniObject() {
  if (Init_Ok) {
    return C_Object ;
  } else {
    throw omniORB::fatalException(__FILE__,
				  __LINE__,
				  "Call of Ada_OmniObject::getOmniObject without initialising object.") ;
  }
}

// ada_create_objref
//---------------------
Ada_OmniObject*
Ada_OmniObject::ada_create_objref(const char* repoId,
				  IOP::TaggedProfileList* profiles,
				  _CORBA_Boolean release) {
ADABROKER_TRY
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
ADABROKER_CATCH
  // never reach here just a default return for dummy compilers.
  Ada_OmniObject* default_result = NULL;
  return default_result; 
}


#undef DEBUG
