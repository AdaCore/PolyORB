
//  This class is both a C class and an Ada Class (see
//  omniObject.ads). It is wrapped around omniObject_C2Ada in order to
//  avoid the presence of non default constructors. It provides the
//  same functions as omniObject_C2Ada except that constructors are
//  replaced by Init functions.  It has also a pointer on the
//  underlying omniObject_C2Ada object

#include <iostream>
#include "Ada_OmniObject.hh"
#include "Ada_exceptions.hh"
#include <omniORB2/CORBA.h>

//--------------------------------//
// Ada_OmniObject::Ada_OmniObject //
//--------------------------------//

Ada_OmniObject::Ada_OmniObject()
{
  Init_Ok = false;
};

//--------------------------------//
// Ada_OmniObject::Ada_OmniObject //
//--------------------------------//

Ada_OmniObject::Ada_OmniObject(omniObject_C2Ada* cpp_object,
			       int               interface)
{
  CPP_Object = cpp_object;
  Interface = interface;
  Init_Ok = true;
};

//---------------------------------//
// Ada_OmniObject::~Ada_OmniObject //
//---------------------------------//

Ada_OmniObject::~Ada_OmniObject()
{
  if (Init_Ok) {

#ifdef DEBUG
    cerr << "Ada_OmniObject::~Ada_OmniObject : ref = "
	 << CPP_Object->getRefCount()
	 << endl;
#endif

    omni::objectRelease(CPP_Object);
    // We must not delete CPP_Object because there might be other
    // refernces to this object. omniORB's objectRelease is here to
    // handle memory.

    Init_Ok = false;
  } else {
    throw omniORB::fatalException
      (__FILE__,
       __LINE__,
       "Ada_OmniObject::~Ada_OmniObject without initializing object.");
  }
}

//----------------------------//
// Ada_OmniObject:Constructor //
//----------------------------//

Ada_OmniObject*
Ada_OmniObject::Constructor() {
  ADABROKER_TRY
    return new Ada_OmniObject();
  ADABROKER_CATCH
    // never reach here just a default return for dummy compilers.
    Ada_OmniObject* default_result = NULL;
    return default_result; 
}

//----------------------------//
// Ada_OmniObject::Destructor //
//----------------------------//

void
Ada_OmniObject::Destructor(Ada_OmniObject* o)
{
  ADABROKER_TRY
    
#ifdef DEBUG
    cerr << "Ada_OmniObject::Destructor : enter" << endl;
    cerr << "Ada_OmniObject::Destructor : ref = " 
         << o->CPP_Object->getRefCount()
	 << endl;
#endif
    
    if (o->Init_Ok) {
      
#ifdef DEBUG
      if (o->is_proxy()) {
	cerr << "Ada_OmniObject::Destructor (proxy) : Init_Ok true" << endl;
      } else {
	cerr << "Ada_OmniObject::Destructor (local) : Init_Ok true" << endl;
      }
#endif
      delete o;
      
#ifdef DEBUG
      cerr << "Ada_OmniObject::Destructor : leave" << endl;
#endif
      
    } else {

#ifdef DEBUG
      cerr << "Ada_OmniObject::Destructor : Init_Ok false" << endl;
      cerr << "   Raise a Fatal Error" << endl;
#endif

      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::Destructor without initializing object.");
    }
  ADABROKER_CATCH
}

//---------------------------------//
// Ada_OmniObject::InitLocalObject //
//---------------------------------//

void
Ada_OmniObject::initLocalObject (const char* repoid)
{
  ADABROKER_TRY
    // Create underlying omniobject_C2Ada object.
    try {
      CPP_Object = new omniObject_C2Ada (this);

#ifdef DEBUG
      cout << "initLocalObject : " << *repoid
	   << " correctly initialized" << endl;
#endif

    } catch (...) {
      cerr << "initLocalObject : cannot initialize";
      cerr << " without initialized ORB and BOA" << endl;
    }

  // Set its repository ID.
  CPP_Object->PR_IRRepositoryId(repoid);

  // Update Init_OK.
  Init_Ok = true;
  ADABROKER_CATCH
}

//---------------------------------//
// Ada_OmniObject::InitProxyObject //
//---------------------------------//

void
Ada_OmniObject::initProxyObject (const char             * repoId,
				 Rope                   * r,
				 _CORBA_Octet           * key,
				 size_t                   keysize,
				 IOP::TaggedProfileList * profiles,
				 _CORBA_Boolean           release)
{
  // Create underlying omniobject_C2Ada object.
  CPP_Object = new omniObject_C2Ada (repoId,
				     r,
				     key,
				     keysize,
				     profiles,
				     release);

  // Update Init_OK.
  Init_Ok = true;
}

//---------------------------------//
// Ada_OmniObject::objectDuplicate //
//---------------------------------//

Ada_OmniObject*
Ada_OmniObject::objectDuplicate(Ada_OmniObject* same)
{
  ADABROKER_TRY

#ifdef DEBUG
    cerr << "Ada_OmniObject::objectDuplicate : enter" << endl;
#endif

    if (same->Init_Ok) {
    
#ifdef DEBUG
      if (same->is_proxy()) {
	cerr << "Ada_OmniObject::objectDuplicate (proxy) : Init_Ok true"
	     << endl;
      } else {
	cerr << "Ada_OmniObject::objectDuplicate (local) : Init_Ok true"
	     << endl;
      }
#endif

    // Register a new pointer to this object.
      omni::objectDuplicate(same->CPP_Object);

#ifdef DEBUG
      cerr << "Ada_OmniObject::objectDuplicate : leave" << endl;
      cerr << "Ada_OmniObject::objectDuplicate : ref = " 
	   << same->CPP_Object->getRefCount()
	   << endl;
#endif

      return new Ada_OmniObject(same->CPP_Object, same->Interface);
      
    } else {
      
#ifdef DEBUG
      cerr << "Ada_OmniObject::objectDuplicate : Init_Ok = false" << endl;
      cerr << "   Raise Fatal_Exception" << endl;
#endif

      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::objectDuplicate without initializing object.");
    }
  ADABROKER_CATCH 
    // Never reach this code. Just a default return for dummy compilers.
    Ada_OmniObject* default_result = NULL;
    return default_result; 
}

//-------------------------------//
// Ada_OmniObject::objectIsReady //
//-------------------------------//

void
Ada_OmniObject::objectIsReady()
{
  ADABROKER_TRY
    if (Init_Ok) {
      omni::objectIsReady(CPP_Object);
    } else {
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::objectIsReady without initializing object.");
    }

#ifdef DEBUG
    cerr << "Ada_OmniObject::object_is_ready : ref = " 
	 << CPP_Object->getRefCount()
	 << endl;
#endif

  ADABROKER_CATCH
}

//-----------------------------//
// Ada_OmniObjectdisposeObject //
//-----------------------------//

void
Ada_OmniObject::disposeObject()
{
  ADABROKER_TRY
    if (Init_Ok) {
      
#ifdef DEBUG
      cerr << "Ada_OmniObject::disposeObject : enter" << endl;
#endif

      omni::disposeObject(CPP_Object);
      
#ifdef DEBUG
      cerr << "Ada_OmniObject::disposeObject : C++ dispose" << endl;
#endif

    } else {
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::disposeObject without initializing object.");
    }
  ADABROKER_CATCH
}

//----------------------//
// Ada_OmniObject::hash //
//----------------------//

_CORBA_ULong
Ada_OmniObject::hash(_CORBA_ULong maximum)
{
  ADABROKER_TRY
    if (Init_Ok) {
      CORBA::Object_ptr tmp = new CORBA::Object;
      tmp->PR_setobj(CPP_Object);
      _CORBA_ULong result = tmp->_hash(maximum);
      delete tmp;
      return result;
    } else {
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::hash without initializing object.");
    }
  ADABROKER_CATCH
    // Never reach this code. Just a default return for dummy compilers.
    _CORBA_ULong default_result = 0;
    return default_result; 
}

//-------------------------------//
// Ada_OmniObject::is_equivalent //
//-------------------------------//

_CORBA_Boolean
Ada_OmniObject::is_equivalent(Ada_OmniObject * other)
{
  ADABROKER_TRY
    if (Init_Ok && other->Init_Ok) {
      CORBA::Object_ptr tmp_self  = new CORBA::Object;
      CORBA::Object_ptr tmp_other = new CORBA::Object;
      tmp_self->PR_setobj (CPP_Object);
      tmp_other->PR_setobj (other->CPP_Object);
      _CORBA_Boolean result = tmp_self->_is_equivalent(tmp_other);
      delete tmp_self;
      delete tmp_other;
      return result;
    } else {
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::is_equivalent without initializing object.");
    }
  ADABROKER_CATCH
    // Never reach this code. Just a default return for dummy
    // compilers.
    _CORBA_Boolean default_result = false;
    return default_result; 
}

//------------------------------//
// Ada_OmniObject::non_existent //
//------------------------------//

bool
Ada_OmniObject::non_existent()
{
  ADABROKER_TRY
    if (Init_Ok) {
      CORBA::Object_ptr tmp = new CORBA::Object;
      tmp->PR_setobj(CPP_Object);
      _CORBA_Boolean result = tmp->_non_existent();
      delete tmp;
      return result;
    } else {
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::non_existent without initializing object.");
    }
  ADABROKER_CATCH
    // Never reach this code. Just a default return for dummy compilers.
    bool default_result = false;
    return default_result; 
}

//------------------------------//
// Ada_OmniObject:setRopeAndKey //
//------------------------------//

void
Ada_OmniObject::setRopeAndKey(const Ada_OmniRopeAndKey& l,
			      _CORBA_Boolean keepIOP)
{
  ADABROKER_TRY
    if ( (Init_Ok) && (l.assertInit_Ok())) {
      // If already initialized, call function on CPP_Object.
      CPP_Object->setRopeAndKey(*(l.CPP_Object),keepIOP);
    } else {
      // Raise an Ada Exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::setRopeAndKey without initializing object.");
    }
  ADABROKER_CATCH 
}

//---------------------------------//
// Ada_OmniObject::resetRopeAndKey //
//---------------------------------//

void
Ada_OmniObject::resetRopeAndKey ()
{
  ADABROKER_TRY
    if (Init_Ok) {
      // If already initialized, call function on CPP_Object.
      CPP_Object->resetRopeAndKey();
    } else {
      // Raise an Ada Exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::resetRopeAndKey without initializing object.");
    }
  ADABROKER_CATCH 
}
  
//-------------------------------//
// Ada_OmniObject::getRopeAndKey
//-------------------------------//

void
Ada_OmniObject::getRopeAndKey(Ada_OmniRopeAndKey & l,
			      _CORBA_Boolean     & success)
{
  ADABROKER_TRY
    if ((Init_Ok) && (l.assertInit_Ok())) {
      // It already initialized, call function on CPP_Object.
      success = CPP_Object->getRopeAndKey(*(l.CPP_Object));
    } else {
      // Raise an Ada Exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::getRopeAndKey without initializing object.");
    }
  ADABROKER_CATCH 
}

//--------------------------------------//
// Ada_OmniObject::assertObjectExistent //
//--------------------------------------//

void
Ada_OmniObject::assertObjectExistent()
{
  ADABROKER_TRY
    if (Init_Ok) {
      // If already Initialized, call function on CPP_Object.
      CPP_Object->assertObjectExistent();
    } else {
      // Raise an Ada Exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::assertObjectExistent without initializing object.");
    }
  ADABROKER_CATCH 
}

//--------------------------//
// Ada_OmniObject::is_proxy //
//--------------------------//

_CORBA_Boolean
Ada_OmniObject::is_proxy() 
{
  if (Init_Ok) {
    // If already initialized, call function on CPP_Object.
    return CPP_Object->is_proxy();
  } else {
    // Raise an Ada Exception.
    throw omniORB::fatalException
      (__FILE__,
       __LINE__,
       "Ada_OmniObject::is_proxy without initializing object.");
  }
}

extern _CORBA_Boolean dispatch(GIOP_S         &,
			       const char     * operation,
			       _CORBA_Boolean   response_expected);

extern _CORBA_Boolean Ada_Is_A(const char * repoid);
// See Ada implementation of OmniObject.

//---------------------------------//
// Ada_OmniObject::setRepositoryID //
//---------------------------------//

void
Ada_OmniObject::setRepositoryID(const char* repoId)
{
  ADABROKER_TRY
    if (Init_Ok) {
      // If already initialized, call function on CPP_Object.
      CPP_Object->PR_IRRepositoryId(repoId);
    } else {
      // Raise an Ada Exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::setRepositoryId without initializing object.");
    }
  ADABROKER_CATCH
}

//---------------------------------//
// Ada_OmniObject::getRepositoryID //
//---------------------------------//
const char*
Ada_OmniObject::getRepositoryID() 
{
  ADABROKER_TRY
    if (Init_Ok) {
      // If already initialized, call function on CPP_Object.
      const char *result = CPP_Object->NP_IRRepositoryId();

#ifdef DEBUG
      cerr << "Ada_OmniObject::getRepositoryID : " << result << endl;
#endif

      return result;
    } else {
      // Raise an Ada Exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniObject::getRepositoryId without initializing object.");
    }
  ADABROKER_CATCH
    // Never reach this code. Just a default return for dummy compilers.
    char* default_result = NULL;
    return default_result; 
}
 
//------------------------------------------------//
// Ada_OmniObject::Ada_resolve_initial_references //
//------------------------------------------------//

Ada_OmniObject*
Ada_OmniObject::Ada_resolve_initial_references(CORBA::ORB_ptr   theORB,
					       const char     * identifier)
{
  ADABROKER_TRY
    CORBA::Object_ptr obj = theORB->resolve_initial_references(identifier);
    omniObject *omniobj   = obj->PR_getobj();
    omniObject_C2Ada *adaobj = dynamic_cast<omniObject_C2Ada*>(omniobj);
  
    if (adaobj == 0) {
      return 0;
    } else {
      // Create a new Ada_OmniObject that points to the same
      // omniObject_C2Ada.
      return new Ada_OmniObject (adaobj, 0);
    }
  ADABROKER_CATCH
    // Never reach this code. Just a default return for dummy
    // compilers.
    Ada_OmniObject* default_result = NULL;
    return default_result; 
}

//--------------------------------------//
// Ada_OmniObject::string_to_ada_object //
//--------------------------------------//

Ada_OmniObject*
Ada_OmniObject::string_to_ada_object(const char *from)
{
  ADABROKER_TRY
    omniObject *objptr = omni::stringToObject(from);
    omniObject_C2Ada *adaobj = dynamic_cast<omniObject_C2Ada*>(objptr);
  
    if (adaobj == 0) {
      return 0;
    } else {
      // Create a new Ada_OmniObject that points to the same
      // omniObject_C2Ada.
      return new Ada_OmniObject (adaobj, 0);
    }
    ADABROKER_CATCH
      // Never reach this code. Just a default return for dummy
      // compilers.
      Ada_OmniObject* default_result = NULL;
      return default_result; 
}

//--------------------------------------//
// Ada_OmniObject::ada_object_to_string //
//--------------------------------------//

char*
Ada_OmniObject::ada_object_to_string(Ada_OmniObject* objptr)
{
  char * result;
  
  ADABROKER_TRY
    if ( objptr == 0 ) {
      return omni::objectToString(0);
    } else {
      if (objptr->Init_Ok) {
	
#ifdef DEBUG
	cerr << "invoke omni::objectToString" << endl;
#endif

	result = omni::objectToString(objptr->CPP_Object);

#ifdef DEBUG
	cerr << "invoke omni::objectToString done" << endl;
#endif

	return result;
      } else {
	throw omniORB::fatalException
	  (__FILE__,
	   __LINE__,
	   "Ada_OmniObject::ada_object_to_string without initializing object.");
      }
    }
  ADABROKER_CATCH
    // Never reach thsi code. Just a default return for dummy compilers.
    char* default_result = NULL;
    return default_result; 
}

//-----------------------------//
// Ada_OmniObject::iopProfiles //
//-----------------------------//

IOP::TaggedProfileList*
Ada_OmniObject::iopProfiles() 
{
  ADABROKER_TRY
    if (Init_Ok) {
      // If already initialized, call function on CPP_Object.
      return CPP_Object->iopProfiles();
    } else {
    // Raise an Ada Exception.
    throw omniORB::fatalException
      (__FILE__,
       __LINE__,
       "Ada_OmniObject::getRepositoryId without initializing object.");
    }
  ADABROKER_CATCH
    // Never reach this code. Just a default return for dummy
    // compilers.
    IOP::TaggedProfileList* default_result = NULL;
    return default_result; 
}

//-------------------------------//
// Ada_OmniObject::getOmniObject //
//-------------------------------//

omniObject_C2Ada*
Ada_OmniObject::getOmniObject() 
{
  if (Init_Ok) {
    return CPP_Object;
  } else {
    throw omniORB::fatalException
      (__FILE__,
       __LINE__,
       "Ada_OmniObject::getOmniObject without initializing object.");
  }
}

//-----------------------------------//
// Ada_OmniObject::ada_create_objref //
//-----------------------------------//

Ada_OmniObject*
Ada_OmniObject::ada_create_objref(const char             * repoId,
				  IOP::TaggedProfileList * profiles,
				  _CORBA_Boolean           release)
{
  ADABROKER_TRY
    
#ifdef DEBUG
    cerr << "Ada_OmniObject::createObject : enter" << endl;
#endif

    // omniORB believes that we just want to cast the result into a
    // CORBA::Object_ptr.
    omniObject *objptr = omni::createObjRef (repoId,
					     0, 
					     profiles,
					     release);
    omniObject_C2Ada *adaobj = dynamic_cast<omniObject_C2Ada*>(objptr);
    
    if (adaobj == 0) {
      return 0;
    } else {
      
#ifdef DEBUG
      cerr << "Ada_OmniObject::createObject : leave" << endl;
#endif

      // Create a new Ada_OmniObject that points to the same
      // omniObject_C2Ada.
      return new Ada_OmniObject (adaobj, 0);
    }
  ADABROKER_CATCH
    // Never reach this code. Just a default return for dummy
    // compilers.
    Ada_OmniObject* default_result = NULL;
    return default_result; 
}

#undef DEBUG
