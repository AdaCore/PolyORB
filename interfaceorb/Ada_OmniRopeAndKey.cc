#include "Ada_OmniRopeAndKey.hh"
#include "Ada_exceptions.hh"


//----------------------------------------//
// Ada_OmniRopeAndKey::Ada_OmniRopeAndKey //
//----------------------------------------//

Ada_OmniRopeAndKey::Ada_OmniRopeAndKey ()
{
  Init_Ok = false;
  CPP_Object = 0;
};


//--------------------------//
// Ada_OmniRopeAndKey::Init //
//--------------------------//

void
Ada_OmniRopeAndKey::Init ()
{
  ADABROKER_TRY

    if (CPP_Object) {
      delete CPP_Object;
    }
    CPP_Object = new omniRopeAndKey ();
    Init_Ok = true;

  ADABROKER_CATCH
}

//--------------------------//
// Ada_OmniRopeAndKey::Free //
//--------------------------//

void
Ada_OmniRopeAndKey::Free ()
{
  ADABROKER_TRY

    if (CPP_Object) {
      delete CPP_Object;
    }
    Init_Ok = false;

  ADABROKER_CATCH
}


//--------------------------//
// Ada_OmniRopeAndKey::rope //
//--------------------------//

Rope*
Ada_OmniRopeAndKey::rope()
{
  ADABROKER_TRY

    if (Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      return CPP_Object->rope();

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniRopeAndKey::rope : uninitialized object.");
  }

  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy
    // compilers.
    Rope* default_result = NULL;
    return default_result; 
}


//-------------------------//
// Ada_OmniRopeAndKey::key //
//-------------------------//

_CORBA_Octet*
Ada_OmniRopeAndKey::key()
{
  ADABROKER_TRY

    if (Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      return CPP_Object->key();

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniRopeAndKey::key : uninitialized object.");
    }

  ADABROKER_CATCH 

    // Never reach this. Just a default return for dummy compilers.
    _CORBA_Octet* default_result = NULL;
    return default_result; 
}


//----------------------------//
// Ada_OmniRopeAndKey:keysize //
//----------------------------//

_CORBA_ULong
Ada_OmniRopeAndKey::keysize()
{
  ADABROKER_TRY

    if (Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      return CPP_Object->keysize();

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniRopeAndKey::keysize : uninitialized object.");
    }
  
  ADABROKER_CATCH 

    // Never reach this code. Just a default return for dummy
    // compilers.
    _CORBA_ULong default_result = 0;
    return default_result; 
}


//----------------------------//
// Ada_OmniRopeAndKey::equals //
//----------------------------//

_CORBA_Boolean
Ada_OmniRopeAndKey::equals(Ada_OmniRopeAndKey other) 
{
  ADABROKER_TRY

    if ((Init_Ok) && (other.assertInit_Ok())) {
      // I already initialized then compare effectively the two
      // objects. This code is pasted from corbaObject.cc L160.
      if (CPP_Object->keysize() != other.CPP_Object->keysize() ||
	  memcmp((void*)(CPP_Object->key()),(void*)(other.CPP_Object->key()),
		 CPP_Object->keysize()) != 0) {
	return 0;
      }
      
      if (CPP_Object->rope() == other.CPP_Object->rope())
	return 1;
      else
	return 0;      
      
    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_OmniRopeAndKey::equals : uninitialized object.");
    }

  ADABROKER_CATCH 

    // Never reach this code. Just a default return for dummy
    // compilers.
    _CORBA_Boolean default_result = false;
    return default_result; 
}


//-----------------------------------//
// Ada_OmniRopeAndKey::assertInit_Ok //
//-----------------------------------//

bool
Ada_OmniRopeAndKey::assertInit_Ok () const {
  return Init_Ok;
}
