#include "Ada_netBufferedStream.hh"
#include "Ada_exceptions.hh"


//----------------------------------------------//
// Ada_netBufferedStream::Ada_netBufferedStream //
//----------------------------------------------//

Ada_netBufferedStream::Ada_netBufferedStream ()
{
  CPP_Object = 0;
  Init_Ok = false;
};


//-----------------------------//
// Ada_netBufferedStream::Init //
//-----------------------------//

void
Ada_netBufferedStream::Init (Rope *r, _CORBA_Boolean RdLock,
                             _CORBA_Boolean WrLock, size_t Bufsize)
{
  ADABROKER_TRY

    // Create underlying omniobject_C2Ada object.
    CPP_Object = new NetBufferedStream (r,RdLock,WrLock,Bufsize);

    // Update Init_OK flag.
    Init_Ok = true;
    return;

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Char a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Char& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Boolean a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Boolean& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Short a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Short& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_UShort a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_UShort& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY
  
    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Long a, Ada_netBufferedStream &s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}

//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Long& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<=  *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_ULong a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY
    
    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_ULong& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream:;marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Float a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Float& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_netBufferedStream::marshall //
//---------------------------------//

void
Ada_netBufferedStream::marshall (_CORBA_Double a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);
    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_netBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_netBufferedStream::unmarshall (_CORBA_Double& a, Ada_netBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//----------------------------------------------------//
// Ada_netBufferedStream::isReUsingExistingConnection //
//----------------------------------------------------//

_CORBA_Boolean
Ada_netBufferedStream::isReUsingExistingConnection()
{
  ADABROKER_TRY

    if (Init_Ok) {
      // If already initialized, then call corresponding function on
      // CPP_Object.
      return CPP_Object->isReUsingExistingConnection();

    } else {
      //  Raise an Ada exception,
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_netBufferedStream::isReUsingExistingConnection : uninitialized object.");
    }

  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy
    // compilers.
    CORBA::Boolean default_result = false;
    return default_result; 
}







