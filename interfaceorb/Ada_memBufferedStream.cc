#include "Ada_memBufferedStream.hh"
#include "Ada_exceptions.hh"

//----------------------------------------------//
// Ada_memBufferedStream::Ada_memBufferedStream //
//----------------------------------------------//

Ada_memBufferedStream::Ada_memBufferedStream ()
{
  Init_Ok = false;
};

  
//-----------------------------//
// Ada_memBufferedStream::Init //
//-----------------------------//

void
Ada_memBufferedStream::Init (size_t Bufsize)
{
  ADABROKER_TRY

    // Create underlying omniobject_C2Ada object.
    CPP_Object = new MemBufferedStream (Bufsize);

    // Update Init_OK flag
    Init_Ok = true;
    return;

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Char a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::marshall : initialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Char& a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialited, call the corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Boolean a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized, call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Boolean& a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Short a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY

    if (s.Init_Ok) {
      // If already initialized then call corresponding function on
      // CPP_Object.
      a >>= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Short& a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_UShort a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_UShort& a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY
    
    if (s.Init_Ok) {
      // If already uninitialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Long a, Ada_memBufferedStream& s)
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
       "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Long& a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_ULong a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_ULong& a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Float a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Float& a, Ada_memBufferedStream& s)
{
  ADABROKER_TRY
    
    if (s.Init_Ok) {
      // It already initialized, then call corresponding function on
      // CPP_Object.
      a <<= *(s.CPP_Object);

    } else {
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//---------------------------------//
// Ada_memBufferedStream::marshall //
//---------------------------------//

void
Ada_memBufferedStream::marshall (_CORBA_Double a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::marshall : uninitialized object.");
    }

  ADABROKER_CATCH
}


//-----------------------------------//
// Ada_memBufferedStream::unmarshall //
//-----------------------------------//

void
Ada_memBufferedStream::unmarshall (_CORBA_Double& a, Ada_memBufferedStream& s)
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
	 "Ada_memBufferedStream::unmarshall : uninitialized object.");
    }

  ADABROKER_CATCH
}
