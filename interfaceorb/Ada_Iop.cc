#include "Ada_exceptions.hh"
#include "Ada_Iop.hh"

void
marshall(IOP::TaggedProfileList * t,
	 Ada_netBufferedStream  & s)
{
  ADABROKER_TRY

    if (t != NULL)
      *t >>= *(s.CPP_Object);
    else
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Iop::marshall: null TaggedProfileList");

  ADABROKER_CATCH
}


void
unmarshall(IOP::TaggedProfileList * & t,
	   Ada_netBufferedStream    & s)
{
  ADABROKER_TRY

    // Create the TaggedProfileList corresponding to the object. It
    // will be released by the ORB when the last reference to the
    // omniObject using this TaggedProfileList will be released. See
    // CORBA::UnMarshalObjRef in objectRef.cc.

    t = new IOP::TaggedProfileList();
    *t <<= *(s.CPP_Object);

  ADABROKER_CATCH
}


void
marshall(IOP::TaggedProfileList * t,
	 Ada_memBufferedStream  & s)
{
  ADABROKER_TRY

    if (t != NULL)
      *t >>= *(s.CPP_Object);
    else
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Iop::marshall: null TaggedProfileList");

  ADABROKER_CATCH
}


void
unmarshall(IOP::TaggedProfileList * & t,
	   Ada_memBufferedStream    & s)
{
  ADABROKER_TRY

    // Create TaggedProfileList corresponding to object. It will be
    // released by the ORB when the last reference to the omniObject
    // using this TaggedProfileList will be released. See
    // CORBA::UnMarshalObjRef in objectRef.cc.
    
    t = new IOP::TaggedProfileList();
    *t <<= *(s.CPP_Object);

  ADABROKER_CATCH
}


size_t
NP_alignedSize (IOP::TaggedProfileList* t, size_t initialoffset)
{
  ADABROKER_TRY

    if (t != NULL)
      return t->NP_alignedSize (initialoffset);
    else
      //  Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Iop::NP_alignedSize: null TaggedProfileList");

  ADABROKER_CATCH

    // Never reach this code. Just a default return for dummy
    // compilers.
    size_t default_result = 0;
    return default_result; 
}


size_t
length (IOP::TaggedProfileList* t)
{
  ADABROKER_TRY

    if (t != NULL)
      return t->length ();
    else
      // Raise an Ada exception.
      throw omniORB::fatalException
	(__FILE__,
	 __LINE__,
	 "Ada_Iop::length: null TaggedProfileList");

  ADABROKER_CATCH
    // Never reach this code. Just a default return for dummy
    // compilers.
    size_t default_result = 0;
    return default_result; 
}

