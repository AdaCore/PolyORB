////////////////////////////////////////////////////////////////////////////
////                                                                    ////
////     This class is both a C class and an Ada Class (see             ////
////     MemBufferedStream.ads). It is wrapped around MemBuffered-      ////
////     Stream in order to avoid the presence of non default           ////
////     constructors.                                                  ////
////     So, it provides the same functions as MemBufferedStream        ////
////     except that constructors are replaced by Init functions.       ////
////     It has also a pointer on the underlying MemBufferedStream      ////
////     object                                                         ////
////                                                                    ////
////                                                                    ////
////                Date : 02/25/99                                     ////
////                                                                    ////
////                authors : Sebastien Ponce                           ////
////                                                                    ////
////////////////////////////////////////////////////////////////////////////

#include "omniORB2/omniInternal.h"

class Ada_memBufferedStream {

public:

  Ada_memBufferedStream ();
  // Default Constructor
  
  void Init (size_t Bufsize);
  // Initialisation of Ada_memBufferedStream, calls the
  // underlying MemBufferedStream constructor
  
  
  static void marshall (_CORBA_Char a, Ada_memBufferedStream& s);
  // wrapper around inline friend inline void operator>>= 

  static void unmarshall (_CORBA_Char& a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Boolean b, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Boolean& b, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Short a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Short& a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_UShort a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_UShort& a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Long a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Long& a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_ULong a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_ULong& a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Float a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Float& a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator<<=
  
  static void marshall (_CORBA_Double a, Ada_memBufferedStream& s);
  // wrapper around friend inline void operator>>=
  
  static void unmarshall (_CORBA_Double& a, Ada_memBufferedStream& s);
  // wrapper around friend inliney void operator<<=

private:

  MemBufferedStream *C_Object;
  // Pointer on the underlying memBufferedStream object

  bool Init_Ok;
  // This flag tells whether an init function was called or not

  void* VTable;
  // This field is only used by Ada. It is needed to interface C++ and Ada  

};

extern void raise_ada_exception (const char *msg);
// this function allows C code to raise Ada exception
// It is implemented in Ada and only raise a No_Initialisation
// exception with the message msg. (see corba.ads)




