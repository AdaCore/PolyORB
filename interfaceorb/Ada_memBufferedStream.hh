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

  MemBufferedStream *CPP_Object;
  // Pointer on the underlying memBufferedStream object
  // needed in Ada_Iop.cc
  
private:

  bool Init_Ok;
  // This flag tells whether an init function was called or not

  void* VTable;
  // This field is only used by Ada. It is needed to interface C++ and Ada  

};

