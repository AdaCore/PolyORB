//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.11 $
//                                                                          //
//         Copyright (C) 1999-2000 ENST Paris University, France.           //
//                                                                          //
// AdaBroker is free software; you  can  redistribute  it and/or modify it  //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. AdaBroker  is distributed  in the hope that it will be  useful, //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with AdaBroker; see file COPYING. If  //
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, //
// Boston, MA 02111-1307, USA.                                              //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//             AdaBroker is maintained by ENST Paris University.            //
//                     (email: broker@inf.enst.fr)                          //
//                                                                          //
//--------------------------------------------------------------------------//
#ifndef __ADA_MEMBUFFEREDSTREAM_H__
#define __ADA_MEMBUFFEREDSTREAM_H__

#include <omniORB2/omniInternal.h>

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

#endif
