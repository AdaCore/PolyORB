//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.17 $
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
#ifndef __ADA_OMNIROPEANDKEY_H__
#define __ADA_OMNIROPEANDKEY_H__

#include <omniORB2/omniInternal.h>

class Ada_OmniRopeAndKey {
  
public:

  Ada_OmniRopeAndKey ();
  // Default Constructor
  
  void Init ();
  // Initialisation of Ada_OmniRopeAndKey, calls the
  // underlying omniRopeAndKey constructor
  
  void Free() ;
  // deletes the underlying CPP_Object

  Rope* rope();
  // wrapper around inline Rope* rope() const

  _CORBA_Octet* key();
  // wrapper around inline _CORBA_Octet* key() const
  
  _CORBA_ULong keysize();
  // wrapper around inline _CORBA_ULong keysize() const

  _CORBA_Boolean equals(Ada_OmniRopeAndKey other) ;
  // compares two objects, imported in Ada
  
  omniRopeAndKey *CPP_Object;
  // Pointer on the underlying Rope object

  bool assertInit_Ok () const ;
  // returns init_Ok;
  
private:

  bool Init_Ok;
  // This flag tells whether an init function was called or not

  void* VTable;
  // This field is only used by Ada. It is needed to interface C++ and Ada  
  
};

#endif
