//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.5 $
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
#ifndef __ADA_IOP_H__
#define __ADA_IOP_H__

#include "Ada_netBufferedStream.hh"
#include "Ada_memBufferedStream.hh"

void marshall (IOP::TaggedProfileList* t, Ada_netBufferedStream &s);
// wrapper around IOP::TaggedProfile operator >>=
// (see IOP.h)


void unmarshall (IOP::TaggedProfileList* &t, Ada_netBufferedStream &s);
// wrapper around IOP::TaggedProfile operator <<=
// (see IOP.h)


void marshall (IOP::TaggedProfileList* t, Ada_memBufferedStream &s);
// wrapper around IOP::TaggedProfile operator >>=
// (see IOP.h)


void unmarshall (IOP::TaggedProfileList* &t, Ada_memBufferedStream &s);
// wrapper around IOP::TaggedProfile operator <<=
// (see IOP.h)


size_t NP_alignedSize (IOP::TaggedProfileList* t, size_t initialoffset);
// wrapper around IOP::TaggedProfile function NP_alignedSize
// (see IOP.h)


size_t length (IOP::TaggedProfileList* t);
// wrapper around IOP::TaggedProfile function length
// (see IOP.h)

#endif
