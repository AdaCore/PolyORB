//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.8 $
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
#include <adabe.h>

  
IMPL_NARROW_METHODS1 (adabe_enum_val, AST_EnumVal)
IMPL_NARROW_FROM_DECL (adabe_enum_val)

adabe_enum_val::adabe_enum_val (unsigned long v, UTL_ScopedName *n, UTL_StrList *p): AST_Decl (AST_Decl::NT_enum_val, n, p),
    AST_Constant (AST_Expression::EV_ulong,
		 AST_Decl::NT_enum_val,
		 new AST_Expression (v),
		 n,
		 p),
 
  AST_EnumVal (v, n, p),
  adabe_name (AST_Decl::NT_enum_val, n, p)
{
}


string
adabe_enum_val::dump_name (dep_list& with, string &previous)
{
  return get_ada_local_name ();
}

string
adabe_enum_val::marshal_name (dep_list& with, string &previous)
{
  return get_ada_local_name ();
}

