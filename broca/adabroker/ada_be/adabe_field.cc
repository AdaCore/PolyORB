//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.1 $
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

adabe_field::adabe_field (AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p)
	: AST_Field (ft, n, p),
	  AST_Decl (AST_Decl::NT_field, n, p),
	  adabe_name (AST_Decl::NT_field, n, p)
{
}

void
adabe_field::produce_ads (dep_list& with, string &body, string &previous) 
{
  compute_ada_name ();
  body += "      " + get_ada_local_name ();
  body += " : ";
  AST_Decl *b = field_type ();
#ifdef DEBUG_FIELD
  cerr << "before the dump name of the field " << endl;
#endif 
  body += dynamic_cast<adabe_name *>(b)->dump_name (with, previous); 
#ifdef DEBUG_FIELD
  cerr << "after the dump name of the field " << endl;
#endif 
  body += ";\n";
}

void
adabe_field::produce_stream_adb (dep_list& with, string &body, string &marshall, string &unmarshall, string &marshall_size)
{
  string previous = "";
  adabe_name *e = dynamic_cast<adabe_name *>(field_type ());
  string name = e->marshal_name (with, previous);
  
  body += previous;
  
  marshall += 
    "      Marshall (Stream, Val." +  get_ada_local_name () + ");\n";
  
  unmarshall += 
    "      Unmarshall (Stream, Res." + get_ada_local_name () + ");\n";
  
  marshall_size +=
    "      Marshall_Size (Stream, Val." +  get_ada_local_name () + ");\n";

  set_already_defined ();
}
  
IMPL_NARROW_METHODS1 (adabe_field, AST_Field)
IMPL_NARROW_FROM_DECL (adabe_field)


