//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.2 $
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

adabe_interface_fwd::adabe_interface_fwd (UTL_ScopedName *n, UTL_StrList *p)
  : AST_InterfaceFwd (n, p),
    AST_Decl (AST_Decl::NT_interface_fwd, n, p),
    adabe_name (AST_Decl::NT_interface_fwd, n, p)
{
}
  
static string remove_dot (string  name)
{
  char c;
  while ((c = name.find (".")) != -1) 
    name[c]='-';
  return name;
}
void
adabe_interface_fwd::produce_ads (dep_list& with, string &body, string &previous)
{
  //  compute_ada_name ();  
  //  dynamic_cast<adabe_name *>(full_definition ())->set_ada_local_name (get_ada_local_name ()); 
  //  dynamic_cast<adabe_name *>(full_definition ())->set_ada_full_name (get_ada_full_name ());
  string file_name = remove_dot (get_ada_full_name ()) + "_forward.ads";
  char *lower_case_name = lower (file_name.c_str ());
  ofstream file (lower_case_name);
  delete[] lower_case_name;
  file << "with CORBA.Forward;\n";
  file << "package " + get_ada_full_name () + "_Forward is new CORBA.Forward;\n";
  file.close ();  
}

/*
  void
  adabe_interface_fwd::produce_ads (dep_list& with, string &body, string &previous)
  {
  produce_ads ( with, body, previous);
  }
*/

IMPL_NARROW_METHODS1 (adabe_interface_fwd, AST_InterfaceFwd)
IMPL_NARROW_FROM_DECL (adabe_interface_fwd)
IMPL_NARROW_FROM_SCOPE (adabe_interface_fwd)









