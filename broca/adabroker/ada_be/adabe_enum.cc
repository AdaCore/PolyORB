//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.3 $
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

  
IMPL_NARROW_METHODS1 (adabe_enum, AST_Enum);
IMPL_NARROW_FROM_DECL (adabe_enum);
IMPL_NARROW_FROM_SCOPE (adabe_enum);

adabe_enum::adabe_enum (UTL_ScopedName * n,
			UTL_StrList    * p)
  : AST_Enum (n, p),
    AST_Decl (AST_Decl::NT_enum, n, p),
    UTL_Scope (AST_Decl::NT_enum),
    adabe_name (AST_Decl::NT_enum, n, p)
{
  pd_number_value = 0;
}

void
adabe_enum::produce_ads (dep_list & with,
			 string   & body,
			 string   & previous)
{
  int numb = 0;
  // number of enum values
  
  compute_ada_name ();
  body += "   type " + get_ada_local_name () + " is\n";
  body += "     (";
  UTL_ScopeActiveIterator activator (this, UTL_Scope::IK_decls);
  while (!activator.is_done ())
    {
      AST_Decl *d = activator.item ();
      activator.next ();
      switch (d->node_type ())
	{
	case AST_Decl::NT_enum_val:
	  numb++;
	  body+=adabe_enum_val::narrow_from_decl (d)->dump_name (with, previous);
	  break;
	default:
	  throw adabe_internal_error
	    (__FILE__,__LINE__,"unexpected scope in enumeration type");
	}
      if (!activator.is_done ()) body += ",\n      ";
    }
  set_number_value (numb);
  // set the number of enum values
  body +=");\n\n";
  // body += "   type " + get_ada_local_name () + "_Ptr is access ";
  // body += get_ada_local_name () + ";\n\n";
  // body += "   procedure Free is new Ada.Unchecked_Deallocation (";
  // body += get_ada_local_name () + ", " + get_ada_local_name ()+ "_Ptr);\n\n\n";  
  set_already_defined ();
}

void  
adabe_enum::produce_stream_ads (dep_list & with,
				string   & body,
				string   & previous)
{
  gen_marshalling_declarations (body, get_ada_local_name ());

  set_already_defined ();
}


void 
adabe_enum::produce_stream_adb (dep_list & with,
				string   & body,
				string   & previous)
{
  body +=
    "   procedure Marshall\n"
    "      (Stream : in out Buffer_Descriptor;\n"
    "       Val : " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n"
    "      Marshall\n"
    "        (Stream,\n"
    "         CORBA.Unsigned_Long (" + get_ada_local_name () + 
    "'Pos (Val)));\n"
    "   end Marshall;\n"
    "\n"
    "   procedure Unmarshall\n"
    "      (Stream : in out Buffer_Descriptor;\n"
    "       Res : out " + get_ada_local_name () + ")\n"
    "   is \n"
    "      Tmp : CORBA.Unsigned_Long;\n"
    "   begin\n"
    "      Unmarshall (Stream, Tmp, S);\n"
    "      A := " + get_ada_local_name () + "'Val (Tmp);\n"
    "   end Unmarshall;\n"
    "\n"
    "   procedure Compute_New_Size\n"
    "      (Stream : in out Buffer_Descriptor;\n"
    "       Val : " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n"
    "      Compute_New_Size (Stream, UL_Size, UL_Size);\n"
    "   end Compute_New_Size;\n"
    "\n";

  set_already_defined ();
}

string
adabe_enum::dump_name (dep_list & with,
		       string   & previous) 
{
   if (!is_imported (with))
    {
      if (!is_already_defined ())
	{
	  string tmp = "";
	  produce_ads (with, tmp, previous);
	  previous += tmp;
	}
      return get_ada_local_name ();
    }
  return get_ada_full_name ();	   
}

string
adabe_enum::marshal_name (dep_list & with,
			  string   & previous) 
{
   if (!is_marshal_imported (with))
    {
      if (!is_already_defined ())
	{
	  string tmp = "";
	  produce_stream_adb (with, tmp, previous);
	  previous += tmp;
	}
      return get_ada_local_name ();
    }
  return get_ada_full_name ();	   
}
