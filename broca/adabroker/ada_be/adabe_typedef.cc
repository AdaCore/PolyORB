//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.4 $
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

adabe_typedef::adabe_typedef (AST_Type       * bt,
			      UTL_ScopedName * n,
			      UTL_StrList    * p)
  : AST_Typedef (bt, n, p),
    AST_Decl (AST_Decl::NT_typedef, n, p),
    adabe_name (AST_Decl::NT_typedef, n, p)

{
  pd_number_value = 0;
}

void
adabe_typedef::produce_ads (dep_list & with,
			    string   & body,
			    string   & previous)
{
  compute_ada_name ();
  AST_Decl *b = base_type ();
  adabe_name *c = dynamic_cast<adabe_name *>(b);
  if (((string) b->local_name ()->get_string ()) == "local type")
    {
      switch (b->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_string:
	case AST_Decl::NT_union:
	  {
	    c->set_ada_local_name (get_ada_local_name ());
	    c->set_ada_full_name (get_ada_full_name ());
	    c->produce_ads (with, body, previous);
	    break;
	  }
        case AST_Decl::NT_interface:
        case AST_Decl::NT_interface_fwd:
          {
	    body += "   subtype " + get_ada_local_name () + " is ";
	    string name = 
	      dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
	    body += name;
	    body += ";\n\n";
	    break;
          }
	default:
          {
            char c[2] = { ('a' + (unsigned) b->node_type ()), '\0' };
            body += "   -- base type kind: ";
            body += ((char*)&c);
            body += "\n";
	    body += "   type " + get_ada_local_name () + " is new ";
	    string name = 
	      dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
	    body += name;
	    body += ";\n\n";
	    break;
          }
	}
    }
  else
    {
      switch (b->node_type ())
        {
        case AST_Decl::NT_interface:
        case AST_Decl::NT_interface_fwd:
          {
	    body += "   subtype " + get_ada_local_name () + " is ";
	    string name = 
	      dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
	    body += name;
	    body += ";\n\n";
	    break;
          }
	default:
          {
            char c[2] = { ('a' + (unsigned) b->node_type ()), '\0' };
            body += "   -- 2 base type kind: ";
            body += ((char*)&c);
            body += "\n";
	    body += "   type " + get_ada_local_name () + " is new ";
	    string name = 
	      dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
	    body += name;
	    body += ";\n\n";
	    break;
          }
      }
    }
  if (!c->has_fixed_size ()) no_fixed_size ();
  switch (b->node_type ())
    {
    case AST_Decl::NT_typedef:
      set_number_value (dynamic_cast<adabe_typedef *>(b)->get_number_value ());
      break;
    case AST_Decl::NT_enum:
      set_number_value (dynamic_cast<adabe_enum *>(b)->get_number_value ());
      break;
    default:
      break;      
    }  
  set_already_defined ();
}

void
adabe_typedef::produce_stream_ads (dep_list & with,
				   string   & body,
				   string   & previous)
{
  AST_Decl *b = base_type ();
  if (((string) b->local_name ()->get_string ()) == "local type")
    {
      switch (b->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_string:
	case AST_Decl::NT_union:
	  {
	    string arg2 = "";
	    adabe_name *c = dynamic_cast<adabe_name *>(b);
	    c->produce_stream_ads (with, body, arg2);
	    body += arg2;
	    set_already_defined ();
	    return;
	  }
	default : {}
	}
    }

  gen_marshalling_declarations (body, get_ada_local_name ());

  set_already_defined ();
}

void
adabe_typedef::produce_stream_adb (dep_list & with,
				   string   & body,
				   string   & previous)
{
  string arg2 = "";
  AST_Decl *b = base_type ();
  if (((string) b->local_name ()->get_string ()) == "local type")
    {
      switch (b->node_type ())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_string:
	case AST_Decl::NT_union:
	  {
	    adabe_name *c = dynamic_cast<adabe_name *>(b);
	    c->produce_stream_adb (with, body, arg2);
	    body += arg2;
	    set_already_defined ();
	    return;
	  }
	  default : {}
	}
    }
  
  string name = (dynamic_cast<adabe_name *> (base_type ()))->marshal_name (with, arg2); 
  body += arg2;
  body +=
    "   procedure Marshall\n"
    "      (Stream : in out Broca.Types.Buffer_descriptor;\n"
    "       Val : " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n"
    "      Marshall (Stream, " + name + "(Val));\n"
    "   end Marshall;\n"
    "\n"
    "   procedure Unmarshall\n"
    "      (Stream : in out Broca.Types.Buffer_descriptor;\n"
    "       Res : out " + get_ada_local_name () + ")\n"
    "   is\n"
    "      Tmp : " + name + ";\n"
    "   begin\n"
    "      Unmarshall (Stream, Tmp);\n"
    "      Res := " + get_ada_local_name () + "(Tmp);\n"
    "   end Unmarshall;\n"
    "\n"
    "   procedure Marshall_Size\n"
    "      (Stream : in out Broca.Types.Buffer_descriptor;\n"
    "       Val : " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n"
    "      Marshall_Size (Stream, " + name + "(Val));\n"
    "   end Marshall_Size;\n"
    "\n";

  set_already_defined ();
}

string
adabe_typedef::dump_name (dep_list & with,
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
adabe_typedef::marshal_name (dep_list & with,
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
IMPL_NARROW_METHODS1 (adabe_typedef, AST_Typedef)
IMPL_NARROW_FROM_DECL (adabe_typedef)
