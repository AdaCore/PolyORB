//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.29 $
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
#include <strstream>

IMPL_NARROW_METHODS1 (adabe_string, AST_String);
IMPL_NARROW_FROM_DECL (adabe_string);

static string to_string (AST_Expression::AST_ExprValue *exp)
{
  char temp[10]; 
  switch ( exp->et ) {
  case AST_Expression::EV_short:
    sprintf (temp, "%d", exp->u.sval);
    break;
  case AST_Expression::EV_ushort:
    sprintf (temp, "%d", exp->u.usval);
    break;
  case AST_Expression::EV_long:
    sprintf (temp, "%ld", exp->u.lval);
    break;
  case AST_Expression::EV_ulong:
    sprintf (temp, "%ld", exp->u.ulval);
    break;
  default:
    throw adabe_internal_error
      (__FILE__,__LINE__,"Unexpected string dimension type");
  }
  //  temp.freeze (temp.pcount ());
  return temp;
}

static int evaluate (AST_Expression::AST_ExprValue *exp)
{
  switch ( exp->et ) {
  case AST_Expression::EV_short:
    return exp->u.sval;
    break;
  case AST_Expression::EV_ushort:
    return exp->u.usval;
    break;
  case AST_Expression::EV_long:
    return exp->u.lval;
    break;
  case AST_Expression::EV_ulong:
    return exp->u.ulval;
    break;
  default:
    throw adabe_internal_error 
      (__FILE__,__LINE__,"Unexpected string dimension type");
  }
}

adabe_string::adabe_string (AST_Expression *v):
  AST_String (v),
  AST_Decl (AST_Decl::NT_string, new UTL_ScopedName 
	    (new Identifier ("string", 1, 0, I_FALSE), NULL), NULL),
  adabe_name (AST_Decl::NT_string, new UTL_ScopedName
	      (new Identifier ("string", 1, 0, I_FALSE), NULL), NULL)
{
}

adabe_string::adabe_string (AST_Expression *v, long wide):
  AST_String (v, wide),
  AST_Decl (AST_Decl::NT_string, new UTL_ScopedName
	    (new Identifier ("string", 1, 0, I_FALSE), NULL), NULL),
  adabe_name (AST_Decl::NT_string, new UTL_ScopedName
	      (new Identifier ("string", 1, 0, I_FALSE), NULL), NULL)
{
}

string
adabe_string::local_type ()
{
  bool find = false;
  UTL_Scope *parent_scope = defined_in ();
  UTL_ScopeActiveIterator parent_scope_activator
    (parent_scope, UTL_Scope::IK_decls);
  adabe_name *decl =
    dynamic_cast<adabe_name *>(parent_scope_activator.item ());
  do
    {
      switch (decl->node_type ())
	{
	case AST_Decl::NT_field:
	case AST_Decl::NT_argument:
	  if (dynamic_cast<AST_Field *>(decl)->field_type () == this)
	    find = true;
	  break;
	case AST_Decl::NT_op:
	  if (dynamic_cast<AST_Operation *>(decl)->return_type () == this)
	    find =true;
	  break;
	default:
	  break;
	}
      parent_scope_activator.next ();
      if (!find)
	decl = dynamic_cast<adabe_name *>(parent_scope_activator.item ());
    }
  while (!find && !(parent_scope_activator.is_done ()));
  if (find)
    return decl->get_ada_local_name () +"_String";

  return "local_type";
}

void adabe_string::produce_ads (dep_list & with,
				string   & body,
				string   & previous)
{
  //  with.add ("CORBA.Bounded_Strings");
  
  if (evaluate (max_size ()->ev ())==0)
    {
      body+= "   type " + get_ada_local_name () + " is new CORBA.String;\n\n";
    }
  else
    {
      // PROBLEM TO SOLVE
      body += "   package CORBA.Bounded_Strings_"
	   + to_string (max_size ()->ev ());
      body += " is\n";
      body += "    new CORBA.Bounded_Strings (";
      body +=  to_string (max_size ()->ev ());
      body += ");\n\n";
      body += "   type "+ get_ada_local_name () + " is\n";
      body += "      new CORBA.Bounded_Strings_";
      body += to_string (max_size ()->ev ());
      body += ".Bounded_String;\n\n";
    }

  // body += "   type " + get_ada_local_name () + "_Ptr is access ";
  // body += get_ada_local_name () + ";\n\n";
  // body += "   procedure Free is new Ada.Unchecked_Deallocation (";
  // body += get_ada_local_name () + ", " + get_ada_local_name ()+ "_Ptr);\n\n\n";
  set_already_defined ();
  
  set_already_defined ();
}


void  
adabe_string::produce_stream_ads (dep_list & with,
				  string   & body,
				  string   & previous)
{
  body += "   procedure Marshall\n";
  body += "     (A : in " + get_ada_local_name () + ";\n";
  body += "      S : in out AdaBroker.NetBufferedStream.Object'Class);\n\n";

  body += "   procedure Unmarshall\n";
  body += "     (A : out " + get_ada_local_name () + ";\n";
  body += "      S : in out AdaBroker.NetBufferedStream.Object'Class);\n\n";

  body += "   function Align_Size\n";
  body += "     (A              : in " + get_ada_local_name () + ";\n";
  body += "      Initial_Offset : in CORBA.Unsigned_Long;\n";
  body += "      N              : in CORBA.Unsigned_Long := 1)\n";
  body += "      return CORBA.Unsigned_Long;\n\n";

  set_already_defined ();
}


void 
adabe_string::produce_stream_adb (dep_list & with,
				  string   & body,
				  string   & previous)
{
  string tmp="";

  body += "   procedure Marshall\n";
  body += "     (A : in " + get_ada_local_name () + ";\n";
  body += "      S : in out AdaBroker.NetBufferedStream.Object'Class) is\n";
  body += "   begin\n";
  body += "      Marshall (CORBA.String (A), S);\n";
  body += "   end Marshall;\n\n";

  body += "   procedure Unmarshall\n";
  body += "     (A : out " + get_ada_local_name () + ";\n";
  body += "      S : in out AdaBroker.NetBufferedStream.Object'Class) is \n\n";
  body += "   begin\n";
  body += "      Unmarshall (CORBA.String (A), S);\n";
  body += "   end Unmarshall;\n\n";

  body += "   function Align_Size\n";
  body += "     (A              : in " + get_ada_local_name () + ";\n";
  body += "      Initial_Offset : in CORBA.Unsigned_Long;\n";
  body += "      N              : in CORBA.Unsigned_Long := 1)\n";
  body += "      return CORBA.Unsigned_Long is \n";
  body += "   begin\n";
  body += "      return Align_Size (CORBA.String (A), Initial_Offset, N);\n";
  body += "   end Align_Size;\n\n";

  set_already_defined ();
}

string 
adabe_string::dump_name (dep_list & with,
			 string   & previous)
{
  UTL_Scope *temp = defined_in ();
  if (!is_imported (with))
    {
      if (evaluate (max_size ()->ev ())==0)
	{
	  if ((defined_in () == NULL) ||
	      (dynamic_cast<AST_Decl *> (defined_in ())->node_type () 
					 != AST_Decl::NT_typedef))
	    {
	      set_already_defined ();
	      with.add ("CORBA");
	      return "CORBA.String";
	    }
	}
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

string adabe_string::marshal_name (dep_list & with,
				   string   & previous)
{
  if (!is_marshal_imported (with))
    {
      if (evaluate (max_size ()->ev ())==0)
	{
	  if ((defined_in () == NULL) ||
	      (dynamic_cast<AST_Decl *> (defined_in ())->node_type () 
	       != AST_Decl::NT_typedef))
	    {
	      set_already_defined ();
	      with.add ("CORBA");
	      return "CORBA.String";
	    }
	}
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
