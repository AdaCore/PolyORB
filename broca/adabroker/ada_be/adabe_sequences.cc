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
    throw adabe_internal_error (__FILE__,__LINE__,"Unexpected string dimension type");
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
    throw adabe_internal_error (__FILE__,__LINE__,"Unexpected string dimension type");
  }
}

void
adabe_sequence::produce_ads (dep_list& with, string &body,
			    string &previous)
{
  static int count = 0;
  char count_str[4];
  string is_bounded;
  adabe_name *adabe_base_type;

  string seq_size_st = to_string (max_size ()->ev ());
  int seq_size = evaluate (max_size ()->ev ());
  bool bounded = (seq_size != 0);
  if (bounded)
    is_bounded = "Bounded";
  else
    is_bounded = "Unbounded";

  with.add ("CORBA.Sequences." + is_bounded);

  // Writing the header :

  adabe_base_type =  dynamic_cast<adabe_name *> (base_type ());
  string type_name =  adabe_base_type->dump_name (with, previous);
  string short_type_name = type_name.substr (type_name.find_last_of ('.') + 1);
  if (short_type_name == "Sequence") 
    {
      short_type_name = type_name.substr (type_name.find_first_of ('_') + 1);
      short_type_name = short_type_name.substr (0, short_type_name.length ()-9);
    }
  if (count > 0)
    {
      sprintf (count_str,"%d", count);
      short_type_name += "_";
      short_type_name += count_str;
    }
  count ++;
  set_ada_local_name ("IDL_Sequence_" + short_type_name + ".Sequence");
  //  body += "   type IDL_SEQUENCE_" + short_type_name +"_Array is ";
  //  if (bounded)
  //    body += "array (1.." + seq_size_st + ") of " + type_name +";\n";
  //  else 
  //    body += "array (Integer range <>) of " + type_name +";\n";
  //  body += "\n";
  body += "   package IDL_Sequence_" + short_type_name + " is\n";
  body += "      new ";
  body += "CORBA.Sequences." + is_bounded;
  if (bounded) {
    body += "(" + type_name + ", ";
    body += seq_size_st + ");\n\n";
  }
  else
    body += "(" + type_name +");\n\n";
  set_already_defined ();
}
void
adabe_sequence::produce_stream_ads (dep_list & with,
				    string   & body,
				    string   & previous)
{
  gen_marshalling_declarations (body, get_ada_local_name ());
  set_already_defined ();
}

void
adabe_sequence::produce_stream_adb (dep_list & with,
				    string   & body,
				    string & previous)
{
  string name =
    (dynamic_cast<adabe_name *> (base_type ()))->marshal_name (with, body); 
  string type_name = get_ada_local_name ();
  string inter_name = type_name.substr (0, type_name.find_first_of ('.'));
  
  body +=
    "   procedure Marshall\n"
    "     (Stream : in out Broca.Types.Buffer_Descriptor;\n"
    "      Val : in " + type_name + ")\n" 
    "   is\n"
    "      Len : CORBA.Unsigned_Long\n"
    "        := CORBA.Unsigned_Long (" + inter_name + ".Length (Val));\n"
    "   begin\n"
    "      Marshall (Stream, Len);\n"
    "      for I in 1 .. Len loop\n"
    "         Marshall\n"
    "            (Stream, " + inter_name + ".Element_Of (Val, Integer (I)));\n"
    "      end loop;\n"
    "   end Marshall;\n"
    "\n"
    "   procedure Unmarshall\n"
    "     (Stream : in out Broca.Types.Buffer_Descriptor;\n"
    "      Res : out " + type_name + ")\n"
    "   is\n"
    "      Len : CORBA.Unsigned_Long;\n"
    "      Tmp : " + type_name + ";\n"
    "      El : " + name + ";\n"
    "   begin\n"
    "      Unmarshall (Stream, Len);\n"
    "      Tmp := " + inter_name + ".To_Sequence (Natural (Len));\n"
    "      for I in 1 .. Len loop\n"
    "         Unmarshall (Stream, El);\n"
    "         " + inter_name + ".Replace_Element (Tmp, Positive (Len), El);\n"
    "      end loop;\n"
    "      Res := Tmp;\n"
    "   end Unmarshall;\n"
    "\n"
    "   procedure Marshall_Size\n"
    "     (Stream : in out Broca.Types.Buffer_Descriptor;\n"
    "      Val : in " + type_name + ")\n" 
    "   is\n"
    "      Len : CORBA.Unsigned_Long\n"
    "        := CORBA.Unsigned_Long (" + inter_name + ".Length (Val));\n"
    "   begin\n"
    "      Marshall_Size_Unsigned_Long (Stream);\n"
    "      for I in 1 .. Len loop\n"
    "         Marshall_Size\n"
    "            (Stream, " + inter_name + ".Element_Of (Val, Integer (I)));\n"
    "      end loop;\n"
    "   end Marshall_Size;\n";
  set_already_defined ();
}

string
adabe_sequence::dump_name (dep_list & with,
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
      return (get_ada_local_name ());
    }
  return (get_ada_local_name ());	   
}

string
adabe_sequence::marshal_name (dep_list & with,
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
      return (get_ada_local_name ());
    }
  return (get_ada_local_name ());	   
}

IMPL_NARROW_METHODS1 (adabe_sequence, AST_Sequence)
IMPL_NARROW_FROM_DECL (adabe_sequence)
IMPL_NARROW_FROM_SCOPE (adabe_sequence)

adabe_sequence::adabe_sequence (AST_Expression * v,
				AST_Type       * t)
  : AST_Sequence (v, t),
    AST_Decl (AST_Decl::NT_sequence, new UTL_ScopedName
	      (new Identifier ("sequence", 1, 0, I_FALSE), NULL), NULL),
    adabe_name (AST_Decl::NT_sequence, new UTL_ScopedName
		(new Identifier ("sequence", 1, 0, I_FALSE), NULL), NULL)
{
}


