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
#include <stdio.h>


adabe_constant::adabe_constant (AST_Expression::ExprType et,
			 AST_Expression *v,
			 UTL_ScopedName *n,
			 UTL_StrList *p)
	   : AST_Constant (et, v, n, p),
	     AST_Decl (AST_Decl::NT_const, n, p),
	     adabe_name (AST_Decl::NT_const, n, p)
{
}

void
adabe_constant::produce_ads (dep_list& with, string &body, string &previous)
{
  idl_bool intfconst = 0;
  bool initialized = false;
  char value[256];
  compute_ada_name ();
  body += "   " +get_ada_local_name () +": constant ";
  AST_Expression::ExprType etype = et ();
  switch (etype) 
    {
    case AST_Expression::EV_short:
      body += "Short_Integer := ";
      sprintf (value,"%d", constant_value ()->ev ()->u.sval);
      break;
    case AST_Expression::EV_ushort:
      body += "Unsigned_Short_Integer := ";
      sprintf (value,"%d", constant_value ()->ev ()->u.usval);
      break;
    case AST_Expression::EV_long:
      body += "Short_Integer := ";
      sprintf (value,"%ld", constant_value ()->ev ()->u.lval);	
      break;
    case AST_Expression::EV_ulong:
      body += "Unsigned_Long_Integer := ";
      sprintf (value,"%ld", constant_value ()->ev ()->u.ulval);	
      break;
    case AST_Expression::EV_float:
      body += "Real := ";
      sprintf (value,"%f", constant_value ()->ev ()->u.fval);
      break;
    case AST_Expression::EV_double:
      body += "Long_Real := ";
      sprintf (value,"%f", constant_value ()->ev ()->u.dval);	
      break;
    case AST_Expression::EV_char:
      body += "Character := ";
      sprintf (value,"%c", constant_value ()->ev ()->u.cval);	
      break;
    case AST_Expression::EV_octet:
      body += "Byte := ";
      sprintf (value,"%d", constant_value ()->ev ()->u.oval);	
      break;
    case AST_Expression::EV_bool:
      body += "Boolean := ";
      sprintf (value,"%ld", constant_value ()->ev ()->u.bval);
      break;
    case AST_Expression::EV_string: {
      string temp;
      temp = constant_value ()->ev ()->u.strval->get_string ();
      body += "String := ";
      with.add ("Ada.Characters.Latin_1");
      write_string_to_ada (temp, body);
      initialized = true;
    }
    break;
    default:
      throw adabe_internal_error (__FILE__,__LINE__,"unexpected type under constant class");
      break;
    }
  if (initialized == false) body += value;
  body += ";\n";
  return;
}


void adabe_constant::write_string_to_ada (string c_string, string &output) {
  int slash_position;
  while ((slash_position = c_string.find ('\\')) >= 0) 
    {
      string temp1 = c_string.substr (0, slash_position-1);
      switch (c_string[slash_position+1]) 
	{
	case 'n':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.LF";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  break;
	case 't':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.HT";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //... 011 Ada.Characters.Latin_1.HT
	  break;
	case 'v':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.VT";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //...013 Ada.Characters.Latin_1.VT
	  break;
	case 'b':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.BS";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //...010 Ada.Characters.Latin_1.BS
	  break;
	case 'r':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.CR";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //...015 Ada.Characters.Latin_1.CR
	  break;
	case 'f':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.FF";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //...014 Ada.Characters.Latin_1.FF
	  break;
	case 'a':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.BEL";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //...007 Ada.Characters.Latin_1.BEL
	  break;
	case '\\':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.Reverse_Solidus";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //...134 Ada.Characters.Latin_1.Reverse_Solidus
	  break;
	case '?':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.Question";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //...077 Ada.Characters.Latin_1.Question
	  break;
	case '\'':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.Apostrophe";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //...047 Ada.Characters.Latin_1.Apostrophe
	  break;
	case '"':
	  output += "\"" + temp1 + "\" & Ada.Characters.Latin_1.Quotation";
	  c_string = c_string.substr (slash_position+2, c_string.length ());
	  //...042 Ada.Characters.Latin_1.Quotation
	  break;
	case 'o':
	  output += "\"" + temp1 + "\" & character'val (8#" + c_string.substr (slash_position+2, slash_position+4)
	    +"#)";
	  c_string = c_string.substr (slash_position+5, c_string.length ());
	  //...'ooo' -> 'ooo' Character'val (8#ooo#);
	  break;
	case 'x':
	  output += "\"" + temp1 + "\" & Character'val (16#" + c_string.substr (slash_position+2, slash_position+4) 
	    +"#)";
	  c_string = c_string.substr (slash_position+5, c_string.length ());
	  //Character'val (16#hhh#)
	  break;
	default:
	  throw adabe_internal_error (__FILE__,__LINE__,"unexpected escape character in string");
	}
      if (c_string.length () > 0)
	output += " & ";
    }
  if (c_string.length () > 0)
    output += "\"" + c_string + "\"";

}

string
adabe_constant::dump_name (dep_list& with, string &previous)
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
adabe_constant::marshal_name (dep_list& with, string &previous)
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
IMPL_NARROW_METHODS1 (adabe_constant, AST_Constant)
IMPL_NARROW_FROM_DECL (adabe_constant)





