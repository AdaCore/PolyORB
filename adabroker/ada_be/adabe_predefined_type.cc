//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.23 $
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

adabe_predefined_type::adabe_predefined_type (AST_PredefinedType::PredefinedType t, UTL_ScopedName *n, UTL_StrList *p)
  : AST_PredefinedType (t, n, p),
    AST_Decl (AST_Decl::NT_pre_defined, n, p),
    adabe_name (AST_Decl::NT_pre_defined, n, p)
{
}

void 
adabe_predefined_type::produce_ads (dep_list& with, string &body, string &previous)
{
  body += get_ada_predefined_type ();
  set_already_defined ();
}

string
adabe_predefined_type::dump_name (dep_list& with, string &previous)
{
  return get_ada_predefined_type ();
}

string
adabe_predefined_type::marshal_name (dep_list& with, string &previous)
{
  return get_ada_predefined_type ();
}

string adabe_predefined_type::get_ada_predefined_type () 
{
  string name = ""; 
  switch (pt ()) 
    { 
    case AST_PredefinedType::PT_long: name = "CORBA.Long"; 
      break; 
    case AST_PredefinedType::PT_ulong: name = "CORBA.Unsigned_Long";
      break; 
    case AST_PredefinedType::PT_short: name = "CORBA.Short";
      break; 
    case AST_PredefinedType::PT_ushort: name = "CORBA.Unsigned_Short";
      break; 
    case AST_PredefinedType::PT_float: name = "CORBA.Float"; 
      break; 
    case AST_PredefinedType::PT_double: name = "CORBA.Double"; 
      break; 
    case AST_PredefinedType::PT_char: name = "CORBA.Char"; 
      break;
    case AST_PredefinedType::PT_boolean: name = "CORBA.Boolean"; 
      break; 
    case AST_PredefinedType::PT_octet: name = "CORBA.Octet"; 
      break; 
    case AST_PredefinedType::PT_void: name = "<void>"; 
      break;
    case AST_PredefinedType::PT_longlong:
      throw adabe_internal_error
	(__FILE__,__LINE__,"unimplemented predefined type long long");
    case AST_PredefinedType::PT_ulonglong:
      throw adabe_internal_error
	(__FILE__,__LINE__,"unimplemented predefined type unsigned long long");
    case AST_PredefinedType::PT_longdouble:
      throw adabe_internal_error
	(__FILE__,__LINE__,"unimplemented predefined type long double");
    case AST_PredefinedType::PT_wchar:
      throw adabe_internal_error
	(__FILE__,__LINE__,"unimplemented predefined type wchar_t");
    case AST_PredefinedType::PT_any:
      throw adabe_internal_error
	(__FILE__,__LINE__,"unimplemented predefined type Any");
    case AST_PredefinedType::PT_pseudo:
      throw adabe_internal_error
	(__FILE__,__LINE__,"unimplemented predefined type pseudo object");
    case AST_PredefinedType::PT_TypeCode:
      throw adabe_internal_error
	(__FILE__,__LINE__,"unimplemented predefined type TypeCode");
    default:
      throw adabe_internal_error
	(__FILE__,__LINE__,"Unexpected predefined type");
    }
  return name; 
}

IMPL_NARROW_METHODS1 (adabe_predefined_type, AST_PredefinedType)
IMPL_NARROW_FROM_DECL (adabe_predefined_type)
 








