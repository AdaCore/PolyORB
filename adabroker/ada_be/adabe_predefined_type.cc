/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_predefined_type.cc                                ***
***                                                                                            ***
***   This file provides the implementation of class adabe_predefined_type declared in adabe.h ***
***   (L 182). This class is the correspondant of the Sun's Front-End class AST_PredefinedType.***
***   It provides produce functions for each generated file, a constructor and two little      ***
***   functions : dump_name and marshall_name whose job is to print the name of the type.      ***
***                                                                                            ***
***   Copyright 1999                                                                           ***
***   Jean Marie Cottin, Laurent Kubler, Vincent Niebel                                        ***
***                                                                                            ***
***   This is free software; you can redistribute it and/or modify it under terms of the GNU   ***
***   General Public License, as published by the Free Software Foundation.                    ***
***                                                                                            ***
***  This back-end is distributed in the hope that it will be usefull, but WITHOUT ANY         ***
***  WARRANTY; without even the implied waranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR ***
***  PURPOSE.                                                                                  ***
***                                                                                            ***
***  See the GNU General Public License for more details.                                      ***
***                                                                                            ***
***                                                                                            ***
*************************************************************************************************/
#include <adabe.h>

adabe_predefined_type::adabe_predefined_type(AST_PredefinedType::PredefinedType t, UTL_ScopedName *n, UTL_StrList *p)
  : AST_PredefinedType(t, n, p),
    AST_Decl(AST_Decl::NT_pre_defined, n, p),
    adabe_name(AST_Decl::NT_pre_defined, n, p)
{
}

void 
adabe_predefined_type::produce_ads(dep_list& with, string &body, string &previous)
{
  body += get_ada_predefined_type();
  set_already_defined();
}

string
adabe_predefined_type::dump_name(dep_list& with, string &previous)
{
  return get_ada_predefined_type();
}

string
adabe_predefined_type::marshal_name(dep_list& with, string &previous)
{
  return get_ada_predefined_type();
}

string adabe_predefined_type::get_ada_predefined_type() 
{ 
  string name = ""; 
  switch(pt()) 
    { 
    case AST_PredefinedType::PT_long:
      name = "Corba.Long"; 
      break; 
    case AST_PredefinedType::PT_ulong: name = "Corba.Unsigned_Long";
      break; 
    case AST_PredefinedType::PT_short: name = "Corba.Short";
      break; 
    case AST_PredefinedType::PT_ushort: name = "Corba.Unsigned_Short";
      break; 
    case AST_PredefinedType::PT_float: name = "Corba.Float"; 
      break; 
    case AST_PredefinedType::PT_double: name = "Corba.Double"; 
      break; 
    case AST_PredefinedType::PT_char: name = "Corba.Char"; 
      break;
    case AST_PredefinedType::PT_boolean: name = "Corba.Boolean"; 
      break; 
    case AST_PredefinedType::PT_octet: name = "Corba.Octet"; 
      break; 
    case AST_PredefinedType::PT_void: name = "<void>"; 
      break;
    default: throw
	       adabe_internal_error(__FILE__,__LINE__,"Unexpected predefined type");
      break; 
    }
  return name; 
}

IMPL_NARROW_METHODS1(adabe_predefined_type, AST_PredefinedType)
IMPL_NARROW_FROM_DECL(adabe_predefined_type)
 








