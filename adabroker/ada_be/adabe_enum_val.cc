/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_enum_val                                          ***
***                                                                                            ***
***      This file provides the implementation of class adabe_enum_val declared in adabe.h     ***
***   (L 246). This class is the correspondant of the Sun's Front-End class AST_enum_val.      ***
***   It provides produce functions to access to the name                                      ***
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

  
IMPL_NARROW_METHODS1(adabe_enum_val, AST_EnumVal)
IMPL_NARROW_FROM_DECL(adabe_enum_val)

adabe_enum_val::adabe_enum_val(unsigned long v, UTL_ScopedName *n, UTL_StrList *p): AST_Decl(AST_Decl::NT_enum_val, n, p),
    AST_Constant(AST_Expression::EV_ulong,
		 AST_Decl::NT_enum_val,
		 new AST_Expression(v),
		 n,
		 p),
 
  AST_EnumVal(v,n,p),
  adabe_name(AST_Decl::NT_enum_val,n,p)
{
}


string
adabe_enum_val::dump_name(dep_list& with, string &previous)
{
  return get_ada_local_name();
}

string
adabe_enum_val::marshal_name(dep_list& with, string &previous)
{
  return get_ada_local_name();
}

