/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_interface_fwd.cc                                  ***
***                                                                                            ***
***      This file provides the implementation of class adabe_interface declared in adabe.h    ***
***   (L 550). This class is the correspondant of the Sun's Front-End class AST_Interface_fwd. ***
***      It creates a file in which the interface is forward declared.                         ***
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

adabe_interface_fwd::adabe_interface_fwd(UTL_ScopedName *n, UTL_StrList *p)
  : AST_InterfaceFwd(n, p),
    AST_Decl(AST_Decl::NT_interface_fwd, n, p),
    adabe_name(AST_Decl::NT_interface_fwd, n, p)
{
}
  
static string remove_dot(string  name)
{
  char c;
  while ((c = name.find(".")) != -1) 
    name[c]='-';
  return name;
}
void
adabe_interface_fwd::produce_ads(dep_list& with, string &body, string &previous)
{
  compute_ada_name();  
  dynamic_cast<adabe_name *>(full_definition())->set_ada_local_name(get_ada_local_name()); 
  dynamic_cast<adabe_name *>(full_definition())->set_ada_full_name(get_ada_full_name());
  string file_name = remove_dot(get_ada_full_name()) + "_forward.ads";
  char *lower_case_name = lower(file_name.c_str());
  ofstream file(lower_case_name);
  delete[] lower_case_name;
  file << "with Corba.Forward ;\n";
  file << "package " + get_ada_full_name() + "_Forward is new Corba.Forward ;\n";
  file.close();  
}

/*
  void
  adabe_interface_fwd::produce_ads(dep_list& with,string &body, string &previous)
  {
  produce_ads( with, body, previous);
  }
*/

IMPL_NARROW_METHODS1(adabe_interface_fwd, AST_InterfaceFwd)
IMPL_NARROW_FROM_DECL(adabe_interface_fwd)
IMPL_NARROW_FROM_SCOPE(adabe_interface_fwd)









