/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_enum.cc                                           ***
***                                                                                            ***
***      This file provides the implementation of class adabe_enum      declared in adabe.h    ***
***   (L 226). This class is the correspondant of the Sun's Front-End class AST_Enum.          ***
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

  
IMPL_NARROW_METHODS1(adabe_enum, AST_Enum);
IMPL_NARROW_FROM_DECL(adabe_enum);
IMPL_NARROW_FROM_SCOPE(adabe_enum);


adabe_enum::adabe_enum(UTL_ScopedName *n, UTL_StrList *p)
       : AST_Enum(n, p),
	 AST_Decl(AST_Decl::NT_enum, n, p),
	 UTL_Scope(AST_Decl::NT_enum),
	 adabe_name(AST_Decl::NT_enum, n, p)
{
  pd_number_value = 0;
}

void
adabe_enum::produce_ads(dep_list& with,string &body, string &previous) {
  
  int numb = 0;
  // number of enum values
  
  compute_ada_name ();
  body += "   type " + get_ada_local_name() + " is ( ";
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  while (!activator.is_done())
    {
      AST_Decl *d = activator.item();
      activator.next();
      switch (d->node_type())
	{
	case AST_Decl::NT_enum_val:
	  numb++;
	  body+=adabe_enum_val::narrow_from_decl(d)->dump_name(with, previous);
	  break;
	default:
	  throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope in enumeration type");
	}
      if (!activator.is_done()) body += ", ";
    }
  set_number_value(numb);
  // set the number of enum values
  body +=" ) ;\n";
  // body += "   type " + get_ada_local_name() + "_Ptr is access ";
  // body += get_ada_local_name() + " ;\n\n";
  // body += "   procedure Free is new Ada.Unchecked_Deallocation(";
  // body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr) ;\n\n\n";  
  set_already_defined();
}

void  
adabe_enum::produce_marshal_ads(dep_list& with, string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out AdaBroker.NetBufferedStream.Object'Class) ;\n\n";

  body += "   procedure Unmarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out AdaBroker.NetBufferedStream.Object'Class) ;\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in CORBA.Unsigned_Long ;\n";
  body += "                        N : in CORBA.Unsigned_Long := 1)\n";
  body += "                        return CORBA.Unsigned_Long ;\n\n\n";

  set_already_defined();
}


void 
adabe_enum::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out AdaBroker.NetBufferedStream.Object'Class) is\n";
  body += "   begin\n";
  body += "      Marshall (CORBA.Unsigned_Long("+get_ada_local_name()+"'Pos(A)), S) ;\n";
  body += "   end Marshall ;\n\n";

  body += "   procedure Unmarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                         S : in out AdaBroker.NetBufferedStream.Object'Class) is \n";
  body += "      Tmp : CORBA.Unsigned_Long ;\n";
  body += "   begin\n";
  body += "      Unmarshall (Tmp,S) ;\n";
  body += "      A := ";
  body += get_ada_local_name();
  body += "'Val(Tmp) ;\n";
  body += "   end Unmarshall ;\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in CORBA.Unsigned_Long ;\n";
  body += "                        N : in CORBA.Unsigned_Long := 1)\n";
  body += "                        return CORBA.Unsigned_Long is\n";
  body += "   begin\n";
  body += "      return Align_Size (CORBA.Unsigned_Long(0), Initial_Offset ,N) ;\n";
  body += "   end Align_Size ;\n\n\n";

  set_already_defined();
}

string
adabe_enum::dump_name(dep_list& with, string &previous) 
{
   if (!is_imported(with))
    {
      if (!is_already_defined())
	{
	  string tmp = "";
	  produce_ads(with, tmp, previous);
	  previous += tmp;
	}
      return get_ada_local_name();
    }
  return get_ada_full_name();	   
}

string
adabe_enum::marshal_name(dep_list& with, string &previous) 
{
   if (!is_marshal_imported(with))
    {
      if (!is_already_defined())
	{
	  string tmp = "";
	  produce_marshal_adb(with, tmp, previous);
	  previous += tmp;
	}
      return get_ada_local_name();
    }
  return get_ada_full_name();	   
}



