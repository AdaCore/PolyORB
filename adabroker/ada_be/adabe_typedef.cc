/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_typedef.cc                                        ***
***                                                                                            ***
***      This file provides the implementation of class adabe_typedef   declared in adabe.h    ***
***   (L 497). This class is the correspondant of the Sun's Front-End class AST_Typedef.       ***
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

adabe_typedef::adabe_typedef(AST_Type *bt, UTL_ScopedName *n, UTL_StrList *p)
	  : AST_Typedef(bt, n, p),
	    AST_Decl(AST_Decl::NT_typedef, n, p),
	    adabe_name(AST_Decl::NT_typedef, n, p)

{
  pd_number_value = 0;
}

void
adabe_typedef::produce_ads(dep_list& with, string &body, string &previous)
{
  compute_ada_name();
  AST_Decl *b = base_type();
  adabe_name *c = dynamic_cast<adabe_name *>(b);
  if (((string) b->local_name()->get_string()) == "local type")
    {
      switch (b->node_type())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_string:
	case AST_Decl::NT_union:
	  {
	    c->set_ada_local_name(get_ada_local_name());
	    c->set_ada_full_name(get_ada_full_name());
	    c->produce_ads(with, body, previous);
	    break;
	  }
	default:      
	  body += "   type " + get_ada_local_name() + " is new ";
	  string name =  dynamic_cast<adabe_name *>(b)->dump_name(with, previous); //virtual method
	  body += name;
	  body += " ;\n";
	  body += "   type " + get_ada_local_name() + "_Ptr is access all ";
	  body += get_ada_local_name() + " ;\n\n";
	  body += "   procedure free is new Ada.Unchecked_Deallocation(";
	  body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr) ;\n\n\n";
	  break;
	}
    }
  else
    {
      body += "   type " + get_ada_local_name() + " is new ";
      string name =  dynamic_cast<adabe_name *>(b)->dump_name(with, previous); //virtual method
      body += name;
      body += " ;\n";
      body += "   type " + get_ada_local_name() + "_Ptr is access all ";
      body += get_ada_local_name() + ";\n\n";
      body += "   procedure free is new Ada.Unchecked_Deallocation(";
      body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr) ;\n\n\n";
    }
  if (!c->has_fixed_size()) no_fixed_size();
  switch (b->node_type())
    {
    case AST_Decl::NT_typedef:
      set_number_value(dynamic_cast<adabe_typedef *>(b)->get_number_value());
      break;
    case AST_Decl::NT_enum:
      set_number_value(dynamic_cast<adabe_enum *>(b)->get_number_value());
      break;
    default:
      break;      
    }  
  set_already_defined ();
}

void
adabe_typedef::produce_marshal_ads(dep_list& with, string &body, string &previous)
{
  AST_Decl *b = base_type();
  if (((string) b->local_name()->get_string()) == "local type")
    {
      switch (b->node_type())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_string:
	case AST_Decl::NT_union:
	  {
	    string arg2 = "";
	    adabe_name *c = dynamic_cast<adabe_name *>(b);
	    c->set_ada_local_name(get_ada_local_name());
	    c->set_ada_full_name(get_ada_full_name());
	    c->produce_marshal_ads(with, body, arg2);
	    body += arg2;
	    set_already_defined();
	    return;
	  }
	default : {}
	}
    }
  
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                      S : in out Netbufferedstream.Object'Class) ;\n\n";
  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        S : in out Netbufferedstream.Object'Class) ;\n\n";
  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long ;\n\n\n";

  set_already_defined();
}

void
adabe_typedef::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  string arg2 = "";
  AST_Decl *b = base_type();
  if (((string) b->local_name()->get_string()) == "local type")
    {
      switch (b->node_type())
	{
	case AST_Decl::NT_array:
	case AST_Decl::NT_string:
	case AST_Decl::NT_union:
	  {
	    adabe_name *c = dynamic_cast<adabe_name *>(b);
	    c->set_ada_local_name(get_ada_local_name());
	    c->set_ada_full_name(get_ada_full_name());
	    c->produce_marshal_adb(with, body, arg2);
	    body += arg2;
	    set_already_defined();
	    return ;
	  }
	  default : {}
	}
    }
  
  string name = (dynamic_cast<adabe_name *> (base_type()))->marshal_name(with, arg2); 
  body += arg2;
  body += "   procedure Marshall(A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                      S : in out Netbufferedstream.Object'Class) is\n";
  body += "   begin\n";
  body += "      Marshall(";
  body += name;
  body += "(A), S) ;\n";
  body += "   end ;\n\n\n";
	    
  body += "   procedure UnMarshall(A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        S : in out Netbufferedstream.Object'Class) is\n";
  body += "   begin\n";
  body += "      UnMarshall(";
  body += name;
  body += "(A) ,S) ;\n";
  body += "   end ;\n\n\n";
  
  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long is\n";
  body += "      Tmp : Corba.Unsigned_Long := Initial_Offset ;\n";
  body += "   begin\n";
  body += "      Tmp := Align_Size(";
  body += name;
  body += "(A) , Tmp) ;\n";
  body += "      return Tmp ;\n";
  body += "   end ;\n\n\n";

  set_already_defined();
}

string
adabe_typedef::dump_name(dep_list& with, string &previous)
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
adabe_typedef::marshal_name(dep_list& with, string &previous)
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
IMPL_NARROW_METHODS1(adabe_typedef, AST_Typedef)
IMPL_NARROW_FROM_DECL(adabe_typedef)








