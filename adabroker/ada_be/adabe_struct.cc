/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_struct.cc                                         ***
***                                                                                            ***
***      This file provides the implementation of class adabe_struct    declared in adabe.h    ***
***   (L 340). This class is the correspondant of the Sun's Front-End class AST_Struct.        ***
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

adabe_structure::adabe_structure(UTL_ScopedName *n, UTL_StrList *p)
	    : AST_Decl(AST_Decl::NT_struct, n, p),
	      UTL_Scope(AST_Decl::NT_struct),
	      adabe_name(AST_Decl::NT_struct, n, p)
{
}

//////////////////////////////////////////////////////////////////
/////////////////        produce_ads          ////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_structure::produce_ads(dep_list& with, string &body, string &previous)
{
  // This library will be needed for the Free function
  with.add ("Ada.Unchecked_Deallocation");
  
  // beginning of the declaration
  body += "   type " + get_ada_local_name() + " is record\n";

  // we must now look in the scope and name all the fields
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      adabe_field *e = dynamic_cast<adabe_field *>(d);
      if (d->node_type() == AST_Decl::NT_field)
	{
	  // the currrent field is beeing produced
	  e->produce_ads(with, body, previous);
	}
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in structure");
      if (!(dynamic_cast<adabe_name *>(e->field_type()))->has_fixed_size()) no_fixed_size();
      i.next();      
    }

  // the end of the Structure
  body += "   end record ;\n";
  
  // the pointer  which will access the structure
  body += "   type " + get_ada_local_name() + "_Ptr is access ";
  body += get_ada_local_name() + " ;\n\n";

  // the free function
  body += "   procedure Free is new Ada.Unchecked_Deallocation(";
  body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr) ;\n\n\n";
  set_already_defined();
}

//////////////////////////////////////////////////////////////////
///////////////       produce_marshal_ads      ///////////////////
//////////////////////////////////////////////////////////////////

void
adabe_structure::produce_marshal_ads(dep_list &with, string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out AdaBroker.NetBufferedStream.Object'Class) ;\n\n";
  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                         S : in out AdaBroker.NetBufferedStream.Object'Class) ;\n\n";
  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in CORBA.Unsigned_Long ;\n";
  body += "                        N : in CORBA.Unsigned_Long := 1)\n";
  body += "                        return CORBA.Unsigned_Long ;\n\n\n";

  set_already_defined();
}

//////////////////////////////////////////////////////////////////
///////////////       produce_marshal_adb         ////////////////
//////////////////////////////////////////////////////////////////

void
adabe_structure::produce_marshal_adb(dep_list &with, string &body, string &previous)
{
  string marshall = "";
  string unmarshall = "";
  string align_size = "";
  marshall += "   procedure Marshall(A : in ";
  marshall += get_ada_local_name();
  marshall += " ;\n";
  marshall += "                      S : in out AdaBroker.NetBufferedStream.Object'Class) is\n";
  marshall += "   begin\n";
  
  unmarshall += "   procedure UnMarshall(A : out ";
  unmarshall += get_ada_local_name();
  unmarshall += " ;\n";
  unmarshall += "                        S : in out AdaBroker.NetBufferedStream.Object'Class) is\n";
  unmarshall += "   begin\n";
  
  align_size += "   function Align_Size (A : in ";
  align_size += get_ada_local_name();
  align_size += " ;\n";
  align_size += "                        Initial_Offset : in CORBA.Unsigned_Long ;\n";
  align_size += "                        N : in CORBA.Unsigned_Long := 1)\n";
  align_size += "                        return CORBA.Unsigned_Long is\n";
  align_size += "      Tmp : CORBA.Unsigned_Long := Initial_Offset ;\n";
  align_size += "   begin\n";
  align_size += "      for I in 1..N loop\n";

  // for all of the field we must lauch his
  // produce_marshall adb
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_field) {
	dynamic_cast<adabe_field *>(d)->produce_marshal_adb(with, body, marshall, unmarshall, align_size);
      }
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in structure");
      i.next();
    }

  marshall += "   end Marshall;\n\n";
  unmarshall += "   end Unmarshall;\n\n";
  align_size += "      end loop ;\n";
  align_size += "      return Tmp ;\n";
  align_size += "   end Align_Size;\n\n\n";

  body += marshall;
  body += unmarshall;
  body += align_size;

  set_already_defined();
}

//////////////////////////////////////////////////////////////////
//////////////////        dump_name       ////////////////////////
//////////////////////////////////////////////////////////////////

string
adabe_structure::dump_name(dep_list& with, string &previous)
{
  if (!is_imported(with))
    {
      if (!is_already_defined())
	{
	  // has this structure already been defined ?
	  string tmp = "";
	  produce_ads(with, tmp, previous);
	  previous += tmp;
	}
      // this structure is defined in this file, so
      // a local name is enough
      return get_ada_local_name();
    }
  // because the structure is defined in another file
  // we need to use a full name
  return get_ada_full_name();	   
}

//////////////////////////////////////////////////////////////////
////////////////       marshal_name       ////////////////////////
//////////////////////////////////////////////////////////////////

string
adabe_structure::marshal_name(dep_list& with, string &previous)
{
  if (!is_marshal_imported(with))
    {
      if (!is_already_defined())
	{
	  // have the marshall functions for this
	  // structure already been defined
	  string tmp = "";
	  produce_marshal_adb(with, tmp, previous);
	  previous += tmp;
	}
      // this structure is defined in this file, so
      // a local name is enough
      return get_ada_local_name();
    }
  // because this structure is defined in another file
  // we need to use a full name
  return get_ada_full_name();	   
}  
IMPL_NARROW_METHODS1(adabe_structure, AST_Structure)
IMPL_NARROW_FROM_DECL(adabe_structure)
IMPL_NARROW_FROM_SCOPE(adabe_structure)





