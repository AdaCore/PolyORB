/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_array.cc                                          ***
***                                                                                            ***
***      This file provides the implementation of class adabe_array     declared in adabe.h    ***
***   (L 381). This class is the correspondant of the Sun's Front-End class AST_Array.         ***
***   It provides produce functions for each generated file, a constructor and two little      ***
***   functions : dump_name and marshall_name whose job is to print the name of the type.      ***
***      It provides also a function to determine name of the "local type" from the front end. ***
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
#include <stdio.h>
  
IMPL_NARROW_METHODS1(adabe_array, AST_Array);
IMPL_NARROW_FROM_DECL(adabe_array);

adabe_array::adabe_array(UTL_ScopedName *n, unsigned long ndims, UTL_ExprList *dims):
  AST_Array(n,ndims,dims),
  AST_Decl(AST_Decl::NT_array, n, NULL),
  adabe_name(AST_Decl::NT_array,n,NULL)
{
}

////////////////////////////////////////////////////////////////
//////////////             local_type            ///////////////    
////////////////////////////////////////////////////////////////

string
adabe_array::local_type()
{
  // because the array is a local type
  // we need a little work to compute
  // his name :
  bool find = false;
  UTL_Scope *parent_scope = defined_in();
  UTL_ScopeActiveIterator parent_scope_activator(parent_scope,UTL_Scope::IK_decls);
  adabe_name *decl = dynamic_cast<adabe_name *>(parent_scope_activator.item());

  // We must find the element for which it is defined:
  do
    {
      switch (decl->node_type())
	{
	case AST_Decl::NT_field:
	case AST_Decl::NT_argument:
	  // this array is an argument or a field
	  if (dynamic_cast<AST_Field *>(decl)->field_type() == this)
	    find = true;
	  break;
	case AST_Decl::NT_op:
	  // this array is the return type of an operation
	  if (dynamic_cast<AST_Operation *>(decl)->return_type() == this)
	    find =true;
	  break;
	case AST_Decl::NT_typedef:
	  // this array is defined in a typedef:
	  // his name must be the one of this typedef
	  if (dynamic_cast<AST_Typedef *>(decl)->base_type() == this)
	    return decl->get_ada_local_name ();
		       
	default:
	  break;
	}
      parent_scope_activator.next();
      if (!find)
	decl = dynamic_cast<adabe_name *>(parent_scope_activator.item());
    } // We must do this loop until we have found which element
      // defines the array
  while (!find && !(parent_scope_activator.is_done()));

  // if we have found the element we add "_Array" to the name
  // of the element to defines the name of the array
  if (find)
    return decl->get_ada_local_name() +"_Array";

  return "local_type";
}

////////////////////////////////////////////////////////////////
//////////////       is_compute_name_needed      ///////////////    
////////////////////////////////////////////////////////////////

bool adabe_array::is_compute_name_needed()
  // this function returns false if the array is defined in a typedef
  // and true in all other cases
{
  adabe_name *parent_node;
  UTL_Scope *parent_scope = defined_in();
  UTL_ScopeActiveIterator parent_scope_activator(parent_scope,UTL_Scope::IK_decls);
  adabe_name *decl;
  while (!parent_scope_activator.is_done())
    {
      decl = dynamic_cast<adabe_name *>(parent_scope_activator.item());
      switch (decl->node_type())
	{
	case AST_Decl::NT_field:
	case AST_Decl::NT_argument:
	  if (dynamic_cast<AST_Field *>(decl)->field_type() == this)
	    return true;
	  break;
	case AST_Decl::NT_op:
	  if (dynamic_cast<AST_Operation *>(decl)->return_type() == this)
	    return true;
	  break;
	case AST_Decl::NT_typedef:
	  if (dynamic_cast<AST_Typedef *>(decl)->base_type() == this)
	    return false;
	  break;
		       
	default:
	  break;
	}
      parent_scope_activator.next();
    }
  return true;
}

////////////////////////////////////////////////////////////////
//////////////            produce_ads            ///////////////    
////////////////////////////////////////////////////////////////

void
adabe_array::produce_ads(dep_list& with,string &body, string &previous) {
  char number[256];
  
  // if the array s defined in a typedef,
  // no compute_ada_name is needed (the name has been forced)
  if (is_compute_name_needed())
    compute_ada_name();
  
  body += "   type " + get_ada_local_name() + " is array (";

  
  for (unsigned int i=0; i < n_dims(); i++)
    // loops over the dimensions of the array
    {
      AST_Expression::AST_ExprValue* v = dims()[i]->ev();
      body += " 0..";  
      switch (v->et) 
	{
	case AST_Expression::EV_short:
	  sprintf (number, "%d", v->u.sval-1);
	  break;
	case AST_Expression::EV_ushort:
	  sprintf (number, "%d", v->u.usval-1);
	  break;
	case AST_Expression::EV_long:
	  sprintf (number, "%ld", v->u.lval-1);
	  break;
	case AST_Expression::EV_ulong:
	  sprintf (number, "%ld", v->u.ulval-1);
	  break;
	default:
	  throw adabe_internal_error(__FILE__,__LINE__,"unexpected type in array expression");
	}
      body +=number;
      
      // if an other dimension follows, a ',' is added
      if (i != n_dims() - 1) body += ",";  
    }
  body +=" )";

  // determinaion of the array type
  adabe_name *f = dynamic_cast<adabe_name *>(base_type());
  body+= " of " + f->dump_name(with, previous);
  body += " ;\n" ;

  // definition of a pointer on the array
  body += "   type " + get_ada_local_name() + "_Ptr is access ";
  body += get_ada_local_name() + " ;\n\n";

  // definition of the free function
  body += "   procedure Free is new Ada.Unchecked_Deallocation(";
  body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr) ;\n\n\n";

  // if the structure in the array does not have
  // a fixed size a fag is put (the marshall is not
  // the same)
  if (!f->has_fixed_size()) no_fixed_size();

  // set the node to defined
  set_already_defined();
}


////////////////////////////////////////////////////////////////
//////////////        produce_marshal_ads        ///////////////    
////////////////////////////////////////////////////////////////

void
adabe_array::produce_marshal_ads(dep_list& with,string &body, string &previous)
{
  // we now need do declare the marshalling functions:
  // Marshall, Unmarshall, Align_Size
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Netbufferedstream.Object'Class) ;\n\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Netbufferedstream.Object'Class) ;\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "               Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "               N : in Corba.Unsigned_Long := 1)\n";
  body += "               return Corba.Unsigned_Long ;\n\n\n";

  set_already_defined();
}

////////////////////////////////////////////////////////////////
//////////////        produce_marshal_adb        ///////////////    
////////////////////////////////////////////////////////////////

void
adabe_array::produce_marshal_adb(dep_list& with,string &body, string &previous)
{
  adabe_name *b = dynamic_cast<adabe_name *>(base_type());
  string name = b->marshal_name(with, previous);

  // computing the number of cases in the array
  unsigned long size = 1;
  for (unsigned int i=0; i < n_dims(); i++) {
    AST_Expression::AST_ExprValue* v = dims()[i]->ev();
    switch (v->et) 
      {
      case AST_Expression::EV_short:
	size *= v->u.sval;
	break;
      case AST_Expression::EV_ushort:
	size *= v->u.usval;
	break;
      case AST_Expression::EV_long:
	size *= v->u.lval;
	break;
      case AST_Expression::EV_ulong:
	size *= v->u.ulval;
	break;
      default:
	throw adabe_internal_error(__FILE__,__LINE__,"unexpected type in array expression");
      }
  }
  char Size[256];
  sprintf(Size,"%lu",size);

  string marshall = "";
  string unmarshall = "";
  string align_size = "";

  // declaration of the function marshall
  marshall += "   procedure Marshall (A : in ";
  marshall += get_ada_local_name();
  marshall += " ;\n";
  marshall += "                       S : in out Netbufferedstream.Object'Class) is\n";
  marshall += "   begin\n";

  // declaration of the function unmarshall
  unmarshall += "   procedure UnMarshall (A : out ";
  unmarshall += get_ada_local_name();
  unmarshall += " ;\n";
  unmarshall += "                         S : in out Netbufferedstream.Object'Class) is\n";
  unmarshall += "   begin\n";

  // declaration of the function align size
  align_size += "   function Align_Size (A : in ";
  align_size += get_ada_local_name();
  align_size += " ;\n";
  align_size += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  align_size += "                        N : in Corba.Unsigned_Long := 1)\n";
  align_size += "                        return Corba.Unsigned_Long is\n";
  align_size += "      Tmp : Corba.Unsigned_long := Initial_Offset ;\n";
  align_size += "   begin\n";

  // Two eventuallity must be considered:
  if (b->has_fixed_size())
    {
      // if the size of the type in this array
      // is fixed, ther no problem
      align_size += "      Tmp := Align_Size (A(1";
      for (unsigned int i = 1; i < n_dims() ; i++ )
	{
	  align_size += ",1";
	}
      align_size += "), Initial_Offset, N * ";
      align_size += Size;
      align_size += ") ;\n";
    } 
  else 
    {
      // else, we must loop over all the types
      // to compute the memory needed
      align_size += "      for I in 1..N loop\n";
    }

  string spaces = "      ";
  for (unsigned int i = 0 ; i < n_dims() ; i++) 
    {
      char number[10];
      sprintf (number,"%d",i+1);
      // we have now a loop over the diemension "number"
      marshall += spaces + "for I";
      marshall += number;
      marshall += " in A'range(";
      marshall += number;
      marshall += ") loop \n";
      
      unmarshall += spaces + "for I";
      unmarshall += number;
      unmarshall += " in A'range(";
      unmarshall += number;
      unmarshall += ") loop \n";
      
      if (!b->has_fixed_size())
	{
	  align_size += spaces + "   for I";
	  align_size += number;
	  align_size += " in A'range(";
	  align_size += number;
	  align_size += ") loop \n";
	}
      spaces += "   ";
    }

  marshall += spaces + "Marshall (A(I1";
  unmarshall += spaces + "UnMarshall (A(I1";
  if (!b->has_fixed_size())
    align_size += spaces + "   Tmp := Align_Size (A(I1";

  for (unsigned int i = 1 ; i < n_dims() ; i++) 
    {
      char number[256];
      sprintf (number,"%d",i+1);

      marshall += ", I";
      marshall +=  number;
      unmarshall += ", I";
      unmarshall += number;
      if (!b->has_fixed_size())
	{
	  align_size += ", I";
	  align_size += number;
	}
    }

  marshall += "), S) ; \n";
  unmarshall += "), S) ; \n";
  if (!b->has_fixed_size())
    align_size += "), Tmp) ; \n";

  for (unsigned int i = 0 ; i < n_dims() ; i++) 
    {
      spaces = spaces.substr(0,spaces.length()-3);
      marshall += spaces + "end loop ;\n";
      unmarshall += spaces + "end loop ;\n";
      if (!b->has_fixed_size())
	align_size += spaces + "   end loop ;\n";
    }
      
  marshall += "   end Marshall ;\n\n";
  unmarshall += "   end UnMarshall ;\n\n";
  if (!b->has_fixed_size())
    align_size += "      end loop ;\n";
  align_size += "      return Tmp ;\n";
  align_size += "   end Align_Size ;\n\n\n";      

  body += marshall;
  body += unmarshall;
  body += align_size;

  set_already_defined();
}

////////////////////////////////////////////////////////////////
//////////////             dump_name             ///////////////    
////////////////////////////////////////////////////////////////

string adabe_array::dump_name(dep_list& with, string &previous) 
{
  if (!is_imported(with))
    {
      if (!is_already_defined())
	{
	  // has this array already been defined ?
	  string tmp = "";
	  produce_ads(with, tmp, previous);
	  previous += tmp;
	}
      // this array is defined in this file, so
      // a local name is enough
      return get_ada_local_name();
    }
  // the array is defined in aother file
  // we need to use a full name
  return get_ada_full_name();	   
}

////////////////////////////////////////////////////////////////
//////////////           marshal_name            ///////////////    
////////////////////////////////////////////////////////////////

string adabe_array::marshal_name(dep_list& with, string &previous) 
{
  if (!is_marshal_imported(with))
    {
     if (!is_already_defined())
	{
	  // has this array already been defined ?
	  string tmp = "";
	  produce_marshal_adb(with, tmp, previous);
	  previous += tmp;
	}
      // this array is defined in this file, so
      // a local name is enough
      return get_ada_local_name();
    }
  // the array is defined in aother file
  // we need to use a full name
  return get_ada_full_name();	   
}    


