//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.4 $
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
  
IMPL_NARROW_METHODS1 (adabe_array, AST_Array);
IMPL_NARROW_FROM_DECL (adabe_array);

adabe_array::adabe_array (UTL_ScopedName * n,
			  unsigned long    ndims,
			  UTL_ExprList   * dims)
  : AST_Array (n, ndims, dims),
    AST_Decl (AST_Decl::NT_array, n, NULL),
    adabe_name (AST_Decl::NT_array, n, NULL)
{
}

////////////////////////////////////////////////////////////////
//////////////             local_type            ///////////////    
////////////////////////////////////////////////////////////////

string
adabe_array::local_type ()
{
  // because the array is a local type
  // we need a little work to compute
  // his name :
  bool find = false;
  UTL_Scope *parent_scope = defined_in ();
  UTL_ScopeActiveIterator parent_scope_activator
    (parent_scope, UTL_Scope::IK_decls);
  adabe_name *decl =
    dynamic_cast<adabe_name *>(parent_scope_activator.item ());

  // We must find the element for which it is defined:
  do
    {
      switch (decl->node_type ())
	{
	case AST_Decl::NT_field:
	case AST_Decl::NT_argument:
	  // this array is an argument or a field
	  if (dynamic_cast<AST_Field *>(decl)->field_type () == this)
	    find = true;
	  break;
	case AST_Decl::NT_op:
	  // this array is the return type of an operation
	  if (dynamic_cast<AST_Operation *>(decl)->return_type () == this)
	    find =true;
	  break;
	case AST_Decl::NT_typedef:
	  // this array is defined in a typedef:
	  // his name must be the one of this typedef
	  if (dynamic_cast<AST_Typedef *>(decl)->base_type () == this)
	    return decl->get_ada_local_name ();
		       
	default:
	  break;
	}
      parent_scope_activator.next ();
      if (!find)
	decl = dynamic_cast<adabe_name *>(parent_scope_activator.item ());
    } // We must do this loop until we have found which element
      // defines the array
  while (!find && !(parent_scope_activator.is_done ()));

  // if we have found the element we add "_Array" to the name
  // of the element to defines the name of the array
  if (find)
    return decl->get_ada_local_name () +"_Array";

  return "local_type";
}

////////////////////////////////////////////////////////////////
//////////////       is_compute_name_needed      ///////////////    
////////////////////////////////////////////////////////////////

bool adabe_array::is_compute_name_needed ()
  // this function returns false if the array is defined in a typedef
  // and true in all other cases
{
  adabe_name *parent_node;
  UTL_Scope *parent_scope = defined_in ();
  UTL_ScopeActiveIterator parent_scope_activator
    (parent_scope, UTL_Scope::IK_decls);
  adabe_name *decl;
  while (!parent_scope_activator.is_done ())
    {
      decl = dynamic_cast<adabe_name *>(parent_scope_activator.item ());
      switch (decl->node_type ())
	{
	case AST_Decl::NT_field:
	case AST_Decl::NT_argument:
	  if (dynamic_cast<AST_Field *>(decl)->field_type () == this)
	    return true;
	  break;
	case AST_Decl::NT_op:
	  if (dynamic_cast<AST_Operation *>(decl)->return_type () == this)
	    return true;
	  break;
	case AST_Decl::NT_typedef:
	  if (dynamic_cast<AST_Typedef *>(decl)->base_type () == this)
	    return false;
	  break;
		       
	default:
	  break;
	}
      parent_scope_activator.next ();
    }
  return true;
}

////////////////////////////////////////////////////////////////
//////////////            produce_ads            ///////////////    
////////////////////////////////////////////////////////////////

void
adabe_array::produce_ads (dep_list & with,
			  string   & body,
			  string   & previous)
{
  char number[256];
  
  // if the array s defined in a typedef,
  // no compute_ada_name is needed (the name has been forced)
  if (is_compute_name_needed ())
    compute_ada_name ();
  
  body += "   type " + get_ada_local_name () + " is array (";

  
  for (unsigned int i=0; i < n_dims (); i++)
    // loops over the dimensions of the array
    {
      AST_Expression::AST_ExprValue* v = dims ()[i]->ev ();
      body += "0 .. ";  
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
	  throw adabe_internal_error
	    (__FILE__,__LINE__,"unexpected type in array expression");
	}
      body +=number;
      
      // if an other dimension follows, a ',' is added
      if (i != n_dims () - 1) body += ", ";  
    }
  body +=")";

  // determinaion of the array type
  adabe_name *f = dynamic_cast<adabe_name *>(base_type ());
  body+= " of " + f->dump_name (with, previous);
  body += ";\n\n";

  // definition of a pointer on the array
  body += "   type " + get_ada_local_name () + "_Ptr is access ";
  body += get_ada_local_name () + ";\n\n";

  // definition of the free function
  // body += "   procedure Free is new Ada.Unchecked_Deallocation (";
  // body += get_ada_local_name () + ", " + get_ada_local_name ()+ "_Ptr);\n\n\n";

  // if the structure in the array does not have
  // a fixed size a fag is put (the marshall is not
  // the same)
  if (!f->has_fixed_size ()) no_fixed_size ();

  // set the node to defined
  set_already_defined ();
}


////////////////////////////////////////////////////////////////
//////////////        produce_stream_ads        ///////////////    
////////////////////////////////////////////////////////////////

void
adabe_array::produce_stream_ads (dep_list & with,
				 string   & body,
				 string   & previous)
{
  // we now need do declare the marshalling functions:
  // Marshall, Unmarshall, Compute_New_Size
  gen_marshalling_declarations (body, get_ada_local_name ());
  set_already_defined ();
}

////////////////////////////////////////////////////////////////
//////////////        produce_stream_adb        ///////////////    
////////////////////////////////////////////////////////////////

void
adabe_array::produce_stream_adb (dep_list & with,
				 string   & body,
				 string   & previous)
{
  adabe_name *b = dynamic_cast<adabe_name *>(base_type ());
  string name = b->marshal_name (with, previous);

  // computing the number of cases in the array
  unsigned long size = 1;
  for (unsigned int i=0; i < n_dims (); i++) {
    AST_Expression::AST_ExprValue* v = dims ()[i]->ev ();
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
	throw adabe_internal_error
	  (__FILE__,__LINE__,"unexpected type in array expression");
      }
  }
  char Size[32];
  sprintf (Size,"%lu", size);

  string marshall = "";
  string unmarshall = "";
  string marshall_size = "";

  // declaration of the function marshall
  marshall += 
    "   procedure Marshall\n"
    "      (Stream : in out Broca.Buffers.Buffer_descriptor;\n"
    "       Val : " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n";

  // declaration of the function unmarshall
  unmarshall +=
    "   procedure Unmarshall\n"
    "      (Stream : in out Broca.Buffers.Buffer_descriptor;\n"
    "       Res : out " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n";

  // declaration of the function align size
  marshall_size +=
    "   procedure Compute_New_Size\n"
    "      (Stream : in out Broca.Buffers.Buffer_descriptor;\n"
    "       Val : " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n";

  string spaces = "      ";
  for (unsigned int i = 0; i < n_dims (); i++) 
    {
      char number[10];
      sprintf (number,"%d", i+1);
      // we have now a loop over the diemension "number"
      marshall += spaces + "for I";
      marshall += number;
      marshall += " in Val'range (";
      marshall += number;
      marshall += ") loop \n";

      unmarshall += spaces + "for I";
      unmarshall += number;
      unmarshall += " in Res'range (";
      unmarshall += number;
      unmarshall += ") loop \n";
      
      marshall_size += spaces + "for I";
      marshall_size += number;
      marshall_size += " in Val'range (";
      marshall_size += number;
      marshall_size += ") loop \n";
      spaces += "   ";
    }

  marshall += spaces + "Marshall (Stream, Val (I1";
  unmarshall += spaces + "Unmarshall (Stream, Res (I1";
  marshall_size += spaces + "Compute_New_Size (Stream, Val (I1";

  for (unsigned int i = 1; i < n_dims (); i++) 
    {
      char number[256];
      sprintf (number,"%d", i+1);

      marshall += ", I";
      marshall +=  number;
      unmarshall += ", I";
      unmarshall += number;
      marshall_size += ", I";
      marshall_size += number;
    }

  marshall += ")); \n";
  unmarshall += ")); \n";
  marshall_size += ")); \n";

  for (unsigned int i = 0; i < n_dims (); i++) 
    {
      spaces = spaces.substr (0, spaces.length ()-3);
      marshall += spaces + "end loop;\n";
      unmarshall += spaces + "end loop;\n";
      marshall_size += spaces + "end loop;\n";
    }
      
  marshall += "   end Marshall;\n\n";
  unmarshall += "   end Unmarshall;\n\n";
  marshall_size += "   end Compute_New_Size;\n\n";      

  body += marshall;
  body += unmarshall;
  body += marshall_size;

  set_already_defined ();
}

////////////////////////////////////////////////////////////////
//////////////             dump_name             ///////////////    
////////////////////////////////////////////////////////////////

string adabe_array::dump_name (dep_list & with,
			       string   & previous) 
{
  if (!is_imported (with))
    {
      if (!is_already_defined ())
	{
	  // has this array already been defined ?
	  string tmp = "";
	  produce_ads (with, tmp, previous);
	  previous += tmp;
	}
      // this array is defined in this file, so
      // a local name is enough
      return get_ada_local_name ();
    }
  // the array is defined in aother file
  // we need to use a full name
  return get_ada_full_name ();	   
}

////////////////////////////////////////////////////////////////
//////////////           marshal_name            ///////////////    
////////////////////////////////////////////////////////////////

string adabe_array::marshal_name (dep_list & with,
				  string   & previous) 
{
  if (!is_marshal_imported (with))
    {
     if (!is_already_defined ())
	{
	  // has this array already been defined ?
	  string tmp = "";
	  produce_stream_adb (with, tmp, previous);
	  previous += tmp;
	}
      // this array is defined in this file, so
      // a local name is enough
      return get_ada_local_name ();
    }
  // the array is defined in aother file
  // we need to use a full name
  return get_ada_full_name ();	   
}    
