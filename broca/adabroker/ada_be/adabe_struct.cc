//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.3 $
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

adabe_structure::adabe_structure (UTL_ScopedName * n,
				  UTL_StrList    * p)
  : AST_Decl (AST_Decl::NT_struct, n, p),
    UTL_Scope (AST_Decl::NT_struct),
    adabe_name (AST_Decl::NT_struct, n, p)
{
}

//////////////////////////////////////////////////////////////////
/////////////////        produce_ads          ////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_structure::produce_ads (dep_list & with,
			      string   & body,
			      string   & previous)
{
  // This library will be needed for the Free function
  // with.add ("Ada.Unchecked_Deallocation");
  
  // beginning of the declaration
  body += "   type " + get_ada_local_name () + " is record\n";

  // we must now look in the scope and name all the fields
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      AST_Decl *d = i.item ();
      adabe_field *e = dynamic_cast<adabe_field *>(d);
      if (d->node_type () == AST_Decl::NT_field)
	{
	  // the currrent field is beeing produced
	  e->produce_ads (with, body, previous);
	}
      else throw adabe_internal_error 
	     (__FILE__,__LINE__,"Unexpected node in structure");
      if (!(dynamic_cast<adabe_name *>(e->field_type ()))->has_fixed_size ()) no_fixed_size ();
      i.next ();      
    }

  // the end of the Structure
  body += "   end record;\n\n";
  
  // the pointer  which will access the structure
  // body += "   type " + get_ada_local_name () + "_Ptr is access ";
  // body += get_ada_local_name () + ";\n\n";

  // the free function
  // body += "   procedure Free is new Ada.Unchecked_Deallocation (";
  // body += get_ada_local_name () + ", " + get_ada_local_name ()+ "_Ptr);\n";
  set_already_defined ();
}

//////////////////////////////////////////////////////////////////
///////////////       produce_stream_ads      ///////////////////
//////////////////////////////////////////////////////////////////

void
adabe_structure::produce_stream_ads (dep_list & with,
				     string   & body,
				     string   & previous)
{
  gen_marshalling_declarations (body, get_ada_local_name ());

  set_already_defined ();
}

//////////////////////////////////////////////////////////////////
///////////////       produce_stream_adb         ////////////////
//////////////////////////////////////////////////////////////////

void
adabe_structure::produce_stream_adb (dep_list & with,
				     string   & body,
				     string   & previous)
{
  string marshall = "";
  string unmarshall = "";
  string marshall_size = "";
  marshall += 
    "   procedure Marshall\n"
    "      (Stream : in out Broca.Types.Buffer_descriptor;\n"
    "       Val : " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n";
  
  unmarshall += 
    "   procedure Unmarshall\n"
    "      (Stream : in out Broca.Types.Buffer_descriptor;\n"
    "       Res : out " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n";

  marshall_size +=
    "   procedure Marshall_Size\n"
    "      (Stream : in out Broca.Types.Buffer_descriptor;\n"
    "       Val : " + get_ada_local_name () + ")\n"
    "   is\n"
    "   begin\n";

  // for all of the field we must lauch his
  // produce_marshall adb
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      AST_Decl *d = i.item ();
      if (d->node_type () == AST_Decl::NT_field) {
	dynamic_cast<adabe_field *>(d)->produce_stream_adb
	  (with, body, marshall, unmarshall, marshall_size);
      }
      else throw adabe_internal_error
	     (__FILE__,__LINE__,"Unexpected node in structure");
      i.next ();
    }

  marshall += "   end Marshall;\n\n";
  unmarshall += "   end Unmarshall;\n\n";
  marshall_size += "   end Marshall_Size;\n\n";

  body += marshall;
  body += unmarshall;
  body += marshall_size;

  set_already_defined ();
}

//////////////////////////////////////////////////////////////////
//////////////////        dump_name       ////////////////////////
//////////////////////////////////////////////////////////////////

string
adabe_structure::dump_name (dep_list & with,
			    string   & previous)
{
  if (!is_imported (with))
    {
      if (!is_already_defined ())
	{
	  // has this structure already been defined ?
	  string tmp = "";
	  produce_ads (with, tmp, previous);
	  previous += tmp;
	}
      // this structure is defined in this file, so
      // a local name is enough
      return get_ada_local_name ();
    }
  // because the structure is defined in another file
  // we need to use a full name
  return get_ada_full_name ();	   
}

//////////////////////////////////////////////////////////////////
////////////////       marshal_name       ////////////////////////
//////////////////////////////////////////////////////////////////

string
adabe_structure::marshal_name (dep_list & with,
			       string   & previous)
{
  if (!is_marshal_imported (with))
    {
      if (!is_already_defined ())
	{
	  // have the marshall functions for this
	  // structure already been defined
	  string tmp = "";
	  produce_stream_adb (with, tmp, previous);
	  previous += tmp;
	}
      // this structure is defined in this file, so
      // a local name is enough
      return get_ada_local_name ();
    }
  // because this structure is defined in another file
  // we need to use a full name
  return get_ada_full_name ();	   
}  
IMPL_NARROW_METHODS1 (adabe_structure, AST_Structure)
IMPL_NARROW_FROM_DECL (adabe_structure)
IMPL_NARROW_FROM_SCOPE (adabe_structure)





