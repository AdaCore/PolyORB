//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.2 $
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

////////////////////////////////////////////////////////////////////////
////////////////      constructor    ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
adabe_union::adabe_union (AST_ConcreteType * dt,
			  UTL_ScopedName   * n,
			  UTL_StrList      * p)
  : AST_Union (dt, n, p),
    AST_Decl (AST_Decl::NT_union, n, p),
    AST_Structure (AST_Decl::NT_union, n, p),
    UTL_Scope (AST_Decl::NT_union),
    adabe_name (AST_Decl::NT_union, n, p)
{
  pd_have_default_case = false;
}

////////////////////////////////////////////////////////////////////////
////////////////      produce_ads    ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_union::produce_ads (dep_list & with,
			  string   & body,
			  string   & previous)
{
  //compute_ada_name ();
  
  int count = 0;
  // to count the number of case when the discriminant type is an enum
  
  bool has_default_case = false;
  // to check if there is a default case

  string default_case = "";
  default_case += "         when others =>\n";
  default_case += "            null;\n";
  // it is the default case
  
  // getting the name of the switch
  adabe_name *b = dynamic_cast<adabe_name *>(disc_type ());
  string name = b->dump_name (with, previous);

  body += "   type " + get_ada_local_name ();
  body += "(Switch : "  + name;
  body += " := " + name + "'First) is\n";
  body += "   record\n";
  body += "      case Switch is\n";

  // we must now look for the differents values
  // that can be taken by the switch
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      AST_Decl *d = i.item ();
      if (d->node_type () == AST_Decl::NT_union_branch)
	{
	  // print the case of the branch
	  adabe_union_branch *unionbr = dynamic_cast<adabe_union_branch *>(d);
	  unionbr->produce_ads (with, body, previous, disc_type ());

	  // if a default cas ehas been found,
	  // set the flag to true
	  if (unionbr->label ()->label_kind () == AST_UnionLabel::UL_default)
	    has_default_case = true;

	  // increases the counter
	  count++;
	}
      else throw adabe_internal_error
	     (__FILE__,__LINE__,"Unexpected node in union");
      i.next ();
    }

  if (!has_default_case)
    {
      switch (disc_type ()->node_type ())
	{
	case AST_Decl::NT_enum:
	  if (count != (dynamic_cast<adabe_enum *>(b)->get_number_value ()))
	    // the switch type is an enum an all of the cases
	    // have not been seen
	    set_default_case (true);
	  break;
	case AST_Decl::NT_typedef:
	  if ((count != (dynamic_cast<adabe_typedef *>(b)->get_number_value ()))
	      && ((dynamic_cast<adabe_typedef *>(b)->get_number_value ()) > 0))
	    // the switch type is an enum and all of the cases
	    // have not been seen
	    set_default_case (true);
	  break;
	default:
	  // the switch if of type int ...
	  // and a default case maybe added
	  set_default_case (true);
	  break;
	}
    }

  // adding a default type if needed.
  // without it gnat won't accept this union
  if (get_default_case ()) body += default_case;
  
  // ending the declaration
  body += "      end case;\n";
  body += "   end record;\n\n";
  // body += "   type " + get_ada_local_name () + "_Ptr is access ";
  // body += get_ada_local_name () + ";\n\n";

  // defining the free function
  // body += "   procedure Free is new Ada.Unchecked_Deallocation (";
  // body += get_ada_local_name () + ", " + get_ada_local_name ()+ "_Ptr);\n";

  // this type has been defined
  set_already_defined ();
}

////////////////////////////////////////////////////////////////////////
////////////////    produce_marshall_adb    ////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_union::produce_stream_ads (dep_list & with,
				 string   & body,
				 string   & previous)
{
  // The three functions neede fore the net
  // transfert:
  body += "   procedure Marshall\n";
  body += "     (A : in " + get_ada_local_name () + ";\n";
  body += "      S : in out AdaBroker.NetBufferedStream.Object'Class);\n\n";

  body += "   procedure Unmarshall\n";
  body += "     (A : out " + get_ada_local_name () + ";\n";
  body += "      S : in out AdaBroker.NetBufferedStream.Object'Class);\n\n";

  body += "   function Align_Size\n";
  body += "     (A              : in " + get_ada_local_name () + ";\n";
  body += "      Initial_Offset : in CORBA.Unsigned_Long)\n";
  body += "      return CORBA.Unsigned_Long;\n\n";

  // the marshall function have been defined
  set_already_defined ();
}

////////////////////////////////////////////////////////////////////////
///////////////    produce_stream_adb    //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_union::produce_stream_adb (dep_list & with,
				 string   & body,
				 string   & previous)
{
  string disc_name =
    (dynamic_cast<adabe_name *>(disc_type ()))->marshal_name (with, previous); 

  // preparing the default case (it MUST NOT
  // serve )
  string default_case = "";
  default_case += "         when others =>\n";
  default_case += "            Ada.Exceptions.Raise_Exception\n";
  default_case += "              (CORBA.Dummy_User'Identity,\n";
  default_case += "               \"Unchecked union case used\");\n";

  // defining the marshall, unmarshall
  // and align size function
  // This type has a variable size and the
  // function must automatically be
  // adjusted
  string marshall = "";
  string unmarshall = "";
  string align_size = "";
  marshall += "   procedure Marshall\n";
  marshall += "     (A : in " + get_ada_local_name () + ";\n";
  marshall += "      S : in out AdaBroker.NetBufferedStream.Object'Class)\n";
  marshall += "   is\n";
  marshall += "   begin\n";
  marshall += "      Marshall (A.Switch, S);\n";
  marshall += "      case A.Switch is\n";
  
  unmarshall += "   procedure Unmarshall\n";
  unmarshall += "     (A : out " + get_ada_local_name () + ";\n";
  unmarshall += "      S : in out AdaBroker.NetBufferedStream.Object'Class)\n";
  unmarshall += "   is\n";
  unmarshall += "      Switch : " + disc_name + ";\n";
  unmarshall += "   begin\n";
  unmarshall += "      Unmarshall (Switch, S);\n";
  unmarshall += "      declare\n";
  unmarshall += "         Tmp : " + get_ada_local_name () + "(Switch);\n";
  unmarshall += "      begin\n";
  unmarshall += "         case Switch is\n";
  
  align_size += "   function Align_Size\n";
  align_size += "     (A              : in " + get_ada_local_name () + ";\n";
  align_size += "      Initial_Offset : in CORBA.Unsigned_Long)\n";
  align_size += "      return CORBA.Unsigned_Long\n";
  align_size += "   is\n";
  align_size += "      Tmp : CORBA.Unsigned_Long := 0;\n";
  align_size += "   begin\n";
  align_size += "      Tmp := Align_Size (A.Switch, Initial_Offset);\n";
  align_size += "      case A.Switch is\n";

  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      AST_Decl *d = i.item ();
      if (d->node_type () == AST_Decl::NT_union_branch) {
	dynamic_cast<adabe_union_branch *>(d)->produce_stream_adb
	  (with, marshall, unmarshall, align_size, disc_type ());
      }
      else throw adabe_internal_error 
	     (__FILE__,__LINE__,"Unexpected node in union");
      i.next ();
    }

  // if the defqult case is called
  // evenif it has not been defined in IDL
  // an exception is thrown
  if (get_default_case ())
    {
      marshall += default_case;
      align_size += default_case;
      unmarshall += "            when others =>\n";
      unmarshall += "               Ada.Exceptions.Raise_Exception\n";
      unmarshall += "                 (CORBA.Dummy_User'Identity,\n";
      unmarshall += "                  \"Unchecked union case used\");\n";

      with.add ("CORBA");
      with.add ("Ada.Exceptions");
    }

  marshall += "      end case;\n";
  marshall += "   end Marshall;\n\n";

  unmarshall += "         end case;\n";
  unmarshall += "      A := Tmp;\n";
  unmarshall += "      end;\n";
  unmarshall += "   end Unmarshall;\n\n";
  
  align_size += "      end case;\n";
  align_size += "      return Tmp;\n";
  align_size += "   end Align_Size;\n\n";

  body += marshall;
  body += unmarshall;
  body += align_size;

  // the marshall function has been written
  set_already_defined ();
}

////////////////////////////////////////////////////////////////////////
////////////////       dump_name     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
string
adabe_union::dump_name (dep_list & with,
			string   & previous)
{
  if (!is_imported (with))
    {
      if (!is_already_defined ())
	  // has this union already been defined ?
	{
	  string tmp = "";
	  produce_ads (with, tmp, previous);
	  previous += tmp;
	}
      // this union is defined in this file, so
      // a local name is enough
      return get_ada_local_name ();
    }
  // because this union is defined in another file
  // we need to use a full name
  return get_ada_full_name ();	   
}

////////////////////////////////////////////////////////////////////////
////////////////    marshal_name     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
string
adabe_union::marshal_name (dep_list & with,
			   string   & previous)
{
  if (!is_marshal_imported (with))
    {
      if (!is_already_defined ())
	{
	  // have the marshall functions for this
	  // union already been defined ?
	  string tmp = "";
	  produce_stream_adb (with, tmp, previous);
	  previous += tmp;
	}
      // this union is defined in this file, so
      // a local name is enough
       return get_ada_local_name ();
    }
  // because this union is defined in another file
  // we need to use a full name
  return get_ada_full_name ();	   
}

////////////////////////////////////////////////////////////////////////
////////////////      miscellaneous  ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
IMPL_NARROW_METHODS1 (adabe_union, AST_Union)
IMPL_NARROW_FROM_DECL (adabe_union)
IMPL_NARROW_FROM_SCOPE (adabe_union)





