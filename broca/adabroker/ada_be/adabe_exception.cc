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

IMPL_NARROW_METHODS1 (adabe_exception, AST_Exception);
IMPL_NARROW_FROM_DECL (adabe_exception);
IMPL_NARROW_FROM_SCOPE (adabe_exception);

//////////////////////////////////////////////////////////////////
//////////////////////// produce_ads /////////////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_exception::produce_ads (dep_list & with,
			      string   & body,
			      string   & previous)
{
  // Add two packages needed to handle exceptions.
  with.add ("Ada.Exceptions");

  // first is used to determine whether an exception has members or not.
  bool first = true;
    
  // Beginning of the exception declaration.
  body +=
    "   " + get_ada_local_name () + " : exception;\n\n"
    "   type " + get_ada_local_name () + "_Members is new "
    "CORBA.IDL_Exception_Members with\n";
  
  // Map the members of this exception.
  UTL_ScopeActiveIterator activator (this, UTL_Scope::IK_decls);
  while (!activator.is_done ())
    {
      AST_Decl *d = activator.item ();
      switch (d->node_type ())
	{
	case AST_Decl::NT_field:
	  {
	    adabe_name *adabe_field =  dynamic_cast<adabe_name *> (d);
	    if (first)
	      {
		// Use "record" for the first member.
		body += "   record\n";
		first = false;
	      }
	    // Like structures, call the produce ads on the field.
	    adabe_field->produce_ads (with, body, previous);
	    break;
	  }
	  
	default:
	  // Throw an exception.
	  throw adabe_internal_error
	    (__FILE__,__LINE__,"unexpected decl in exception scope");
	}
      activator.next ();
    } //
  if (first)
    {
      // The exception has no member.
      body += "null record;\n\n";
    }
  else
    {
      // the exception has members
      body += "   end record;\n\n";
    }

  // An exception needs a RepositoryID that designate it.
  //   body += "   " + get_ada_local_name ();
  //   body += "_Repository_Id : CORBA.String\n";
  //   body += "      := CORBA.To_CORBA_String (\"";
  //   body += repositoryID ();
  //   body += "\");\n\n";

  // We need a function to get the members of an exception.
  body += 
    "   procedure Get_Members\n"
    "     (From : in Ada.Exceptions.Exception_Occurrence;\n"
    "      To   : out " + get_ada_local_name () + "_Members);\n"
    "\n";
}

//////////////////////////////////////////////////////////////////
////////////////////     produce_adb      ////////////////////////
//////////////////////////////////////////////////////////////////

void
adabe_exception::produce_adb (dep_list & with,
			      string   & body,
			      string   & previous)
{
  with.add ("Broca.Exceptions");

  // The exception has only one operation to map: Get_Members.
  body +=
    "   procedure Get_Members\n"
    "     (From : in Ada.Exceptions.Exception_Occurrence;\n"
    "      To   : out " + get_ada_local_name () + "_Members) is\n"
    "   begin\n"
    "      Broca.Exceptions.User_Get_Members (From, To);\n"
//     "      To := " + get_ada_local_name () + "_Members\n"
//     "         (Broca.Exceptions.User_Get_Members (From));\n"
    "   end Get_Members;\n"
    "\n";
}

//////////////////////////////////////////////////////////////////
////////////////       produce_skel_adb       ////////////////////
//////////////////////////////////////////////////////////////////
//  This method is called by adabe_operation::produce_skel_adb to create
//  the code to transform a raised Ada exception into a marshalled exception.
void
adabe_exception::produce_skel_adb (dep_list & with,
				   string   & body)
{
  UTL_ScopeActiveIterator activator (this, UTL_Scope::IK_decls);
  bool has_member = !activator.is_done ();

  // Full name of the interface.
  string full_name = get_ada_full_name ();

  // Name of the package containing the interface.
  string pack = full_name.substr (0, full_name.find_last_of ('.'));

  // Add the package in which the exception is defined.
  with.add (pack);

  // We now map the interface
  body += 
    "            when E : " +  get_ada_local_name () + " =>\n"
    "               declare\n"
    "                  use " + pack + ".Stream;\n"
    "                  RepoID : String\n"
    "                     := \"" + repositoryID () + "\";\n";
  
  if (has_member)
    {
      // Declare members.
      body += 
	"                  Member : " + get_ada_local_name () + "_Members;\n";
    }
  body += 
    "               begin\n";
  if (has_member)
    {
      body += 
	"                  " + pack + ".Get_Members (E, Member);\n";
    }
  body += 
    "                  --  Compute size of reply\n"
    "                  Stream.Pos := Broca.Giop.Message_Header_Size;\n"
    "                  --  service context\n"
    "                  Marshall_Size_Unsigned_Long (Stream);\n"
    "                  --  request id\n"
    "                  Marshall_Size_Unsigned_Long (Stream);\n"
    "                  --  reply status\n"
    "                  Marshall_Size_Unsigned_Long (Stream);\n"

    "                  Marshall_Size (Stream, RepoID);\n";
  if (has_member)
    {
      // If there are members, add them when computing the size
      body += "                  Marshall_Size (Stream, Member);\n";
    }  
  body +=
    "                  Reply_Size :=\n"
    "                     Stream.Pos - Broca.Giop.Message_Header_Size;\n"
    "                  Increase_Buffer_And_Clear_Pos (Stream, Stream.Pos);\n"
    "\n"
    "                  Broca.Giop.Create_Giop_Header\n"
    "                     (Stream, Broca.Giop.Reply,\n"
    "                      CORBA.Unsigned_Long (Reply_Size));\n"
    "\n"
    "                  --  service context\n"
    "                  Marshall (Stream, CORBA.Unsigned_Long (0));\n"
    "                  --  request id\n"
    "                  Marshall (Stream, Request_Id);\n"
    "                  --  reply status\n"
    "                  Marshall (Stream, Broca.Giop.User_Exception);\n"
    "\n"
    "                  --  Marshall exception\n"
    "                  Marshall (Stream, RepoID);\n";
  if (has_member)
    body +=
      "                  Marshall (Stream, Member);\n";
  body += 
    "                  return;\n"
    "               end;\n";
}

////////////////////////////////////////////////////////////////
//////////////       produce_marshall_ads        ///////////////    
////////////////////////////////////////////////////////////////

void
adabe_exception::produce_stream_ads (dep_list & with,
				     string   & body,
				     string   & previous)
{
  string full_name = get_ada_full_name ();
  string pack = full_name.substr (0, full_name.find_last_of ('.'));

  with.add (pack);
  with.add ("Broca.Marshalling");
  with.add ("Broca.Types");

  UTL_ScopeActiveIterator activator (this, UTL_Scope::IK_decls);
  // We must define marshall, unmarshall and align size only if
  // there's an argument in the scope of the exception.
  if (!activator.is_done ())
    gen_marshalling_declarations (body, get_ada_local_name () + "_Members");

  body +=
    "   procedure Unmarshall_And_Raise_" + get_ada_local_name () + "\n"
    "      (Stream : in out Broca.Types.Buffer_Descriptor);\n"
    "\n"
    "   procedure Raise_" + get_ada_local_name () + "\n"
    "      (Bod : " + get_ada_local_name () + "_Members);\n";

  set_already_defined ();
}

////////////////////////////////////////////////////////////////
//////////////       produce_marshall_adb        ///////////////    
////////////////////////////////////////////////////////////////
void
adabe_exception::produce_stream_adb (dep_list & with,
				     string   & body,
				     string   & previous)
{
  // We must define marshall, unmarshall and align size only if
  // there's an argument in the scope of the exception.
  UTL_ScopeActiveIterator activator (this, UTL_Scope::IK_decls);
  if (!activator.is_done ())
    {
      // local strings used to compute the three different functions.
      string marshall = "";
      string unmarshall = "";
      string marshall_size = "";

      // declaration of the function marshall.
      marshall += 
	"   procedure Marshall\n"
	"     (Stream : in out Broca.Types.Buffer_Descriptor;\n"
	"      Val : in " + get_ada_local_name () + "_Members)\n"
	"   is\n"
	"   begin\n";

      // Declaration of the function unmarshall.
      unmarshall +=
	"   procedure Unmarshall\n"
	"     (Stream : in out Broca.Types.Buffer_Descriptor;\n"
	"      Res : out " + get_ada_local_name () + "_Members)\n"
	"   is\n"
	"   begin\n";

      // Declaration of the function align_size.
      marshall_size +=
	"   procedure Marshall_Size\n"
	"     (Stream : in out Broca.Types.Buffer_Descriptor;\n"
	"      Val : in " + get_ada_local_name () + "_Members)\n"
	"   is\n"
	"   begin\n";
      
      // We must now call the function over each field in the scope.
      while (!activator.is_done ())
	{
	  AST_Decl *d = activator.item ();
	  switch (d->node_type ())
	    {
	    case AST_Decl::NT_field:
	      {
		(dynamic_cast<adabe_field *>(d))->produce_stream_adb
		  (with, body, marshall, unmarshall, marshall_size);
		break;
	      }
	    default:
	      cerr << d->node_type () << endl;
	      throw adabe_internal_error
		(__FILE__,__LINE__,"unexpected decl in exception scope");
	    }      
	  activator.next ();
	}
      marshall   += "   end Marshall;\n\n";
      unmarshall += "   end Unmarshall;\n\n";
      marshall_size += "   end Marshall_Size;\n\n";
      
      body += marshall;
      body += unmarshall;
      body += marshall_size;

      with.add ("Broca.Exceptions");

      body +=
	"   procedure Unmarshall_And_Raise_" + get_ada_local_name () + "\n"
	"      (Stream : in out Broca.Types.Buffer_Descriptor)\n"
	"   is\n"
	"      Bod : Broca.Exceptions.IDL_Exception_Members_Acc;\n"
	"   begin\n"
	"      Bod := new " + get_ada_local_name () + "_Members;\n"
	"      Broca.Marshalling.Unmarshall_Skip_String (Stream);\n"
	"      Unmarshall (Stream, " 
	+ get_ada_local_name () + "_Members (Bod.all));\n"
	"      Broca.Exceptions.User_Raise_Exception\n"
	"         (" + get_ada_local_name () + "'Identity, Bod);\n"
	"   end Unmarshall_And_Raise_" + get_ada_local_name () + ";\n"
	"\n"
	"   procedure Raise_" + get_ada_local_name () + "\n"
	"      (Bod : " + get_ada_local_name () + "_Members)\n"
	"   is\n"
	"      Members : Broca.Exceptions.IDL_Exception_Members_Acc;\n"
	"   begin\n"
	"      Members := new " + get_ada_local_name () + "_Members'(Bod);\n"
	"      Broca.Exceptions.User_Raise_Exception\n"
	"         (" + get_ada_local_name () + "'Identity, Members);\n"
	"   end Raise_" +  get_ada_local_name () + ";\n";
    }
  set_already_defined ();
}

string
adabe_exception::dump_name (dep_list & with,
			   string   & previous) 
{
  if (!is_imported (with))
    {
      if (!is_already_defined ())
	{
	  // Has this exception already been defined ?
	  string tmp = "";
	  produce_ads (with, tmp, previous);
	  previous += tmp;
	}
      // This exception is defined in this file. Use local name.
      return get_ada_local_name ();
    }
  // The exception is defined in another file. Use full name.
  return get_ada_full_name ();	   
}

string
adabe_exception::marshal_name (dep_list& with, string &previous) 
{
  if (!is_marshal_imported (with))
    {
      if (!is_already_defined ())
	{
	  // Has this exception already been defined ?
	  string tmp = "";
	  produce_stream_adb (with, tmp, previous);
	  previous += tmp;
	}
      // This exception is defined in this file. se local name.
      return get_ada_local_name ();
    }
  // The exception is defined in another file. Use full name.
  return get_ada_full_name ();	   
}

adabe_exception::adabe_exception (UTL_ScopedName *n, UTL_StrList *p)
  : AST_Decl (AST_Decl::NT_except, n, p),
    AST_Structure (AST_Decl::NT_except, n, p),
    UTL_Scope (AST_Decl::NT_except),
    adabe_name (AST_Decl::NT_except, n, p)
{
}


