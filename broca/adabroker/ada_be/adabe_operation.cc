//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.10 $
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
adabe_operation::adabe_operation (AST_Type             * rt,
				  AST_Operation::Flags   fl,
				  UTL_ScopedName       * n,
				  UTL_StrList          * p)
  : AST_Operation (rt, fl, n, p),
    AST_Decl (AST_Decl::NT_op, n, p),
    UTL_Scope (AST_Decl::NT_op),
    adabe_name (AST_Decl::NT_op, n, p)
{
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_ads (dep_list & with,
			      string   & body,
			      string   & previous)
  // This method produce the ads_file.
  // with is the dependence-list.
  // body is th main part of the file.
  // previous contains the local definition of complexe types.
{
  // Special code for operation mapped into function.
  if (is_function ())
    {
      body += "   function " + get_ada_local_name () + "\n";
      body += "     (Self : in Ref";

      // Declare a node list to push and dump all the operation nodes.
      UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);

      // While the node list is not empty, produce code for the
      // argument in the ads-file. All the nodes should be arguments,
      // else we throw an exception.
      while (!i.is_done ())
	{
	  body += ";\n      ";

	  // i.item is used to take an item from the node list.
	  AST_Decl *d = i.item ();

	  if (d->node_type () == AST_Decl::NT_argument)
	    // Cast it to select the method at run time.
	    dynamic_cast<adabe_name *>(d)->produce_ads (with, body, previous);
	  else throw adabe_internal_error
		 (__FILE__,__LINE__,"Unexpected node in operation");
       	  i.next ();
	}
      body += ")\n      return ";

      // return_type contains the return type of the operation.
      AST_Decl *b = return_type ();

      // Dump the type name and add it to the dependence-list.
      body += dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
      body += ";\n\n";
    }

  // If it is not a function.
  else
    {
      body += "   procedure " + get_ada_local_name () + "\n";
      body += "     (Self : in Ref";

      // Check the node list which must be arguments.
      UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
      while (!i.is_done ())
	{
	  body += ";\n      ";
	  
	  // Take a node from the list.
	  AST_Decl *d = i.item ();
	  if (d->node_type () == AST_Decl::NT_argument)
	    dynamic_cast<adabe_name *>(d)->produce_ads
	      (with, body, previous);
	  else throw adabe_internal_error
	 	 (__FILE__,__LINE__,"Unexpected node in operation");

	  // Jump to next node.
	  i.next (); 
	}

      // For a function, add the Result parameter.
      if (!return_is_void ()) {
	body += ";\n      Returns : out ";
	AST_Decl *b = return_type ();

	// Dump type name and add it to the dependence-list.
	body += dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
	}
      body += ");\n\n";
    }

  // Operation now defined.
  set_already_defined ();
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_adb     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_adb (dep_list & with,
			      string   & body,
			      string   & previous)
  // This method produce the adb_file,
  // with is the dependence-list.
  // body is th main part of the file.
  // previous contains the local definition of complexe types.
{
  // Is it an oneway operation ?
  bool oneway = false;

  switch (flags ())
    {
    case OP_noflags :
    case OP_idempotent :
      break;
    case OP_oneway :
      oneway = true;
      break;
    }

  // space is computed for the presentation (length of the name).
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name ().length ();i++) space += " ";

  // This string contains all the arguments.
  string in_decls = "";

  // This string contains the in arguments.
  string in_args = "";

  // This string contains the out arguments.
  string out_args = "";

  // Is there no out argument ?
  bool no_out = true;

  string marshall_size = "";
  string marshall = "";
  string unmarshall = "";

  // Parse all the argument nodes. Call the adb-file produce on the
  // argument with the previous strings and bool, and the dep-list as
  // parameter.
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      AST_Decl *d = i.item ();
      if (d->node_type () == AST_Decl::NT_argument)
	{
	  // This operation produces the different argument strings which
	  // will be useful. The cast is here explicit in order to call
	  // the right method.
	  dynamic_cast<adabe_argument *>(d)->produce_adb
	    (with, no_out, space, in_decls, in_args, out_args,
	     marshall_size, marshall, unmarshall);
	}
      else throw adabe_internal_error
	     (__FILE__,__LINE__,"Unexpected node in operation");
      i.next ();
    }

  // For a function, the return type is mapped into an argument Result.
  if ((!return_is_void ()) && (!is_function ()))
    {
      // The result is mapped into an out argument.
      out_args += ", Returns";
      AST_Decl *b = return_type ();
      string tmp = dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
      in_decls += ";\n              " + space + "Returns : out " +  tmp;
    }
  in_decls += ")";
  in_args += ");\n";
  out_args += ");\n";

  with.add ("Broca.Giop");
  with.add ("Broca.Object");
  with.add ("Broca.Marshalling");
  with.add ("Broca.Marshalling.Refs");

  // Create a CORBA string containing the name of the operation.
  string operation_name = get_ada_local_name () + "_Operation";
  body += "   ";
  body += operation_name;
  body += 
    " : constant CORBA.Identifier :=\n"
    "     CORBA.To_CORBA_String (\"";
  body += get_ada_local_name ();
  body += "\");\n\n";

  // Check whether the operation is a function.
  if (is_function ())
    body += "   function ";
  else
    body += "   procedure ";
  
  body += get_ada_local_name () + "\n";
  body += "     (Self : in Ref";
  body += in_decls;

  // Produce the specific part of the function ie the return type.
  if (is_function ())
    {
      body += "\n      return ";
      AST_Decl *b = return_type ();

      // Dump the type name and add it to the dependence-list.
      string name =
	(dynamic_cast<adabe_name *>(b))->dump_name (with, previous);
      body += name;
      body += "\n   is\n";
      body += "      Result : ";
      body += name;
      body += ";\n";
    }
  else
    body += "\n   is\n";

  // Produce now the body of the operation
  adabe_name  *c = dynamic_cast<adabe_name *>(ScopeAsDecl (defined_in ()));

  // This string contains the name of the package in which the
  // operation is defined.
  string name_of_the_package = c->get_ada_local_name ();

  // Then, produce the necessary fields to call the operation.
  // FIXME: name conflict
  body +=
    "      use Broca.Marshalling;\n"
    "      use Broca.Marshalling.Refs;\n"
    "      Handler : Broca.Giop.Request_Handler;\n"
    "      Sr_Res : Broca.Giop.Send_Request_Result_Type;\n"
    "   begin\n"
    "      loop\n"
    "         Broca.Giop.Send_Request_Size\n"
    "           (Handler, Broca.Object.Object_Acc (Get (Self)),\n"
    "           " + operation_name + ");\n"
    "\n"
    "         --  In and inout parameter.\n";
  body +=  marshall_size;
  body +=
    "\n"
    "         Broca.Giop.Send_Request_Marshall\n"
    "           (Handler, ";
  body += oneway ? "False" : "True";
  body += ", " + operation_name + ");\n";
  body += marshall;
  body +=
    "         Broca.Giop.Send_Request_Send\n"
    "           (Handler, Broca.Object.Object_Acc (Get (Self)), ";
  body += oneway ? "False" : "True";
  body += ", Sr_Res);\n"
    "         case Sr_Res is\n"
    "            when Broca.Giop.Sr_Reply =>\n"
    "               --  OD: Operation dependant.\n"
    "               --  Outcoming arguments.\n";
  if (oneway)
    {
      body +=
	"               raise program_error;\n";
    }
  else
    {
      body += unmarshall;
      if (is_function ())
	body +=	
	  "               Unmarshall (Handler.Buffer, Result);\n"
	  "               return Result;\n";
      else
	body +=	"               return;\n";
    }
  body +=
    "            when Broca.Giop.Sr_No_Reply =>\n";
  if (oneway)
    {
      body +=
	"            return;\n";
    }
  else
    {
    body +=
      "               raise Program_Error;\n";
    }
  body +=
    "            when Broca.Giop.Sr_User_Exception =>\n";
  UTL_ExceptlistActiveIterator except_iterator (exceptions ());

  body +=
    "               raise Program_Error;\n"
    "            when Broca.Giop.Sr_Forward =>\n"
    "               null;\n"
    "         end case;\n"
    "      end loop;\n";
  body += "   end " + get_ada_local_name () + ";\n\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_skel_ads     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_skel_ads (dep_list & with,
				   string   & body, 
				   string   & previous)
  // this method produce the impl_ads_file,
  // with is the dependence-list
  // body is th main part of the file
  // previous contains the local definition of complexe types in the operation
{
  // Spec for a function.
  if (is_function ())
    {
      // An impl take an access Object as first parameter.
      body += "   function " + get_ada_local_name () + "\n";
      body += "     (Self : access Object";

      // Check all the nodes (arguments) of the operation scope.
      UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
      while (!i.is_done ())
	{
	  body += ";\n      ";
	  AST_Decl *d = i.item ();
	  if (d->node_type () == AST_Decl::NT_argument)
	    // The impl_ads needs the same production as the ads file.
	    dynamic_cast<adabe_name *>(d)->produce_ads (with, body, previous);
	  else throw adabe_internal_error 
		 (__FILE__,__LINE__,"Unexpected node in operation");
	  i.next ();
	}

      // Dump name of the return type and check whether it is imported.
      body += ")\n      return ";
      AST_Decl *b = return_type ();
      body += dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
      body += " is abstract;\n\n";
    }

  // Produce when not a function.
  else
    {
      // An impl take an access Object as first parameter (mapping).
      body += "   procedure " + get_ada_local_name () + "\n";
      body += "     (Self : access Object";
      
      // Check all the nodes (arguments) of the operation scope.
      UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
      while (!i.is_done ())
	{
	  body += ";\n      ";
	  AST_Decl *d = i.item ();
	  if (d->node_type () == AST_Decl::NT_argument)
	    // The impl_ads needs the same production as the ads file  .
	     dynamic_cast<adabe_name *>(d)->produce_ads (with, body, previous);
	  else throw adabe_internal_error 
		 (__FILE__,__LINE__,"Unexpected node in operation");
	  i.next ();
	}

      // If return type is not void, map it as an argument (for a
      // procedure).
      if (!return_is_void ())
	{
	  body += ";\n      Returns : out ";
	  AST_Decl *b = return_type ();

	  // Dump type name and add its file in the dep-list if
	  // imported.
	  body +=  dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
	}
      body += ") is abstract;\n\n";
    }
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_skel_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_skel_adb (dep_list & with,
				   string   & body,
				   string   & private_definition)
  // Produce the skeleton_adb_file.
  // with is the dependence-list.
  // body is th main part of the file.
  // private_definition contains the private part of the mapping.
{
  // Is it an oneway operation ?
  bool oneway = false;

  switch (flags ())
    {
    case OP_noflags :
    case OP_idempotent :
      break;
    case OP_oneway :
      oneway = true;
      break;
    }

  // Full name of the operation
  string full_name = get_ada_full_name ();

  // Name of the current interface. Use for multiple inheritance.
  string pack_name = adabe_global::adabe_current_file ()->get_ada_full_name ();

  string result_name = "";
  string in_decls = "";
  string unmarshall = "";
  string call_args = "";
  string marshall = "";
  string marshall_size = "";
  bool no_in = true;
  bool no_out = true;
  
  // First, process each argument.
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      AST_Decl *d = i.item ();
      if (d->node_type () == AST_Decl::NT_argument)

	// Produce code for arguments. There is a direct cast because
	// this procedure is not virtual.
	dynamic_cast<adabe_argument *>(d)->produce_skel_adb
	  (with, in_decls, no_in, no_out,
	   unmarshall, call_args, marshall, marshall_size);
      else throw adabe_internal_error
	     (__FILE__,__LINE__,"Unexpected node in operation");
      i.next ();
    }

  // For a function, add the Result parameter.
  if ((!is_function ()) && (!return_is_void ()))
    {
      call_args += ", Returns ";
    }
  
  body += "      if Operation = \"" + get_ada_local_name () + "\" then\n";
  body += "         declare\n";
  body += in_decls;

  // If return type is not void, declare Result. It's either an
  // argument or a return value.
  if (!return_is_void ())
    {
      // Get result type.
      AST_Decl *b = return_type ();
      adabe_name *e = dynamic_cast<adabe_name *>(b);
      result_name = e->dump_name (with, private_definition);
      e->is_marshal_imported (with);
      body += "            Returns : ";
      body += result_name;
      body += ";\n";
    }

  body += "         begin\n";

  // For an oneway function, check the client does not expect a reply.
  if (oneway)
    {
      body +=
	"            --  Check the client does not expect any reply\n"
	"            if Response_Expected then\n"
	"               broca.Exceptions.Raise_Bad_Operation;\n"
	"            end if;\n";
    }
  // If there is an in argument.
  if (!no_in) {
    body += "            --  Unmarshalls arguments\n";
    body += unmarshall;
  }

  body += 
    "            --  Call implementation\n"
    "            ";

  // if it's a function, Return will take the value of return
  if (is_function ()) body += "Returns := ";
  body += get_ada_local_name ();
  body += " (Object_Acc (Obj)";
  body += call_args;
  body += ");\n";

  // Special code for oneway procedure
  if (!oneway)
    {
      body +=
	"            Stream.Pos := Broca.Giop.Message_Header_Size;\n"
	"            --  service context\n"
	"            Compute_New_Size (Stream, UL_Size, UL_Size);\n"
	"            --  Request_id\n"
	"            Compute_New_Size (Stream, UL_Size, UL_Size);\n"
	"            --  reply_status\n"
	"            Compute_New_Size (Stream, UL_Size, UL_Size);\n";
      body += marshall_size;
      if (!return_is_void ())
	body +=
	  "            --  return value\n"
	  "            Compute_New_Size (Stream, Returns);\n";
      body +=
	"            Reply_Size := Stream.Pos - Broca.Giop.Message_Header_Size;\n"
	"            Allocate_Buffer_And_Clear_Pos (Stream, Stream.Pos);\n"
	"\n"
	"            Broca.Giop.Create_Giop_Header\n"
	"              (Stream, Broca.Giop.Reply,\n"
	"               CORBA.Unsigned_Long (Reply_Size));\n"
	"\n"
	"            --  service context\n"
	"            Marshall (Stream, CORBA.Unsigned_Long (Broca.Giop.No_Context));\n"
	"            --  request id\n"
	"            Marshall (Stream, Request_Id);\n"
	"            --  reply status\n"
	"            Marshall (Stream, Broca.Giop.No_Exception);\n";
      body += marshall;
      if (!return_is_void ())
	body +=
	  "            --  return value\n"
	  "            Marshall (Stream, Returns);\n";
    }
  body +=
    "            return;\n";

  // See if this operation can raise exceptions
  UTL_ExceptlistActiveIterator except_iterator (exceptions ());
  bool user_exceptions = (! except_iterator.is_done ());

#ifdef DEBUG_OPERATION
  cerr << "begin of exception generation" << endl;
  if (user_exceptions)
    cerr << "     there is some exceptions" << endl;
  else
    cerr << "     actually, there is no exceptions" << endl;
#endif

  // If there is one or several possible exceptions.
  if (user_exceptions)
    {
      body +=
	"\n"
	"         exception\n";
      while (!except_iterator.is_done ())
	{
	  string tmp = "";
	  AST_Decl *d = except_iterator.item ();
	  
	  // Dispatch them and catch them at the end.
	  dynamic_cast<adabe_exception *>(d)->produce_skel_adb (with, tmp);
	  body += tmp;
	  except_iterator.next ();
	}
    }
    
  body +=
    "         end;\n"
    "      end if;\n"
    "\n";
}

/*
////////////////////////////////////////////////////////////////////////
////////////////     produce_stream_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_stream_adb (dep_list &with, string &body, string &previous)
  // this method produce the marshal_adb_file,
  // with is the dependence-list
  // body is the main part of the file
  // previous contains local predefined complex types
{
  // res contains the return type
  AST_Decl *res = return_type ();
  adabe_name *result = dynamic_cast<adabe_name *>(res);

  // is_marshal_imported check if the result is imported are not,
  // if yes, his marshal file is imported 
  if (!result->is_marshal_imported (with))
    {
      // if the type has not been already declared, do the rest
      if (!result->is_already_defined ())
	{
	  string tmp = "";
	  // we produce the marshal of the return type
	  result->produce_stream_adb (with, tmp, previous);
	  previous += tmp;
	}
    }
  // check all the arguments of the function
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      AST_Decl *d = i.item ();
      if (d->node_type () == AST_Decl::NT_argument)
	{
	  adabe_name *dd = dynamic_cast<adabe_argument *>(d);
	  // is_marshal_imported check if the result is imported are not,
	  // if yes, his marshal file is imported 
	  if (!dd->is_marshal_imported (with))
	    {
	      // if the type has not been already declared, do the rest
	      if (!dd->is_already_defined ())
		{
		  string tmp = "";
		  // we produce the marshal of the return type
		  dd->produce_stream_adb (with, tmp, previous);
		  previous += tmp;
		}
	    }	
	}
      else throw adabe_internal_error
      (__FILE__,__LINE__,"Unexpected node in operation");
      i.next ();
    }
}
*/

////////////////////////////////////////////////////////////////////////
////////////////     miscellaneous           ///////////////////////////
////////////////////////////////////////////////////////////////////////
bool  
adabe_operation::is_function ()
  // This function checks whether the operation is a function.
{
  // All arguments must be of mode in.
  AST_Argument::Direction test = AST_Argument::dir_IN;

  // If return is not void, ret = true;
  bool ret = !(return_is_void ());

  // Check all the arguments.
  UTL_ScopeActiveIterator i (this, UTL_Scope::IK_decls);
  while ((!i.is_done ()) && (ret))
    {      
      AST_Decl *e = i.item ();

      // When mode is in, continue.
      if (e->node_type () == AST_Decl::NT_argument)
	ret = (((AST_Argument::narrow_from_decl (e))->direction ()) == test);
      i.next ();
    }
  return (ret);
}

bool
adabe_operation::return_is_void ()
  // Is the return type void?
{
  AST_Decl *rtype = return_type ();
  if ((rtype->node_type () == AST_Decl::NT_pre_defined) &&
      (AST_PredefinedType::narrow_from_decl (rtype)->pt ()
          == AST_PredefinedType::PT_void))
    return true;
  else
    return false;
}

IMPL_NARROW_METHODS1 (adabe_operation, AST_Operation)
IMPL_NARROW_FROM_DECL (adabe_operation)
IMPL_NARROW_FROM_SCOPE (adabe_operation)












