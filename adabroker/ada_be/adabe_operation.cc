#include <adabe.h>

////////////////////////////////////////////////////////////////////////
////////////////      constructor    ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
adabe_operation::adabe_operation(AST_Type             * rt,
				 AST_Operation::Flags   fl,
				 UTL_ScopedName       * n,
				 UTL_StrList          * p)
  : AST_Operation(rt, fl, n, p),
    AST_Decl(AST_Decl::NT_op, n, p),
    UTL_Scope(AST_Decl::NT_op),
    adabe_name(AST_Decl::NT_op, n, p)
{
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_ads(dep_list & with,
			     string   & body,
			     string   & previous)
  // This method produce the ads_file.
  // with is the dependence-list.
  // body is th main part of the file.
  // previous contains the local definition of complexe types.
{
  // oneway should only be added as a comment 
  switch (flags())
    {
    case OP_noflags :
    case OP_idempotent :
      break;
    case OP_oneway :
      body += "   ---   oneway   ---\n";
      break;
    }

  // Special code for operation mapped into function.
  if (is_function())
    {
      // space is computed for presentation (length of the name).
      string space = "";
      for (unsigned int i = 0; i < get_ada_local_name().length(); i++)
	space += " ";
      body += "   function " + get_ada_local_name() + " (Self : in Ref";

      // Declare a node list to push and dump all the operation nodes.
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);

      // While the node list is not empty, produce code for the
      // argument in the ads-file. All the nodes should be arguments,
      // else we throw an exception.
      while (!i.is_done())
	{
	  body += ";\n";
	  body += "             " + space;

	  // i.item is used to take an item from the node list.
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    // Cast it to select the method at run time.
	    dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error
		 (__FILE__,__LINE__,"Unexpected node in operation");
       	  i.next();
	}
      body += ")\n";
      body += "             " + space + "return ";

      // return_type contains the return type of the operation.
      AST_Decl *b = return_type();

      // Dump the type name and add it to the dependence-list.
      body += dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
      body += ";\n\n\n";
    }

  // If it is not a function.
  else
    {
      body += "   procedure " + get_ada_local_name() + " (Self : in Ref";

      // Check the node list which must be arguments.
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";

	  // Take a node from the list.
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    dynamic_cast<adabe_name *>(d)->produce_ads
	      (with, body, previous);
	  else throw adabe_internal_error
		 (__FILE__,__LINE__,"Unexpected node in operation");
	  // Jump to next node.
	  i.next();
	}
      // If it was an IDL function, add the Result parameter.
      if (!return_is_void()) {
	body += "; Returns : out ";
	AST_Decl *b = return_type();
	// Dump the type name and add it to the dependence-list.
	body += dynamic_cast<adabe_name *>(b)->dump_name(with, previous);
	}
      body += ");\n\n\n";
    }
  // The operation is now defined.
  set_already_defined ();
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_adb     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_adb(dep_list & with,
			     string   & body,
			     string   & previous)
  // This method produce the adb_file,
  // with is the dependence-list.
  // body is th main part of the file.
  // previous contains the local definition of complexe types.
{
  // Add some useful package to the dep-list.
  with.add ("AdaBroker.OmniProxyCallWrapper");

  // Boolean to determine whether the operation is oneway.
  bool oneway = false;

  // Determine boolean oneway.
  switch (flags())
    {
    case OP_noflags :
    case OP_idempotent :
      break;
    case OP_oneway :
      oneway = true;
      body += "   ---   oneway   ---\n";
    break;
    }

  // space is computed for the presentation (length of the name).
  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";

  // This string contains all the arguments.
  string in_decls = "";

  // This string contains the in arguments.
  string in_args = "";

  // This string contains the out arguments.
  string out_args = "";

  // Is there no out argument ?
  bool no_out = true;
  
  // Parse all the argument nodes. Call the adb-file produce on the
  // argument with the previous strings and bool, and the dep-list as
  // parameter.
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	// This operation produces the different argument strings which
	// will be useful. The cast is here explicit in order to call
	// the right method,
	dynamic_cast<adabe_argument *>(d)->produce_adb
	  (with, no_out, space, in_decls, in_args, out_args);
      else throw adabe_internal_error
	     (__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }

  // For a function, the return type is mapped into an argument Result.
  if ((!return_is_void()) && (!is_function()))
    {
      // The result is mapped into an out argument.
      out_args += ", Returns";
      AST_Decl *b = return_type();
      string tmp = dynamic_cast<adabe_name *>(b)->dump_name (with, previous);
      in_decls += ";\n              " + space + "Returns : out " +  tmp;
    }
  in_decls += ")";
  in_args += ");\n";
  out_args += ");\n";
  
  // Check whether the operation is a function.
  if (is_function())
    body += "   function ";
  else
    body += "   procedure ";
  
  body += get_ada_local_name() + " (Self : in Ref";
  body += in_decls;

  // Produce the specific part of the function ie the return type.
  if (is_function())
    {
      body += "\n             " + space + "return ";
      AST_Decl *b = return_type();
      // Dump the type name and add it to the dependence-list.
      string name = (dynamic_cast<adabe_name *>(b))->dump_name(with, previous);
      body += name;
    }
    
  body += " is\n";

  // Produce now the body of the operation
  adabe_name  *c = dynamic_cast<adabe_name *>(ScopeAsDecl(defined_in()));

  // This string contains the name of the package in which the
  // operation is defined.
  string name_of_the_package = c->get_ada_local_name();

  // Then, produce the necessary fields to call the operation.
  body += "      Opcd : " + name_of_the_package + ".Proxies.";
  body += get_ada_local_name() + "_Proxy;\n";  
  body += "   begin \n";
  body += "      " + name_of_the_package + ".Proxies.Init (Opcd";
  body += in_args;
	  

  body += "      AdaBroker.OmniProxyCallWrapper.";

  // the oneway needs a specific invoke call
  if (oneway)
    {
      body += "One_Way (Self, Opcd);\n";
    }
  else
    {
      // if there is out argument(s), we need a get_result function
      body += "Invoke (Self, Opcd);\n";
      if (is_function()) {
	body += "      return " + name_of_the_package;
	body += ".Proxies.Get_Result(Opcd);\n";
      }
      else if (!no_out)
	{
	  body += "      " + name_of_the_package;
	  body += ".Proxies.Get_Result(Opcd" + out_args;
	}
    }
  body += "   end;\n\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_ads     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_impl_ads(dep_list & with,
				  string   & body, 
				  string   & previous)
  // this method produce the impl_ads_file,
  // with is the dependence-list
  // body is th main part of the file
  // previous contains the local definition of complexe types in the operation
{
  // this case-of check the flags and set the boolean oneway
  switch (flags())
    {
    case OP_noflags :
    case OP_idempotent :
      break;
    case OP_oneway :
      body += "   ----   oneway   ----\n";
      break;
    }

  // a specific production for the functions
  if (is_function())
    {
      // in the impl,we need to take an access Object as first parameter (mapping)
      body += "   function " + get_ada_local_name() + "(Self : access Object";

      // check all the nodes (arguments) of the operation scope 
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    // the impl_ads needs the same production as the ads file  
	    dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      // dump the name of the return type and verify if it is imported are not (in the dump_name)
      body += ") return ";
      AST_Decl *b = return_type();
      body +=  dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + ";\n\n";
    }
  // production if it is not a function
  else
    {
      // in the impl, we need to take an access Object as first parameter (mapping)
      body += "   procedure " + get_ada_local_name() + "(Self : access Object";
      
      // check all the nodes (arguments) of the operation scope 
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    // the impl_ads needs the same production as the ads file  
	     dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      // if the return type is non void, it is mapped in an argument (for a procedure)
      if (!return_is_void())
	{
	  body += "; Returns : out ";
	  AST_Decl *b = return_type();

	  // we dump the type name and add its file in the dep-list if imported (in the dump_name)  
	  body +=  dynamic_cast<adabe_name *>(b)->dump_name(with, previous);
	}
      body += ");\n";
    }
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_impl_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_impl_adb(dep_list& with,string &body, string &previous)
  // this method produce the impl_adb_file,
  // with is the dependence-list
  // body is th main part of the file
  // previous contains the local definition of complexe types in the operation
{

  body += "   --  " + get_ada_local_name() + "\n";
  body += "   -------------------------------\n";
  // this case-of check the flags and set the boolean oneway
  switch (flags())
    {
    case OP_noflags :
    case OP_idempotent :
      break;
    case OP_oneway :
      body += "   ---    oneway    ---\n";
      break;
    }
  // a specific production for the functions
  if (is_function())
    {
      // in the impl, we need to take an access Object as first parameter (mapping)
      body += "   function " + get_ada_local_name() + "(Self : access Object";

      // check all the nodes (arguments) of the operation scope 
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    // the impl_ads needs the same production as the ads file  
	    dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      // dump the name of the return type and verify if it is imported are not (in the dump_name)
      body += ") return ";
      AST_Decl *b = return_type();
      body +=  dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + " is\n";
      body += "   begin \n";
      // the implementation of the operation is empty, it should be completed by the server
      body += "   end;\n";
    }
  // production if it is not a function
  else
    {
      // in the impl, we need to take an access Object as first parameter (mapping)
      body += "   procedure " + get_ada_local_name() + "(Self : access Object";

      // check all the nodes (arguments) of the operation scope 
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    // the impl_ads needs the same production as the ads file  
	    dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      // if the return type is non void, it is mapped in an argument (for a procedure)
      if (!return_is_void()) {
	   AST_Decl *b = return_type();
	  // we dump the type name and add its file in the dep-list if imported (in the dump_name)  
           string name =   dynamic_cast<adabe_name *>(b)->dump_name(with, previous);
	   body += "; Returns : out " + name + ") is\n";
      }
      else   body += ") is \n";
      body += "   begin \n";
      // the implementation  of the operation is empty, it should be completed by the server
      body += "   end;\n";      
    }
  body += "\n\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_ads     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_proxies_ads(dep_list& with,string &body, string &private_definition)
  // this method produce the proxies_ads_file,
  // with is the dependence-list
  // body is th main part of the file
  // private_definition contains the private part of the mapping of the operation
{
  // is it a "oneway" file?
  bool oneway = (flags() == OP_oneway);

  // name contains the full name of the operation 
  string name = get_ada_full_name();

  // in_decls contains the string of the in arguments
  string in_decls = "";

  // fields contains the string of all arguments pointer
  // it is added in the private part
  string fields = "";

  // is there any out/in arguments
  bool no_in = true;
  bool no_out = true;

  // out_args contains the string of the out arguments
    string out_args = "";

  // See if this operation can raise exceptions
  UTL_ExceptlistActiveIterator except_iterator(exceptions());
  bool user_exceptions = (! except_iterator.is_done());

  // first process each argument
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	// this operation produce the different argument strings which will be usefull
	// the cast is here explicit in order to call the right method
	dynamic_cast<adabe_argument *>(d)->produce_proxies_ads(with, in_decls, no_in, no_out, fields, out_args);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  // if it is a IDL function, the return type is mapped in an argument "Result" 
  if ((!return_is_void()) && (!is_function())) 
    {
      // the result is mapped in an out argument
      AST_Decl *b = return_type();
      string tmp = dynamic_cast<adabe_name *>(b)->dump_name(with, private_definition);
      out_args += "; Returns : out " + tmp;
      fields += "      Returns : ";
      fields += tmp;
      fields += ";\n";
    }
   in_decls += ");\n\n";

  // Get result type.
  string previous = "";
  AST_Decl *b = return_type();
  string result_name =
    dynamic_cast<adabe_name *>(b)->dump_name(with, previous); 

  // Produce all the functions needed for a call.
  body += "   type " + get_ada_local_name() + "_Proxy is\n";
  body += "      new AdaBroker.OmniProxyCallDesc.Object with private;\n\n";
  body += "   procedure Init\n";
  body += "     (Self : in out " + get_ada_local_name() + "_Proxy";
  body += in_decls;  
  body += "   function Operation\n";
  body += "     (Self : in " + get_ada_local_name() + "_Proxy)\n";
  body += "      return CORBA.String;\n\n";

  // Without in arguments, there is no marshall and align_size operations.
  if (!no_in) {
      body += "   function Align_Size\n";
      body += "     (Self    : in " + get_ada_local_name() + "_Proxy;\n";
      body += "      Size_In : in CORBA.Unsigned_Long)\n";
      body += "      return CORBA.Unsigned_Long;\n\n";
      body += "   procedure Marshal_Arguments\n";
      body += "     (Self        : in " + get_ada_local_name() + "_Proxy;\n";
      body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object);\n\n";
  }
  
  // With out arguments, we need an unmarshall operation.
  if ((!no_out) || (is_function()))
    {
      body += "   procedure Unmarshal_Returned_Values\n";
      body += "     (Self        : in out ";
      body +=  get_ada_local_name() + "_Proxy;\n";
      body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object);\n\n";
    }

  // With an out argument, we need a get_result.
  if (is_function())
    {
      // get_result should be a function.
      body += "   function Get_Result\n";
      body += "     (Self : in " + get_ada_local_name() + "_Proxy)\n";
      body += "      return " +  result_name + "; \n\n\n";
    }
  else
    {
      if (!no_out)
	{
	  // get_result should be a procedure.
	  body += "   procedure Get_Result\n";
	  body += "     (Self : in " + get_ada_local_name() + "_Proxy";
	  body += out_args;
	  body += ");\n\n\n";
	}
    }
  // When an exception is raised, use the exception dispatch procedure.
  if (user_exceptions) {
    body += "   procedure User_Exception\n";
    body += "     (Self        : in " + get_ada_local_name() + "_Proxy;\n";
    body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object;\n";
    body += "      RepoID      : in CORBA.String);\n\n\n";
  }


  private_definition += "   type " + get_ada_local_name() + "_Proxy is\n";
  private_definition += "      new AdaBroker.OmniProxyCallDesc.Object\n";
  private_definition += "      with record \n";

  // Without arguments, the private definitions is null.
  if ((fields == "") && (!is_function()))
    {
      private_definition += "      null;\n";
    }
  else
    {
      // Fill the private part with fields.
      private_definition += fields;
    }
  // Add an argument for a function.
  if (is_function())
    {
      private_definition += "      Private_Result : ";
      private_definition += result_name + ";\n";
    }
  private_definition += "   end record; \n";

  // Add a finalize procedure for a function or when there are arguments.
  if (!((fields == "") && (!is_function())))
    {
      private_definition += "   procedure Finalize\n";
      private_definition += "      (Self : in out "
	+ get_ada_local_name() + "_Proxy);\n";
    }
  private_definition += "\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_proxies_adb(dep_list & with,
				     string   & body,
				     string   & private_definition)
  // This method produce the proxies_adb_file,
  // with is the dependence-list.
  // body is th main part of the file.
  // private_definition contains the private part of the mapping.
{
  // Full name of the operation 
  string name = get_ada_full_name();

  // see adabe_argument.cc in produce_proxies_adb for details 
  string result_name = "";
  string in_decls = "";
  string init = "";
  string align_size = "";
  string marshall = "";
  string unmarshall_decls = "";
  string unmarshall = "";
  string finalize = "";
  string out_args = "";
  string result_decls = "";
  bool no_in = true;
  bool no_out = true;

  // First process each argument
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	// Call the produce_proxies_adb of the argument. There is a
	// direct cast because this procedure is not virtual
	dynamic_cast<adabe_argument *>(d)->produce_proxies_adb
	  (with, in_decls, no_in, no_out, init, align_size, marshall,
	   unmarshall_decls, unmarshall, finalize, out_args, result_decls);
      else throw adabe_internal_error
	     (__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  // For a function, the return type is mapped in an argument Result.
  if ((!return_is_void()) && (!is_function())) 
    {
      AST_Decl *b = return_type();
      string tmp = dynamic_cast<adabe_name *>(b)->dump_name
	(with, private_definition);
      unmarshall_decls += "      Returns : ";
      unmarshall_decls += tmp;
      unmarshall_decls += ";\n";
      unmarshall += "      Unmarshall (Returns ,GIOP_Client);\n";
      unmarshall += "      Self.Returns := Returns;\n";
      out_args += "; Returns : out " + tmp;
      result_decls += "      Returns := Self.Returns;\n";
      finalize += "      null;\n";
   }
  in_decls += ") is\n";

  // See if this subprogram can raise exceptions
  UTL_ExceptlistActiveIterator except_iterator(exceptions());
  bool user_exceptions = (! except_iterator.is_done());

  // Implementation of functions needed to call operations.
  body += "   procedure Init\n";
  body += "     (Self : in out " + get_ada_local_name() + "_Proxy";
  body += in_decls;
  body += "   begin\n";
  body += "      Set_User_Exceptions\n";
  body += "        (Self, ";

  // If there is an exception, pass true to Set_User_Exceptions.
  if (user_exceptions)
    {
      body += "True";
    }
  else
    {
      body += "False";
    };
  body += ");\n";
  body += init;
  body += "   end;\n\n\n";
  body += "   function Operation\n";
  body += "     (Self : in " + get_ada_local_name() + "_Proxy)\n";
  body += "      return CORBA.String is\n";
  body += "   begin\n";
  body += "      return CORBA.To_CORBA_String (\""
    + get_ada_local_name() + "\");\n";
  body += "   end Operation;\n\n\n";

  // If there is an argument in
  if (!no_in) {
    body += "   function Align_Size\n";
    body += "     (Self    : in " + get_ada_local_name() + "_Proxy;\n";
    body += "      Size_In : in CORBA.Unsigned_Long)\n";
    body += "     return CORBA.Unsigned_Long is\n";
    body += "      Tmp : CORBA.Unsigned_Long := Size_In;\n";
    body += "   begin\n";
    body += align_size;
    body += "      return Tmp;\n";
    body += "   end Align_Size;\n\n\n";
    
    body += "   procedure Marshal_Arguments\n";
    body += "     (Self        : in ";
    body += get_ada_local_name() + "_Proxy;\n";
    body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object) is\n";
    body += "   begin\n";
    body += marshall;
    body += "   end Marshal_Arguments;\n\n\n";      
  }

  // If there is an out argument
  if ((!no_out) || (is_function()))
    {
      body += "   procedure Unmarshal_Returned_Values\n";
      body += "     (Self        : in out ";
      body += get_ada_local_name() + "_Proxy;\n";
      body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object) is\n";
      body += unmarshall_decls;

      // For a function, add a returned type.
      if (is_function())
	{
	  // Get result type.
	  AST_Decl *b = return_type();
	  result_name = 
	    dynamic_cast<adabe_name *>(b)->dump_name
	    (with, private_definition); 
	  dynamic_cast<adabe_name *>(b)->is_marshal_imported(with);
	  body += "      Returns : " + result_name + ";\n";
	}
      body += "   begin\n";
      body += unmarshall;

      // For a function, add returned type.
      if (is_function())
	{
	  body += "      Unmarshall (Returns, GIOP_client);\n";
	  body += "      Self.Private_Result := Returns;\n";
	}
      body += "   end;\n\n\n";      
    }

  // For a function, add a returned type.
  if (is_function())
    {
      body += "   function Get_Result\n";
      body += "     (Self : in " + get_ada_local_name() + "_Proxy)\n";
      body += "     return " +  result_name + " is\n";
      body += "   begin\n";
      body += "      return Self.Private_Result;\n";
      body += "   end Get_Result;\n\n\n";
    }
  else
    {
      // If there is out arguments in a procedure, get_result exists.
      if (!no_out)
	{
	  body += "   procedure Get_Result\n";
	  body += "     (Self : in " + get_ada_local_name() + "_Proxy";
	  body += out_args;
	  body += ") is\n";
	  body += "   begin\n";
	  body += result_decls;
	  body += "   end Get_Result;\n\n\n";
	}
    }
  // When an exception is raised, use exception dispatch procedure.
  if (user_exceptions) {
    body += "   procedure User_Exception\n";
    body += "     (Self        : in " + get_ada_local_name() + "_Proxy;\n";
    body += "      GIOP_Client : in out AdaBroker.GIOP_C.Object;\n";
    body += "      RepoID      : in CORBA.String) is\n";
    body += "   begin\n";

    // check all the exceptions which can be raised
    while (!except_iterator.is_done())
      {
	string tmp = "";
	AST_Decl *d = except_iterator.item();

	// call the produce_proxies_adb of the exception (not virtual)
	// tmp will contain the production for the exception
	dynamic_cast<adabe_exception *>(d)->produce_proxies_adb(with,tmp);
	body += tmp;
	except_iterator.next();
      }
    // when there's an other exception raised, AdaBroker_Fatal_Error will be raised
    body += "      Ada.Exceptions.Raise_Exception(CORBA.AdaBroker_Fatal_Error'Identity,\n";
    body += "                                     \"In ";
    body += get_ada_local_name();
    body += "_Proxy.User_Exception, unknown exception.\");\n";
    body += "   end;\n";
  }
  // if there's at least one argument, the finalize is necessary 
  if ( (!no_in) || (!no_out) || (is_function())) {
    body += "   -- Finalize\n";
    body += "   -----------\n";
    body += "   procedure Finalize(Self : in out " + get_ada_local_name() + "_Proxy) is\n";
    body += "   begin\n";
    body += finalize;
    if (is_function()) {
      body += "      null;\n";
    } 
    body += "   end;\n\n\n";
  }
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_skel_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_skel_adb(dep_list& with,string &body, string &private_definition)
  // this method produce the skeleton_adb_file,
  // with is the dependence-list
  // body is th main part of the file
  // private_definition contains the private part of the mapping of the operation
{
  // is the operation oneway
  bool oneway = ( flags() == OP_oneway);

  // full name of the operation
  string full_name = get_ada_full_name();

  // the name of the current interface should be taken because of the multiple inheritance
  string pack_name = adabe_global::adabe_current_file()->get_ada_full_name();

  // see adabe_argument.cc in produce_skel_adb for details 
    string result_name = "";
  string in_decls = "";
  string unmarshall = "";
  string call_args = "";
  string marshall = "";
  string align_size = "";
  bool no_in = true;
  bool no_out = true;
  
  // First process each argument
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	// call the produce_skel_adb of the argument
	// there is a direct cast because this procedure is not virtual
	dynamic_cast<adabe_argument *>(d)->produce_skel_adb(with,
							     in_decls ,
							     no_in,
							     no_out,
							     unmarshall,
							     call_args,
							     marshall,
							     align_size);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  // if it was an IDL function, add the Result parameter
  if ((!is_function()) && (!return_is_void()))
    {
      call_args += ", Returns ";
    }
  
  body += "      -- " + full_name + "\n";
  if (oneway)
    {
      body += "      -- oneway \n";
    }
  body += "      -----------------------\n";
  body += "      if Orl_Op = \"";
  body += get_ada_local_name ();
  body += "\" then\n";
  body += "         declare\n";
  body += in_decls;
  // if the return type is non void, result should be declared
  // it's either an argument, or a return value
  if (!return_is_void())
    {
      // get the type of the result
      AST_Decl *b = return_type();
      adabe_name *e = dynamic_cast<adabe_name *>(b);
      result_name = e->dump_name(with, private_definition);
      e->is_marshal_imported(with);
      body += "            Returns : ";
      body += result_name;
      body += ";\n";
    }
  body += "            Size : CORBA.Unsigned_Long;\n";

  body += "         begin\n";

  // For an oneway function, check the client does not expect a reply.
  if (oneway) {
    body += "            -- check that the client does not expect any reply\n";
    body += "            if Orl_Response_Expected then\n";
    body += "               declare\n";
    body += "                  Member : CORBA.Bad_Operation_Members\n";
    body += "                         := (0, COMPLETED_NO);\n";
    body += "               begin\n";
    body += "                  AdaBroker.Exceptions.Raise_Corba_Exception\n";
    body += "                    (CORBA.Bad_Operation'Identity,\n";
    body += "                     Member);\n";
    body += "               end;\n";
    body += "            end if;\n";
  }
  // If there is an in argument.
  if (!no_in) {
    body += "            -- Unmarshalls arguments\n";
    body += unmarshall;
  }
  body += "            -- Change state\n";
  body += "            AdaBroker.GIOP_S.Request_Received (Orls);\n";

  body += "            -- Call implementation\n";
  body += "            ";

  // if it's a function, Return will take the value of return
  if (is_function()) body += "Returns := ";
  body += pack_name;
  body += ".Impl.";
  body += get_ada_local_name ();
  body += " (Self";
  body += call_args;
  body += ");\n";

  // specific treatement for oneway procedure
  if (!oneway)
    {
      body += "            -- Compute size of reply\n";
      body += "            Size := AdaBroker.GIOP_S.Reply_Header_Size;\n";
      body += align_size;

      // Align Return
      if (!return_is_void())
	{
	  body += "            Size := Align_Size (Returns, Size);\n";
	}
      body += "            -- Initialize reply\n";
      body += "            AdaBroker.GIOP_S.Initialize_Reply\n";
      body += "              (Orls,\n";
      body += "               AdaBroker.GIOP.NO_EXCEPTION,\n";
      body += "               Size);\n";
      
      body += "            -- Marshall arguments\n";
      body += marshall;
      
      if (!return_is_void())
	body += "            Marshall (Returns, Orls);\n";
      
    }
  body += "            -- Inform ORB\n";
  body += "            AdaBroker.GIOP_S.Reply_Completed (Orls);\n";

  body += "            Dispatch_Returns := True;\n";
  body += "            return;\n";

  // See if this operation can raise exceptions
  UTL_ExceptlistActiveIterator except_iterator(exceptions());
  bool user_exceptions = (! except_iterator.is_done());

#ifdef DEBUG_OPERATION
  cerr << "begin of exception generation" << endl;
  if (user_exceptions)
    cerr << "     there is some exceptions" << endl;
  else
    cerr << "     actually, there is no exceptions" << endl;
#endif

  // If there is one or several possible exceptions.
  if (user_exceptions) {
    body += "\n         exception\n";
    while (!except_iterator.is_done())
      {
	string tmp = "";
	AST_Decl *d = except_iterator.item();

	// Dispatch them and catch them at the end.
	dynamic_cast<adabe_exception *>(d)->produce_skel_adb(with, tmp);
	body += tmp;
	except_iterator.next();
      }
  }
    
  body += "         end;\n";
  body += "      end if;\n\n";
}

/*
////////////////////////////////////////////////////////////////////////
////////////////     produce_marshal_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_operation::produce_marshal_adb(dep_list &with, string &body, string &previous)
  // this method produce the marshal_adb_file,
  // with is the dependence-list
  // body is the main part of the file
  // previous contains local predefined complex types
{
  // res contains the return type
  AST_Decl *res = return_type();
  adabe_name *result = dynamic_cast<adabe_name *>(res);

  // is_marshal_imported check if the result is imported are not,
  // if yes, his marshal file is imported 
  if (!result->is_marshal_imported(with))
    {
      // if the type has not been already declared, do the rest
      if (!result->is_already_defined())
	{
	  string tmp = "";
	  // we produce the marshal of the return type
	  result->produce_marshal_adb(with, tmp, previous);
	  previous += tmp;
	}
    }
  // check all the arguments of the function
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	{
	  adabe_name *dd = dynamic_cast<adabe_argument *>(d);
	  // is_marshal_imported check if the result is imported are not,
	  // if yes, his marshal file is imported 
	  if (!dd->is_marshal_imported(with))
	    {
	      // if the type has not been already declared, do the rest
	      if (!dd->is_already_defined())
		{
		  string tmp = "";
		  // we produce the marshal of the return type
		  dd->produce_marshal_adb(with, tmp, previous);
		  previous += tmp;
		}
	    }	
	}
      else throw adabe_internal_error
      (__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
}
*/

////////////////////////////////////////////////////////////////////////
////////////////     miscellaneous           ///////////////////////////
////////////////////////////////////////////////////////////////////////
bool  
adabe_operation::is_function()
  // This function checks whether the operation is a function.
{
  // All arguments must be of mode in.
  AST_Argument::Direction test = AST_Argument::dir_IN;

  // If return is not void, ret = true;
  bool ret = !(return_is_void());

  // Check all the arguments.
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while ((!i.is_done()) && (ret))
    {      
      AST_Decl *e = i.item();

      // When mode is in, continue.
      if (e->node_type() == AST_Decl::NT_argument)
	ret = (((AST_Argument::narrow_from_decl(e))->direction()) == test);
      i.next();
    }
  return(ret);
}

bool
adabe_operation::return_is_void()
  // Is the return type void?
{
  AST_Decl *rtype = return_type();
  if ((rtype->node_type() == AST_Decl::NT_pre_defined) &&
      (AST_PredefinedType::narrow_from_decl(rtype)->pt()
          == AST_PredefinedType::PT_void))
    return true;
  else
    return false;
}

IMPL_NARROW_METHODS1(adabe_operation, AST_Operation)
IMPL_NARROW_FROM_DECL(adabe_operation)
IMPL_NARROW_FROM_SCOPE(adabe_operation)












