/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_operation.cc                                      ***
***                                                                                            ***
***      This file provides the implementation of class adabe_operation declared in adabe.h    ***
***   (L 466). This class is the correspondant of the Sun's Front-End class AST_Operation.     ***
***   It provides produce functions for each generated file, a constructor and two little      ***
***   functions : is_function() and return_is_void().                                          ***
***                                                                                            ***
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

adabe_operation::adabe_operation(AST_Type *rt, AST_Operation::Flags fl,
		 UTL_ScopedName *n,UTL_StrList *p)
                : AST_Operation(rt, fl, n, p),
		  AST_Decl(AST_Decl::NT_op, n, p),
		  UTL_Scope(AST_Decl::NT_op),
		  adabe_name(AST_Decl::NT_op, n, p)

{
}

void
adabe_operation::produce_ads(dep_list& with,string &body, string &previous)
{
  compute_ada_name();
  switch (flags())
    {
    case OP_noflags :
    case OP_idempotent :
      break;
    case OP_oneway :
      body += "   ---   oneway   ---\n";
      break;
    }
  if (is_function())
    {
      string space = "";
      for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";
      body += "   function " + get_ada_local_name() + "(Self : in Ref";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += " ;\n";
	  body += "             " + space ;
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
       	  i.next();
	}
      body += ")\n";
      body += "             " + space + "return "; 
      AST_Decl *b = return_type();
      body += dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + " ;\n\n\n";
    }
  else
    {
      body += "   procedure " + get_ada_local_name() + "(Self : in Ref";
       UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      // if it was an IDL function, add the Result parameter
      if (!return_is_void()) {
	body += "; Result : out ";
	AST_Decl *b = return_type();
	body += dynamic_cast<adabe_name *>(b)->dump_name(with, previous) ;
	}
      body += " ) ;\n\n\n";
    }
  set_already_defined ();
}

void
adabe_operation::produce_adb(dep_list& with,string &body, string &previous)
{
  switch (flags())
    {
    case OP_noflags :
    case OP_idempotent :
      break;
    case OP_oneway :
    body += "   ---   oneway   ---\n";
    break;
    }

  string space = "";
  for (unsigned int i=0;i<get_ada_local_name().length();i++) space += " ";

  string in_decls = "";
  string in_args = "";
  string out_args = "";
  bool no_out = true;
  // first parse the arguments 
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	dynamic_cast<adabe_argument *>(d)->produce_adb(with, no_out, space,
						       in_decls, in_args, out_args);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  if ((!return_is_void()) && (!is_function())) 
    {
      out_args += ", Result";
      AST_Decl *b = return_type();
      string tmp = dynamic_cast<adabe_name *>(b)->dump_name(with, previous) ;
      in_decls += ";\n              " + space + "Result : out " +  tmp;
    }
  in_decls += ")";
  in_args += ") ;\n";
  out_args += ") ;\n";
  
  if (is_function())
    body += "   function ";
  else
    body += "   procedure ";
  
  body += get_ada_local_name() + "(Self : in Ref";
  body += in_decls;

  if (is_function())
    {
      body += "\n             " + space + "return ";
      AST_Decl *b = return_type();
      string name = (dynamic_cast<adabe_name *>(b))->dump_name(with, previous);
      body += name;
    }
    
  body += " is\n";
  
  adabe_name  *c = dynamic_cast<adabe_name *>(ScopeAsDecl(defined_in()));      
  string name_of_the_package = c->get_ada_local_name();
  body += "      Opcd : " + name_of_the_package + ".Proxies." + get_ada_local_name() + "_Proxy ;\n";
  body += "   begin \n";
  body += "      " + name_of_the_package + ".Proxies.Init(Opcd";
  body += in_args;
	  
  with.add("OmniProxyCallWrapper");
  body += "      OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
  if (is_function())
    body += "      return " + name_of_the_package + ".Proxies.Get_Result(Opcd) ;\n";
  else if (!no_out)
    body += "      " + name_of_the_package + ".Proxies.Get_Result(Opcd" + out_args;
  
  body += "   end ;\n\n";
}

void
adabe_operation::produce_impl_ads(dep_list& with,string &body, string &previous)
{
  switch (flags()) {
  case OP_noflags :
  case OP_idempotent :
    break;
  case OP_oneway :
    body += "   ----   oneway   ----\n";
    break;
  }
  if (is_function())
    {
      body += "   function " + get_ada_local_name() + "(Self : access Object";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	     dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ") return ";
      AST_Decl *b = return_type();
      body +=  dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + " ;\n\n";
    }
  else
    {
      body += "   procedure " + get_ada_local_name() + "(Self : access Object";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	     dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      if (!return_is_void()) {
	body += "; Result : out ";
	AST_Decl *b = return_type();
	body +=  dynamic_cast<adabe_name *>(b)->dump_name(with, previous) ;
      }
      body += " ) ;\n" ;
    }
}

void
adabe_operation::produce_impl_adb(dep_list& with,string &body, string &previous)
{
  switch (flags()) {
  case OP_noflags :
  case OP_idempotent :
    break;
  case OP_oneway :
    body += "   ---    oneway    ---\n";
    break;
  }
  if (is_function())
    {
      body += "   function " + get_ada_local_name() + "(Self : access Object";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	     dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ") return ";
      AST_Decl *b = return_type();
      body +=  dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + " is\n";
      body += "   begin \n";
      body += "   end ;\n";
    }
  else
    {
      body += "   procedure " + get_ada_local_name() + "(Self : access Object";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += "; ";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	     dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      if (!return_is_void()) {
	   AST_Decl *b = return_type();
           string name =   dynamic_cast<adabe_name *>(b)->dump_name(with, previous);
	   body += "; Result : out " + name + ") is\n";
      }
      else   body += ") is \n";
      body += "   begin \n";
      body += "   end;\n";      
    }
  body += "\n\n" ;
}

void
adabe_operation::produce_proxies_ads(dep_list& with,string &body, string &private_definition)
{
  string name = get_ada_full_name();
  string in_decls = "";
  string fields = "";
  bool no_in = true;
  bool no_out = true;
  string out_args = "";

  // See if this subprogram can raise exceptions
  UTL_ExceptlistActiveIterator except_iterator(exceptions()) ;
  bool user_exceptions = (! except_iterator.is_done()) ;

  // first process each argument
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	 dynamic_cast<adabe_argument *>(d)->produce_proxies_ads(with, in_decls, no_in, no_out, fields, out_args);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  if ((!return_is_void()) && (!is_function())) 
    {
      AST_Decl *b = return_type();
      string tmp = dynamic_cast<adabe_name *>(b)->dump_name(with, private_definition) ;
      out_args += "; Result : out " + tmp;
      fields += "      Result : ";
      fields += tmp;
      fields += "_Ptr := null ;\n";
    }
   in_decls += ") ;\n\n";

  // get the type of the result
  string previous = "";
  AST_Decl *b = return_type();
  string result_name =  dynamic_cast<adabe_name *>(b)->dump_name(with, previous); 

  // produce functions
  body += "   -----------------------------------------------------------\n" ;
  body += "   ---               " + get_ada_local_name() + "\n" ; 
  body += "   -----------------------------------------------------------\n\n" ;
  body += "   type " + get_ada_local_name() + "_Proxy is new OmniProxyCallDesc.Object with private ;\n\n";
  body += "   procedure Init(Self : in out " + get_ada_local_name() + "_Proxy";
  body += in_decls;  
  body += "   function Operation(Self : in " + get_ada_local_name() + "_Proxy )\n";
  body += "                      return Corba.String ;\n\n" ;

  if (!no_in) {
      body += "   function Align_Size(Self : in " + get_ada_local_name() + "_Proxy ;\n";
      body += "                       Size_In : in Corba.Unsigned_Long)\n";
      body += "                       return Corba.Unsigned_Long ;\n\n";
      body += "   procedure Marshal_Arguments(Self : in " + get_ada_local_name() + "_Proxy ;\n";
      body += "                               Giop_Client : in out Giop_C.Object) ;\n\n";
  }

  if ((!no_out) || (is_function())) {
  body += "   procedure Unmarshal_Returned_Values(Self : in out " ;
  body +=  get_ada_local_name() + "_Proxy ;\n";
  body += "                                       Giop_Client : in out Giop_C.Object) ;\n\n";
  }

  if (is_function()) {
    body += "   function Get_Result (Self : in " + get_ada_local_name() + "_Proxy)\n";
    body += "                        return " +  result_name + "; \n\n\n";
  } else {
    if (!no_out) {
      body += "   procedure Get_Result (Self : in " + get_ada_local_name() + "_Proxy";
      body += out_args;
      body += ") ;\n\n\n";
    }
  }

  if (user_exceptions) {
    body += "   procedure User_Exception (Self : in " + get_ada_local_name() + "_Proxy ;\n";
    body += "                             Giop_Client : in out Giop_C.Object ;\n";
    body += "                             RepoId : in CORBA.String) ;\n\n\n";
  }
  
  private_definition += "   type " + get_ada_local_name() + "_Proxy is new OmniProxyCallDesc.Object with record \n";
  if ((fields == "")&&(!is_function())) {
    private_definition += "      null ;\n" ;
  } else {
    private_definition += fields;
  }
  if (is_function())
    {
      private_definition += "      Private_Result : ";
      private_definition += result_name + "_Ptr := null;\n";
    }
  private_definition += "   end record; \n";
  if (!((fields == "")&&(!is_function()))) {
    private_definition += "   procedure Finalize(Self : in out "
      + get_ada_local_name() + "_Proxy) ;\n";
  }
  private_definition += "\n" ;
}

void
adabe_operation::produce_proxies_adb(dep_list& with,string &body,
				     string &private_definition)
{
  string name = get_ada_full_name();
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
	 dynamic_cast<adabe_argument *>(d)->produce_proxies_adb(with, in_decls, no_in, no_out, init, align_size, marshall, unmarshall_decls, unmarshall, finalize, out_args, result_decls);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  if ((!return_is_void()) && (!is_function())) 
    {
      AST_Decl *b = return_type();
      string tmp = dynamic_cast<adabe_name *>(b)->dump_name(with, private_definition) ;
      unmarshall_decls += "      Result : ";
      unmarshall_decls += tmp;
      unmarshall_decls += " ;\n";
      unmarshall += "      Unmarshall(Result ,Giop_Client) ;\n";
      unmarshall += "      Self.Result := new ";
      unmarshall += tmp;
      unmarshall += "'(Result) ;\n";
      out_args += "; Result : out " + tmp;
      result_decls += "      Result := Self.Result.all ;\n";
      finalize += "      Free(Self.Result) ;\n";
   }
  in_decls += ") is\n";

  // See if this subprogram can raise exceptions
  UTL_ExceptlistActiveIterator except_iterator(exceptions()) ;
  bool user_exceptions = (! except_iterator.is_done()) ;

  // produce functions
  body += "   -----------------------------------------------------------\n" ;
  body += "   ---               " + get_ada_local_name() + "\n" ; 
  body += "   -----------------------------------------------------------\n\n" ;
  body += "   -- Init\n" ;
  body += "   -------\n" ;
  body += "   procedure Init(Self : in out " + get_ada_local_name() + "_Proxy" ;
  body += in_decls;
  body += "   begin\n";
  body += "      Set_User_Exceptions(Self, " ;
  if (user_exceptions) {
    body += "True" ;
  } else {
    body += "False" ;
  } ;
  body += " ) ;\n";
  body += init;
  body += "   end ;\n\n\n";
  body += "   -- Operation\n" ;
  body += "   ------------\n" ;
  body += "   function Operation(Self : in " + get_ada_local_name() + "_Proxy )\n";
  body += "                      return Corba.String is\n";
  body += "   begin\n";
  body += "      return Corba.To_Corba_String(\"" + get_ada_local_name() + "\") ;\n";
  body += "   end ;\n\n\n";
  
  if (!no_in) {
    body += "   -- Align_Size\n" ;
    body += "   -------------\n" ;
    body += "   function Align_Size(Self : in " + get_ada_local_name() + "_Proxy ;\n";
    body += "                       Size_In : in Corba.Unsigned_Long)\n";
    body += "                       return Corba.Unsigned_Long is\n";
    body += "      Tmp : Corba.Unsigned_Long := Size_In ;\n";
    body += "   begin\n";
    body += align_size;
    body += "      return Tmp ;\n";
    body += "   end ;\n\n\n";
    
    body += "   -- Marshal_Arguments\n" ;
    body += "   --------------------\n" ;
    body += "   procedure Marshal_Arguments(Self : in " ;
    body += get_ada_local_name() + "_Proxy ;\n";
    body += "                               Giop_Client : in out Giop_C.Object) is\n";
    body += "   begin\n";
    body += marshall;
    body += "   end ;\n\n\n";      
  }
  if ((!no_out) || (is_function())) {
    body += "   -- Unmarshal_Returned_Values\n" ;
    body += "   ----------------------------\n" ;
    body += "   procedure Unmarshal_Returned_Values(Self : in out " ;
    body += get_ada_local_name() + "_Proxy ;\n";
    body += "                                       Giop_Client : in out Giop_C.Object) is\n";
    body += unmarshall_decls;
    if (is_function())
      {
	// get the type of the result
	AST_Decl *b = return_type();
	result_name =  dynamic_cast<adabe_name *>(b)->dump_name(with, private_definition); 
	dynamic_cast<adabe_name *>(b)->is_marshal_imported(with);
	body += "      Result : " + result_name + " ;\n";
      }
    body += "   begin\n";
    body += unmarshall;
    if (is_function()) {
      body += "      Unmarshall(Result, Giop_client) ;\n";
      body += "      Self.Private_Result := new " + result_name + "'(Result) ;\n";
    }
    body += "   end ;\n\n\n";      
  }
  
  if (is_function()) {
    body += "   -- Get_Result\n" ;
    body += "   -------------\n" ;
    body += "   function Get_Result (Self : in " + get_ada_local_name() + "_Proxy )\n";
    body += "                        return " +  result_name + " is\n";
    body += "   begin\n";
    body += "      return Self.Private_Result.all ;\n";
    body += "   end ;\n\n\n";
  } else {
    if (!no_out) {
      body += "   -- Get_Result\n" ;
      body += "   -------------\n" ;
      body += "   procedure Get_Result (Self : in " + get_ada_local_name() + "_Proxy";
      body += out_args;
      body += ") is\n";
      body += "   begin\n";
      body += result_decls;
      body += "   end ;\n\n\n";
    }
  }

  if (user_exceptions) {
    body += "   -- User_Exception\n" ;
    body += "   -----------------\n" ;
    body += "   procedure User_Exception (Self : in " + get_ada_local_name() + "_Proxy ;\n";
    body += "                             Giop_Client : in out Giop_C.Object ;\n";
    body += "                             RepoId : in CORBA.String) is\n";
    body += "   begin\n";
    
    while (!except_iterator.is_done())
      {
	string tmp = "";
	AST_Decl *d = except_iterator.item();
	dynamic_cast<adabe_exception *>(d)->produce_proxies_adb(with,tmp);
	body += tmp;
	except_iterator.next();
      }

    body += "      Ada.Exceptions.Raise_Exception(Corba.AdaBroker_Fatal_Error'Identity,\n";
    body += "                                     \"In ";
    body += get_ada_local_name();
    body += "_Proxy.User_Exception, unknown exception.\") ;\n";
    body += "   end ;\n";
  }

  if ( (!no_in) || (!no_out) || (is_function())) {
    body += "   -- Finalize\n" ;
    body += "   -----------\n" ;
    body += "   procedure Finalize(Self : in out " + get_ada_local_name() + "_Proxy) is\n";
    body += "   begin\n";
    body += finalize;
    if (is_function()) {
      body += "      Free(Self.Private_Result) ;\n";
    } 
    body += "   end ;\n\n\n";
  }
}

void
adabe_operation::produce_skel_adb(dep_list& with,string &body, string &private_definition)
{
  string full_name = get_ada_full_name();
  string pack_name = full_name.substr(0,full_name.find_last_of('.')) ;
  string result_name = "";
  string in_decls = "";
  string unmarshall = "";
  string call_args = "";
  string marshall = "";
  bool no_in = true;
  bool no_out = true;
  
  // First process each argument
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	 dynamic_cast<adabe_argument *>(d)->produce_skel_adb(with, in_decls , no_in, no_out, unmarshall, call_args, marshall);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }

  body += "      if Orl_Op = \"";
  body += get_ada_local_name ();
  body += "\" then\n";
  body += "         declare\n";
  body += in_decls;
  if (is_function())
    {
      // get the type of the result
      AST_Decl *b = return_type();
      adabe_name *e = dynamic_cast<adabe_name *>(b);
      result_name = e->dump_name(with, private_definition);
      e->is_marshal_imported(with);
      body += "            Result : ";
      body += result_name;
      body += " ;\n";
    }

  if ((!no_out) || (is_function()))
    body += "            Mesg_Size : Corba.Unsigned_Long ;\n";

  body += "         begin\n";

  if (!no_in) {
    body += "            -- unmarshalls arguments\n";
    body += unmarshall;
  }

  body += "            -- change state\n";
  body += "            Giop_S.Request_Received(Orls) ;\n";

  body += "            -- call the implementation\n";
  body += "            ";
  if (is_function()) body += "Result := ";
  body += pack_name;
  body += ".Impl.";
  body += get_ada_local_name ();
  body += "(Self";
  body += call_args;
  body += ") ;\n";

  if ((!no_out) || (is_function())) {
    body += "            -- compute the size of the replied message\n";
    body += "            Mesg_Size := Giop_S.Reply_Header_Size ;\n";
    body += "            Mesg_Size := Align_Size (Result, Mesg_Size) ;\n";

    body += "            -- Initialisation of the reply\n";
    body += "            Giop_S.Initialize_Reply (Orls, Giop.NO_EXCEPTION, Mesg_Size) ;\n";

    body += "            -- Marshall the arguments\n";

    body += marshall;
    if (is_function()) body += "            Marshall (Result, Orls) ;\n";
  }

  body += "            -- inform the orb\n";
  body += "            Giop_S.Reply_Completed (Orls) ;\n";

  body += "            Returns := True ;\n";
  body += "            return ;\n";

  UTL_ExceptlistActiveIterator except_iterator(exceptions()) ;

  bool user_exceptions = (! except_iterator.is_done()) ;
#ifdef DEBUG_OPERATION
  cerr << "begin of exception generation" << endl;
  if (user_exceptions)
    cerr << "     there is some exceptions" << endl;
  else
    cerr << "     actually, there is no exceptions" << endl;
#endif

  if (user_exceptions) {
    body += "\n         exception\n";
    while (!except_iterator.is_done())
      {
	string tmp = "";
	AST_Decl *d = except_iterator.item();
	dynamic_cast<adabe_exception *>(d)->produce_skel_adb(with, tmp);
	body += tmp;
	except_iterator.next();
      }
  }
    
  body += "         end ;\n";
  body += "      end if ;\n\n";
}

bool  
adabe_operation::is_function()
{
  AST_Argument::Direction test = AST_Argument::dir_IN;
  bool ret = !(return_is_void());
#ifdef DEBUG_OPERATION
  bool test2 = true;
  cerr << "ret value for this method is " << ret << endl;
  cerr << "true is " << test2 << endl;
#endif
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while ((!i.is_done()) && (ret))
    {
      AST_Decl *e = i.item();
      if (e->node_type() == AST_Decl::NT_argument) ret = (((AST_Argument::narrow_from_decl(e))->direction()) == test);
      i.next();
    }
  return(ret);
}

bool
adabe_operation::return_is_void()
{
  AST_Decl *rtype = return_type();
  if ((rtype->node_type() == AST_Decl::NT_pre_defined) &&
      (AST_PredefinedType::narrow_from_decl(rtype)->pt()
          == AST_PredefinedType::PT_void))
    return true;
  else
    return false;
}

void
adabe_operation::produce_marshal_adb(dep_list &with, string &body, string &previous)
{
  AST_Decl *res = return_type();
  adabe_name *result = dynamic_cast<adabe_name *>(res); 
  if (!result->is_marshal_imported(with))
    {
      if (!result->is_already_defined())
	{
	  string tmp = "";
	  result->produce_marshal_adb(with, tmp, previous);
	  previous += tmp;
	}
    }

  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument) {
	adabe_name *dd = dynamic_cast<adabe_argument *>(d);
	if (!dd->is_marshal_imported(with))
	  {
	    if (!dd->is_already_defined())
	      {
		string tmp = "";
		dd->produce_marshal_adb(with, tmp, previous);
		previous += tmp;
	      }
	  }	
      }
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
}


IMPL_NARROW_METHODS1(adabe_operation, AST_Operation)
IMPL_NARROW_FROM_DECL(adabe_operation)
IMPL_NARROW_FROM_SCOPE(adabe_operation)












