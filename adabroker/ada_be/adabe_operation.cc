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
      body += "   oneway ";
      break;
    }
  if (is_function())
    {
      body += "   function " + get_ada_local_name() + "(Self : in Ref";
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
      body += dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + ";\n";
    }
  else
    {
      body += "   procedure" + get_ada_local_name() + "(Self : in Ref ";
       UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ", Result : out ";
      AST_Decl *b = return_type();
      body += dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + ");\n";
    }
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
    body += "   oneway ";
    break;
    }
  if (is_function())
    {
      body += " function" + get_ada_local_name() + "(Self : in Ref ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ") return ";
      AST_Decl *b = return_type();
      string name = dynamic_cast<adabe_name *>(b)->dump_name(with, previous);
      body += name + "is \n";
      adabe_name  *c = dynamic_cast<adabe_name *>(ScopeAsDecl(defined_in()));      
      string name_of_the_package = c->get_ada_local_name();
      body += "   Opcd : " + name_of_the_package + ".Proxies." + get_ada_local_name() + "_Proxy ;\n";
      body += "   Result : " + name +";\n";
      body += "   begin \n";
      body += "      Assert_Ref_Not_Nil(Self);";
      body += "      Opcd := " + name_of_the_package + ".Proxies.Create(";
      UTL_ScopeActiveIterator j(this,UTL_Scope::IK_decls);
      while (!j.is_done())
	{
	  adabe_name *e = dynamic_cast<adabe_name *>(j.item());
	  if (e->node_type() == AST_Decl::NT_argument) body += e->get_ada_local_name();
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  j.next();
	  if (!j.is_done()) body += "; ";
	}
	  
      body += ") ;\n";
      body += "      OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
      body += "      Result := " + name_of_the_package + ".Proxies.Get_Result(Opcd) ;\n";
      body += "      " + name_of_the_package + ".Proxies.Free(Opcd) ;\n";
      body += "      return Result ;";
      body += "   end;";
    }
  else
    {
      body += "   procedure get_" + get_ada_local_name() + "(Self : in Ref ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	    dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      string name =   dynamic_cast<adabe_name *>(return_type())->dump_name(with, previous);
      if (!return_is_void())
	{	
	  body += ", Result : out " + name + ") is\n";
	}
      else   body += ") is \n";
      adabe_name  *b =  dynamic_cast<adabe_name *>(ScopeAsDecl(defined_in()));
      string name_of_the_package = b->get_ada_local_name();
      body += "   Opcd : " + name_of_the_package + ".Proxies." + get_ada_local_name() + "_Proxy ;\n";
      body += "   Result : " + name + ";\n";
      body += "   begin \n";
      body += "      Assert_Ref_Not_Nil(Self);";
      body += "      Opcd := " + name_of_the_package + ".Proxies.Create(";
      UTL_ScopeActiveIterator j(this,UTL_Scope::IK_decls);
      while (!j.is_done())
	{
	  adabe_name *e =  dynamic_cast<adabe_name *>(j.item());
	  if (e->node_type() == AST_Decl::NT_argument) body += e->get_ada_local_name();
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  j.next();
	  if (!j.is_done()) body += ", ";
	}
      if (!return_is_void()) body += ", Result) ;\n";
      body += ") ;\n";
      body += "      OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
      body += "      " + name_of_the_package + ".Proxies.Free(Opcd) ;\n";
      body += "      return ;";
      body += "   end;";
    }
}

void
adabe_operation::produce_impl_ads(dep_list& with,string &body, string &previous)
{
  switch (flags()) {
  case OP_noflags :
  case OP_idempotent :
    break;
  case OP_oneway :
    body += "   oneway ";
    break;
  }
  if (is_function())
    {
      body += "   function" + get_ada_local_name() + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	     dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ") return ";
      AST_Decl *b = return_type();
      body +=  dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + ";\n";
    }
  else
    {
      body += " procedure" + get_ada_local_name() + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	     dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ", Result : out ";
      AST_Decl *b = return_type();
      body +=  dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + ");\n";
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
    body += "   oneway ";
    break;
  }
  if (is_function())
    {
      body += "   function" + get_ada_local_name() + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	     dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ") return ";
      AST_Decl *b = return_type();
      body +=  dynamic_cast<adabe_name *>(b)->dump_name(with, previous) + ";\n";
      body += "   begin \n\n";
      body += "   end;";
    }
  else
    {
      body += "    procedure get_" + get_ada_local_name() + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_argument)
	     dynamic_cast<adabe_name *>(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      if (!return_is_void()) {
	   AST_Decl *b = return_type();
           string name =   dynamic_cast<adabe_name *>(b)->dump_name(with, previous);
	   body += ", Result : out " + name + ") is\n";
      }
      else   body += ") is \n";
      body += "   begin \n\n";
      body += "   end;\n";      
    }
}

void
adabe_operation::produce_proxies_ads(dep_list& with,string &body, string &private_definition)
{
  string name = get_ada_full_name();
  string in_decls = "";
  string fields = "";
  bool no_in = true;
  bool no_out = false;
  // first process each argument
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	 dynamic_cast<adabe_argument *>(d)->produce_proxies_ads(with, in_decls, no_in, no_out, fields);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  in_decls += ") ;\n\n";

  // get the type of the result
  string previous = "";
  AST_Decl *b = return_type();
  string result_name =  dynamic_cast<adabe_name *>(b)->dump_name(with, previous); 

  // produce functions
  body += "   type " + name + "_Proxy is new OmniProxyCallDesc.Object with private ;\n\n";
  body += "   procedure Init(Self : in out " + get_ada_local_name();
  body += in_decls;  
  body += "   function Operation(Self : in " + get_ada_local_name() + ")\n";
  body += "                      return CORBA.String ;\n\n" ;
  if (!no_in) {
      body += "   function Aligned_Size(Self : in " + get_ada_local_name() + " ;\n";
      body += "                         Size_In : in Corba.Unsigned_Long)\n";
      body += "                         return Corba.Unsigned_Long ;\n\n";
      body += "   procedure Marshal_Arguments(Self : in " + get_ada_local_name() + " ;\n";
      body += "                               Giop_Client : in out Giop_C.Object) ;\n\n";
  }
  if ((!no_out) || (is_function())) {
  body += "   procedure Unmarshal_Returned_Values(Self : in out " + get_ada_local_name() + " ;\n";
  body += "                                       Giop_Client : in Giop_C.Object) ;\n\n";
  }
  if (is_function()) {
  body += "   function Get_Result (Self : in " + get_ada_local_name() + ")\n";
  body += "                        return " +  result_name + "; \n\n\n";
  }

  private_definition += "   type " + name + " is new OmniProxyCallDesc.Object with record \n";
  private_definition += fields;
  if (is_function())
    {
      private_definition += "      Private_Result : ";
      private_definition += result_name + "_Ptr := null;\n";
    }
  private_definition += "   end record; \n";
  private_definition += "   procedure Finalize(Self : in out " + name + "_Proxy) ;\n\n";
}

void
adabe_operation::produce_proxies_adb(dep_list& with,string &body, string &private_definition)
{
  string name = get_ada_full_name();
  string in_decls = "";
  string init = "";
  string align_size = "";
  string marshall = "";
  string unmarshall_decls = "";
  string unmarshall = "";
  string finalize = "";
  bool no_in = true;
  bool no_out = false;
  // First process each argument
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_argument)
	 dynamic_cast<adabe_argument *>(d)->produce_proxies_adb(with, in_decls, no_in, no_out, init, align_size, marshall, unmarshall_decls, unmarshall, finalize);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  in_decls += ") is\n";

  // get the type of the result
  AST_Decl *b = return_type();
  string result_name =  dynamic_cast<adabe_name *>(b)->dump_name(with, private_definition); 

  // produce functions
  body += "   procedure Init(Self : in out " + get_ada_local_name();
  body += in_decls;
  body += "      begin\n";
  body += "      Set_User_Exceptions(Self, False) ;\n";
  body += init;
  body += "   end ;\n\n\n";
  
  body += "   function Operation(Self : in " + get_ada_local_name() + ")\n";
  body += "                      return CORBA.String is\n";
  body += "   begin\n";
  body += "      return Corba.To_Corba_String(\"" + get_ada_local_name() + "\") ;\n";
  body += "   end ;\n\n\n";

  if (!no_in) {
      body += "   function Aligned_Size(Self : in " + get_ada_local_name() + " ;\n";
      body += "                         Size_In : in Corba.Unsigned_Long)\n";
      body += "                         return Corba.Unsigned_Long is\n";
      body += "      Tmp : Corba.Unsigned_Long := Size_In ;\n";
      body += "   begin\n";
      body += align_size;
      body += "      return Tmp ;\n";
      body += "   end ;\n\n\n";

      body += "   procedure Marshal_Arguments(Self : in " + get_ada_local_name() + " ;\n";
      body += "                               Giop_Client : in out Giop_C.Object) is\n";
      body += "   begin\n";
      body += marshall;
      body += "   end ;\n\n\n";      
  }
  if ((!no_out) || (is_function())) {
  body += "   procedure Unmarshal_Returned_Values(Self : in out " + get_ada_local_name() + " ;\n";
  body += "                                       Giop_Client : in Giop_C.Object) is\n";
  body += unmarshall_decls;
  if (is_function()) {
    body += "      Result : " + name + " ;\n";
  }
  body += "   begin\n";
  body += unmarshall;
  if (is_function()) {
    body += "      Unmarshall(Result, Giop_client) ;\n";
    body += "      Self.Result := new " + name + "'(Result) ;\n";
  }
  body += "   end ;\n\n\n";      
  }
  
  if (is_function()) {
  body += "   function Get_Result (Self : in " + get_ada_local_name() + ")\n";
  body += "                        return " +  result_name + " is\n";
  body += "   begin\n";
  body += "      return Self.Result.all ;\n";
  body += "   end ;\n\n\n";
  }

  body += "   procedure Finalize(Self : in out " + name + ") is\n";
  body += "   begin\n";
  body += finalize;
  if (is_function()) {
    body += "      Free(Self.Result) ;\n";
  } else if (no_in && no_out) {
    body += "      null ;\n";
  }
  body += "   end ;\n\n\n";
}

void
adabe_operation::produce_skeleton_adb(dep_list& with,string &body, string &private_definition)
{
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


IMPL_NARROW_METHODS1(adabe_operation, AST_Operation)
IMPL_NARROW_FROM_DECL(adabe_operation)
IMPL_NARROW_FROM_SCOPE(adabe_operation)












