#include <adabe.h>

adabe_operation::adabe_operation(AST_Type *rt, AST_Operation::Flags fl,
		 UTL_ScopedName *n,UTL_StrList *p)
                : AST_Operation(rt, fl, n, p),
		  AST_Decl(AST_Decl::NT_op, n, p),
		  UTL_Scope(AST_Decl::NT_op),
		  adabe_name()

{
}

void
adabe_operation::produce_ads(dep_list with,string &body, string &previous)
{
  compute_ada_names();
  switch (pd_flags) {
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
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
       	  i.next();
	}
      body += ") return "; 
      AST_Decl *b = return_type();
      body += adabe_name::narrow_from_decl(b)->dump_name(with, body, previous) + ";\n";
    }
  else
    {
      body += " procedure" + get_ada_local_name() + "(Self : in Ref ";
       UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ", Result : out ";
      AST_Decl *b = return_type();
      body += adabe_name::narrow_from_decl(b)->dump_name(with, body, previous) + ");\n";
}

void
adabe_operation::produce_adb(dep_list with,string &body, string &previous)
{
  switch (pd_flags) {
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
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ") return ";
      AST_Decl *b = return_type();
      string name += adabe_name::narrow_from_decl(b)->dump_name(with, body, previous);
      body += name + "is \n";
      AST_Decl  *c = defined_in();
      name_of_the_package = c->get_ada_local_name();
      body += "   Opcd : " + name_of_the_package + ".Proxies." + get_ada_local_name() + "_Proxy ;\n";
      body += "   Result : " + name +";\n";
      body += "   begin \n";
      body += "      Assert_Ref_Not_Nil(Self);";
      body += "      Opcd := " + name_of_the_package + ".Proxies.Create(";
      UTL_ScopeActiveIterator j(this,UTL_Scope::IK_decls);
      while (!j.is_done())
	{
	  AST_Decl *e = j.item();
	  if (e->node_type() == AST_Decl::NT_Argument) body += e->get_ada_local_name();
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  j.next();
	  if (!j.is_done()) body += ", ";
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
      body += "   procedure" + ada.name + "(Self : in Ref ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      if (return_type() != NULL) {
	AST_Decl *b = return_type();
	string name =  adabe_name::narrow_from_decl(b)->dump_name(with, body, previous);
	body += ", Result : out " + name + ") is\n";
      }
      else   body += ") is \n";
      AST_Decl  *b = defined_in();
      name_of_the_package = b->get_ada_local_name();
      body += "   Opcd : " + name_of_the_package + ".Proxies." + get_ada_local_name() + "_Proxy ;\n";
      body += "   Result : " + name +";\n";
      body += "   begin \n";
      body += "      Assert_Ref_Not_Nil(Self);";
      body += "      Opcd := " + name_of_the_package + ".Proxies.Create(";
      UTL_ScopeActiveIterator j(this,UTL_Scope::IK_decls);
      while (!j.is_done())
	{
	  AST_Decl *e = j.item();
	  if (e->node_type() == AST_Decl::NT_Argument) body += e->get_ada_local_name();
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  j.next();
	  if (!j.is_done()) body += ", ";
	}
      if (return_type() !=  NULL) body += ", Result) ;\n";
      body += ") ;\n";
      body += "      OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
      body += "      " + name_of_the_package + ".Proxies.Free(Opcd) ;\n";
      body += "      return ;";
      body += "   end;";
    }
}

void
adabe_operation::produce_impl_ads(dep_list with,string &body, string &previous)
{
  switch (pd_flags) {
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
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_impl_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ") return ";
      AST_Decl *b = return_type();
      body += adabe_name::narrow_from_decl(b)->dump_name(with, body, previous) + ";\n";
    }
  else
    {
      body += " procedure" + get_ada_local_name() + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_impl_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ", Result : out ";
      AST_Decl *b = return_type();
      body += adabe_name::narrow_from_decl(b)->dump_name(with, body, previous) + ");\n";
    }
}

void
adabe_operation::produce_impl_adb(dep_list with,string &body, string &previous)
{
  switch (pd_flags) {
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
      while the UTL_Scope is not empty (make a copy)              
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_impl_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      body += ") return ";
      AST_Decl *b = return_type();
      body += adabe_name::narrow_from_decl(b)->dump_name(with, body, previous) + ";\n";
      body += "   begin \n\n";
      body += "   end;";
    }
  else
    {
      body += "    procedure" + ada.name + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  body += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_impl_ads(with, body, previous);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      if (return_type != NULL) {
	   AST_Decl *b = return_type();
           string name =  adabe_name::narrow_from_decl(b)->dump_name(with, body, previous);
	   body += ", Result : out " + name + ") is\n";
      }
      else   body += ") is \n";
      body += "   begin \n\n";
      body += "   end;\n";      
    }
}

void
adabe_operation::produce_proxies_ads(dep_list with,string &body, string &private_definition)
{
  name = get_ada_full_name();
  body += "   type " + name +"_Proxy is new OmniProxyCallDesc.Object with private ;\n";
  body += "   function Create(";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_Argument)
	adabe_name::narrow_from_decl(d)->produce_proxies_ads(with, body, "IN"); //add the ","
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  body = substr(body,0,body.length()-3); //to remove the last ", "
  body += ") return " + name +"_Proxy ;\n";
  body += "   procedure Free(Self : in out " + name + "_Proxy);\n";
  body += "   function Aligned_Size(Self : in " + name + "_Proxy ; Size_In : in Corba.Unsigned_Long)";
  body += " return Corba.Unsigned_Long ;\n";
  body += "   procedure Marshal_Arguments(Self : in " + name + "_Proxy ; Giop_Client : in out Giop_C.Object);\n";
  body += "   procedure Unmarshal_Returned_Values(Self : in out " + name + "_Proxy ; Giop_Client : in Giop_C.Object);\n";
  if (is_function())
    {
      body += "   function Get_Result (Self : in " + name + "_Proxy ) return ";
      AST_Decl *b = return_type();
      body += adabe_name::narrow_from_decl(b)->dump_name(with, body, previous) + "; \n"; 
    }
  private_definition += "   type " + name + "_Proxy is new OmniProxyCallDesc.Object with record \n";
  UTL_ScopeActiveIterator j(this,UTL_Scope::IK_decls);
  while (!j.is_done())
    {
      AST_Decl *e = j.item();
      if (e->node_type() == AST_Decl::NT_Argument)
	adabe_name::narrow_from_decl(e)->produce_proxies_adb(with, private_definition, previous);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      j.next();
    }
  if (is_function())
    {
      private_definition += "      Private_Result : ";
      AST_Decl *c = return_type();
      body += adabe_name::narrow_from_decl(c)->dump_name(with, body, previous);
      body += "_Ptr := null;\n";
    }
  private_definition += "   end record; \n ;";
}

void
adabe_operation::produce_proxies_adb(dep_list with,string &body, string &private_definition)
{
}

void
adabe_operation::produce_skeleton_adb(dep_list with,string &body, string &private_definition)
{
}

bool  
adabe_operation::is_function()
{
  AST_Argument.Direction test = dir_IN;
  bool ret = (return_type() != NULL);
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while ((ret)&&(argument.direction()== test)&&(!i.is_done()))
    {
      i.next();
    }
  return(ret);
}


IMPL_NARROW_METHODS1(adabe_operation, AST_Operation)
IMPL_NARROW_FROM_DECL(adabe_operation)
IMPL_NARROW_FROM_SCOPE(adabe_operation)












