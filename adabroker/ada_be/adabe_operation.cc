#include <idl.hh>
#include <idl_extern.hh>
#include <adabe.h>

adabe_operation::adabe_operation(AST_Type *rt, AST_Operation::Flags fl,
		 UTL_ScopedName *n,UTL_StrList *p)
                : AST_Operation(rt, fl, n, p),
		  AST_Decl(AST_Decl::NT_op, n, p),
		  UTL_Scope(AST_Decl::NT_op),
		  adabe_name(AST_Decl::NT_op,n,p)

{
}

void
adabe_operation::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  compute_ada_names();
  INDENT();
  switch (pd_flags) {
  case OP_noflags :
  case OP_idempotent :
    break;
  case OP_oneway :
    String += " oneway ";
    break;
  }
  if (is_function())
    {
      String += " function" + get_ada_name() + "(Self : in Ref ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_ads(with, String, previousdefinition);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
       	  i.next();
	}
      String += ") return "; 
      AST_Decl *b = return_type();
      String += adabe_name::narrow_from_decl(b)->dump_name(with, String, previousdefinition) + ";\n";
    }
  else
    {
      String += " procedure" + get_ada_name() + "(Self : in Ref ";
       UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_ads(with, String, previousdefinition);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      String += ", Result : out ";
      AST_Decl *b = return_type();
      String += adabe_name::narrow_from_decl(b)->dump_name(with, String, previousdefinition) + ");\n";
}

void
adabe_operation::produce_adb(dep_list with,string &String, string &previousdefinition)
{
  INDENT();
  switch (pd_flags) {
  case OP_noflags :
  case OP_idempotent :
    break;
  case OP_oneway :
    String += " oneway ";
    break;
  }
  if (is_function())
    {
      String += " function" + get_ada_name() + "(Self : in Ref ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_ads(with, String, previousdefinition);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      String += ") return ";
      AST_Decl *b = return_type();
      string name += adabe_name::narrow_from_decl(b)->dump_name(with, String, previousdefinition);
      String += name + "is \n";
      AST_Decl  *c = defined_in();
      name_of_the_package = c->get_ada_name();
      INDENT(String);
      String += "Opcd : " + name_of_the_package + ".Proxies." + get_ada_name() + "_Proxy ;\n";
      INDENT(String);
      String += "Result : " + name +";\n";
      INDENT(String);
      String += "begin \n";
      INC_INDENT();
      INDENT(String);
      String += "Assert_Ref_Not_Nil(Self);";
      INDENT(String);
      String += "Opcd := " + name_of_the_package + ".Proxies.Create(";
      UTL_ScopeActiveIterator j(this,UTL_Scope::IK_decls);
      while (!j.is_done())
	{
	  AST_Decl *e = j.item();
	  if (e->node_type() == AST_Decl::NT_Argument) String += e->get_ada_name();
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  j.next();
	  if (!j.is_done()) String += ", ";
	}
	  
      String += ") ;\n";
      INDENT(String);
      String += "OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
      INDENT(String);
      String += "Result := " + name_of_the_package + ".Proxies.Get_Result(Opcd) ;\n";
      INDENT(String);
      String += name_of_the_package + ".Proxies.Free(Opcd) ;\n";
      INDENT(String);
      String += "return Result ;";
      DEC_INDENT();
      INDENT(String);
      String += "end;";
    }
  else
    {
      String += " procedure" + ada.name + "(Self : in Ref ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_ads(with, String, previousdefinition);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      if (return_type() != NULL) {
	AST_Decl *b = return_type();
	string name =  adabe_name::narrow_from_decl(b)->dump_name(with, String, previousdefinition);
	String += ", Result : out " + name + ") is\n";
      }
      else   String += ") is \n";
      AST_Decl  *b = defined_in();
      name_of_the_package = b->get_ada_name();
      INDENT(String);
      String += "Opcd : " + name_of_the_package + ".Proxies." + get_ada_name() + "_Proxy ;\n";
      INDENT(String);
      String += "Result : " + name +";\n";
      INDENT(String);
      String += "begin \n";
      INC_INDENT();
      INDENT(String);
      String += "Assert_Ref_Not_Nil(Self);";
      INDENT(String);
      String += "Opcd := " + name_of_the_package + ".Proxies.Create(";
      UTL_ScopeActiveIterator j(this,UTL_Scope::IK_decls);
      while (!j.is_done())
	{
	  AST_Decl *e = j.item();
	  if (e->node_type() == AST_Decl::NT_Argument) String += e->get_ada_name();
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  j.next();
	  if (!j.is_done()) String += ", ";
	}
      if (return_type() !=  NULL) String += ", Result) ;\n";
      String += ") ;\n";
      INDENT(String);
      String += "OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
      INDENT(String);
      String += name_of_the_package + ".Proxies.Free(Opcd) ;\n";
      INDENT(String);
      String += "return ;";
      DEC_INDENT();
      INDENT(String);
      String += "end;";
    }
}

void
adabe_operation::produce_impl_ads(dep_list with,string &String, string &previousdefinition)
{
  INDENT();
  switch (pd_flags) {
  case OP_noflags :
  case OP_idempotent :
    break;
  case OP_oneway :
    String += " oneway ";
    break;
  }
  if (is_function())
    {
      String += " function" + get_ada_name() + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_impl_ads(with, String, previousdefinition);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      String += ") return ";
      AST_Decl *b = return_type();
      String += adabe_name::narrow_from_decl(b)->dump_name(with, String, previousdefinition) + ";\n";
    }
  else
    {
      String += " procedure" + get_ada_name() + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_impl_ads(with, String, previousdefinition);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      String += ", Result : out ";
      AST_Decl *b = return_type();
      String += adabe_name::narrow_from_decl(b)->dump_name(with, String, previousdefinition) + ");\n";
    }
}

void
adabe_operation::produce_impl_adb(dep_list with,string &String, string &previousdefinition)
{
  INDENT();
  switch (pd_flags) {
  case OP_noflags :
  case OP_idempotent :
    break;
  case OP_oneway :
    String += " oneway ";
    break;
  }
  if (is_function())
    {
      String += " function" + get_ada_name() + "(Self : access Object ";
      while the UTL_Scope is not empty (make a copy)              
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_impl_ads(with, String, previousdefinition);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      String += ") return ";
      AST_Decl *b = return_type();
      String += adabe_name::narrow_from_decl(b)->dump_name(with, String, previousdefinition) + ";\n";
      INDENT();
      String += "begin \n\n";
      INDENT();
      String += "end;";
    }
  else
    {
      String += " procedure" + ada.name + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  if (d->node_type() == AST_Decl::NT_Argument)
	    adabe_name::narrow_from_decl(d)->produce_impl_ads(with, String, previousdefinition);
	  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
	  i.next();
	}
      if (return_type != NULL) {
	   AST_Decl *b = return_type();
           string name =  adabe_name::narrow_from_decl(b)->dump_name(with, String, previousdefinition);
	   String += ", Result : out " + name + ") is\n";
      }
      else   String += ") is \n";
      INDENT();
      String += "begin \n\n";
      INDENT();
      String += "end;";      
    }
}

void
adabe_operation::produce_proxies_ads(dep_list with,string &String, string &privatedefinition)
{
  INDENT();
  name = get_ada_full_name();
  String += "type " + name +"_Proxy is new OmniProxyCallDesc.Object with private ;\n";
  INDENT();
  String += "function Create(";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_Argument)
	adabe_name::narrow_from_decl(d)->produce_proxies_ads(with, String, "IN"); //add the ","
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      i.next();
    }
  String = substr(String,0,String.length()-3); //to remove the last ", "
  String += ") return " + name +"_Proxy ;\n";
  INDENT();
  String += " procedure Free(Self : in out " + name + "_Proxy);\n";
  INDENT();
  String += " function Aligned_Size(Self : in " + name + "_Proxy ; Size_In : in Corba.Unsigned_Long)";
  INDENT();
  String += " return Corba.Unsigned_Long ;\n";
  INDENT();
  String += " procedure Marshal_Arguments(Self : in " + name + "_Proxy ; Giop_Client : in out Giop_C.Object);\n";
  INDENT();
  String += " procedure Unmarshal_Returned_Values(Self : in out " + name + "_Proxy ; Giop_Client : in Giop_C.Object);\n";
  INDENT();
  if (is_function())
    {
      String += " function Get_Result (Self : in " + name + "_Proxy ) return ";
      INDENT();
      AST_Decl *b = return_type();
      String += adabe_name::narrow_from_decl(b)->dump_name( with, String, previousdefinition) + "; \n"; 
    }
  privatedefinition += "type " + name + "_Proxy is new OmniProxyCallDesc.Object with record \n";
  INC_INDENT();
  UTL_ScopeActiveIterator j(this,UTL_Scope::IK_decls);
  while (!j.is_done())
    {
      AST_Decl *e = j.item();
      if (e->node_type() == AST_Decl::NT_Argument)
	adabe_name::narrow_from_decl(e)->produce_proxies_adb(with, privatedefinition, previousdefinition);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in operation");
      j.next();
    }
  if (is_function())
    {
      INDENT(privatedefinition);
      privatedefinition += "Private_Result : ";
      AST_Decl *c = return_type();
      String += adabe_name::narrow_from_decl(c)->dump_name( with, String, previousdefinition);
      String += "_Ptr := null;\n";
    }
  DEC_INDENT();
  INDENT();
  privatedefinition += "end record; \n ;";
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












