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
	  //	if (d->node_type() == AST_Decl::NT_Argument)
	  //	  {
	  d->produce_ads(with, &String, &previousdefinition);
	  //        }
	  i.next();
	}
      String += ") return ";
      String += return_type()->dump_name(with, &String, &previousdefinition) + ";\n";
    }
  else
    {
      String += " procedure" + get_ada_name() + "(Self : in Ref ";
       UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  //	if (d->node_type() == AST_Decl::NT_Argument)
	  //	  {
	  d->produce_ads(with, &String, &previousdefinition);
	  //        }
	  i.next();
	}
      String += ", Result : out ";
      String += return_type()->dump_name(with, &String, &previousdefinition) + ");\n";
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
	  //	if (d->node_type() == AST_Decl::NT_Argument)
	  //	  {
	  d->produce_ads(with, &String, &previousdefinition);
	  //        }
	  i.next();
	}
      String += ") return ";
      string name += return_type()->dump_name(with, &String, &previousdefinition);
      String += name + "is \n";
      AST_Decl  *i = defined_in();
      name_of_the_package = i->get_ada_name();
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
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += i->get_ada_name();
	  i.next();
	  if (!i.is_done()) String += ", ";
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
	  //	if (d->node_type() == AST_Decl::NT_Argument)
	  //	  {
	  d->produce_ads(with, &String, &previousdefinition);
	  //        }
	  i.next();
	}
      if (return_type() != NULL) {
	string name =  return_type()->dump_name(with, &String, &previousdefinition);
	String += ", Result : out " + name + ") is\n";
      }
      else   String += ") is \n";
      AST_Decl  *i = defined_in();
      name_of_the_package = i->get_ada_name();
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
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += i->get_ada_name();
	  i.next();
	  if (!i.is_done()) String += ", ";
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
	  //	if (d->node_type() == AST_Decl::NT_Argument)
	  //	  {
	  d->produce_impl_ads(with, &String, &previousdefinition);
	  //        }
	  i.next();
	}
      String += ") return ";
      String += return_type()->dump_name(with, &String, &previousdefinition) + ";\n";
  }
  else
    {
      String += " procedure" + get_ada_name() + "(Self : access Object ";
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while (!i.is_done())
	{
	  String += ",";
	  AST_Decl *d = i.item();
	  //	if (d->node_type() == AST_Decl::NT_Argument)
	  //	  {
	  d->produce_impl_ads(with, &String, &previousdefinition);
	  //        }
	  i.next();
	}
      String += ", Result : out ";
      String += return_type()->dump_name(with, &String, &previousdefinition) + ");\n";
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
	  //	if (d->node_type() == AST_Decl::NT_Argument)
	  //	  {
	  d->produce_impl_ads(with, &String, &previousdefinition);
	  //        }
	  i.next();
	}
      String += ") return ";
      String += return_type()->dump_name(with, &String, &previousdefinition) + ";\n";
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
	  //	if (d->node_type() == AST_Decl::NT_Argument)
	  //	  {
	  d->produce_impl_ads(with, &String, &previousdefinition);
	  //        }
	  i.next();
	}
      if (return_type != NULL) {
           string name =  return_type()->dump_name(with, &String, &previousdefinition);
	   String += ", Result : out " + name + ") is\n";
      }
      else   String += ") is \n";
      INDENT();
      String += "begin \n\n";
      INDENT();
      String += "end;";
      
    }

*/
}

void
adabe_operation::produce_proxies_ads(dep_list with,string &String, string &privatedefinition)
{
  INDENT();
  name = get_ada_full_name();
  String += "type " + name +"_Proxy is new OmniProxyCallDesc.Object with private ;\n";
  INDENT();
  String += "function Create(";


  
  //  while the UTL_Scope is not empty               //this
///////////////////// agruments du Create ////////////////////////////


    
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
      String += return_type()->.dump_name( with, &String, &previousdefinition) + "; \n"; 
    }
  privatedefinition += "type " + name + "_Proxy is new OmniProxyCallDesc.Object with record \n";



  //while the UTL_Scope is not empty               //this
///////////////////// pointeur sur les arguments ////////////////////////////

    
  INDENT();
  privatedefinition += "end record; \n ;";
}

bool  
adabe_operation::is_function()
{
  AST_Argument.Direction test = dir_IN;
  bool ret = (return_type()==NULL);
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while ((ret)&&(argument.direction()== test)&&(!(Scope->is_done())))
    {
      i.next();
    }
  return(ret);
}


IMPL_NARROW_METHODS1(adabe_operation, AST_Operation)
IMPL_NARROW_FROM_DECL(adabe_operation)
IMPL_NARROW_FROM_SCOPE(adabe_operation)












