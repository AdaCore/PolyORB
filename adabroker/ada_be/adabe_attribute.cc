// adabe_attribute 
#include <idl.hh>
#include <idl_extern.hh>
#include <adabe.h>


adabe_attribute::adabe_attribute(idl_bool ro, AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p)
  : AST_Attribute(ro,ft,n,p),
    AST_Field(AST_Decl::NT_attr,ft,n,p),
    AST_Decl(AST_Decl::NT_attr,n,p),
    adabe_name(AST_Decl::NT_attr,n,p)
{  
}

void
adabe_attribute::produce_ads(dep_list with,string &String, string &previousdefinition)
{
  compute_ada_names();
  INDENT(String);
  String += "function get_" + get_ada_name() +"(Self : in Ref) return "; 
  AST_Decl *d = field_type();
  string name = adabe_name::narrow_from_decl(d)->dump_name(with, String, previousdefinition) + ";\n";
  String += name;
  if (!pd_readonly)
    {
      INDENT(String);
      String += "procedure set_" + get_ada_name();
      String += "(Self : in Ref, To : in ";
      String += name;
      String += ");";
    }
}

void
adabe_operation::produce_adb(dep_list with,string &String, string &previousdefinition)
{
  INDENT(String);
  String += "function get_" + get_ada_name() +"(Self : in Ref) return "; 
  AST_Decl *d = field_type();  
  name = adabe_name::narrow_from_decl(d)->dump_name(with, String, previousdefinition);
  String += name + ";\n";  
  name_of_the_package = adabe_name::narrow_from_decl(ScopeAsDecl(defined_in()))->get_ada_full_name();
  INDENT(String);
  String += "Opcd : " + name_of_the_package + ".Proxies.Get_" + get_ada_name() + "_Proxy ;\n";
  INDENT(String);
  String += "Result : " + name +";\n";
  INDENT(String);
  String += "begin \n";
  INC_INDENT();
  INDENT(String);
  String += "Assert_Ref_Not_Nil(Self);";
  INDENT(String);
  String += "Opcd := " + name_of_the_package + ".Proxies.Create();\n";
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
  if (!pd_readonly)
    {
      INDENT(String);
      String += "procedure set_" + get_ada_name() +"(Self : in Ref, To : in ";
      String += name + ") is \n";
      INDENT(String);
      String += "Opcd : " + name_of_the_package + ".Proxies." + get_ada_name() + "_Proxy ;\n";
      INDENT(String);
      String += "begin \n";
      INC_INDENT();
      INDENT(String);
      String += "Assert_Ref_Not_Nil(Self);";
      INDENT(String);
      String += "Opcd := " + name_of_the_package + ".Proxies.Create(To);";
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
adabe_attribute::produce_impl_ads(dep_list with,string &String, string &previousdefinition)
{
  INDENT(String);
  String += "function get_" + get_ada_name() +"(Self : access Object) return " 
  AST_Decl *d = field_type();
  string name = adabe_name::narrow_from_decl(d)->dump_name(with, String, previousdefinition) + ";\n";
  String += name;
  if (!pd_readonly)
    {
      INDENT(String);
      String += "procedure set_" + name +"(Self : access Object, To : in ";
      String += name;
      String += ");\n";
    }
}

void
adabe_attribute::produce_impl_adb(dep_list with,string &String, string &previousdefinition)
{
  INDENT(String);
  String += "function get_" + get_ada_name() +"(Self : access Object) return ";
  AST_Decl *d = field_type();
  string name = adabe_name::narrow_from_decl(d)->dump_name(with, String, previousdefinition) + ";\n";
  String += name;
  INDENT(String);
  String += "begin\n\n";
  INDENT(String);
  String += "end; \n"; 
  if (!pd_readonly)
    {
      INDENT(String);
      String += "procedure set_" + name +"(Self : access Object, To : in ";
      String += name;
      String += ") is\n";
      INDENT(String);
      String += "begin\n\n";
      INDENT(String);
      String += "end; \n"; 
    } 
}

void
adabe_attribute::produce_proxies_ads(dep_list with,string &String, string &privatedefinition)
{  
  AST_Decl *d = field_type();
  string name = adabe_name::narrow_from_decl(d)->dump_name(with, String, previousdefinition) + ";\n";
  INDENTATION(String);
  String += "type get_" + get_ada_name() +"_Proxy is new OmniProxyCallDesc.Object with private ;\n";
  INDENTATION(String);
  String += "function Create() return get_" + get_ada_name() +"_Proxy ;\n";
  INDENTATION(String);
  String += " procedure Free(Self : in out get_" + get_ada_name() + "_Proxy);\n";
  INDENTATION(String);
  String += " function Aligned_Size(Self : in get_" + get_ada_name() + "_Proxy ; Size_In : in Corba.Unsigned_Long)";
  String += " return Corba.Unsigned_Long ;\n";
  INDENTATION(String);
  String += " procedure Marshal_Arguments(Self : in get_" + get_ada_name() + "_Proxy ; Giop_Client : in out Giop_C.Object);\n";
  INDENTATION(String);
  String += " procedure Unmarshal_Returned_Values(Self : in out get_" + get_ada_name() + "_Proxy ; Giop_Client : in Giop_C.Object);\n";
  INDENTATION(String);
  String += " function Get_Result (Self : in get_" + get_ada_name() + "_Proxy ) return ";
  String += name + "; \n";
  
  INDENTATION(privatedefinition);
  privatedefinition += "type get_" + get_ada_name() + "_Proxy is new OmniProxyCallDesc.Object with record \n";
  INC_INDENT();
  privatedefinition += ;//pointeur sur le type de retour
  DEC_INDENT();
  INDENTATION(privatedefinition); 
  privatedefinition += "end record ;\n";
  
  if (!pd_readonly)
    {
      String += "type set_" + get_ada_name() +"_Proxy is new OmniProxyCallDesc.Object with private ;\n";
      String += "function Create(Arg : in " + name + ") return set_" + get_ada_name() +"_Proxy ;\n";
      String += " procedure Free(Self : in out get_" + get_ada_name() + "_Proxy);\n";
      String += " function Aligned_Size(Self : in get_" + get_ada_name() + "_Proxy ; Size_In : in Corba.Unsigned_Long)";
      String += " return Corba.Unsigned_Long ;\n";
      String += " procedure Marshal_Arguments(Self : in get_" + get_ada_name() + "_Proxy ; Giop_Client : in out Giop_C.Object);\n";
      String += " procedure Unmarshal_Returned_Values(Self : in out get_" + get_ada_name() + "_Proxy ; Giop_Client : in Giop_C.Object);\n";
      
      INDENTATION(privatedefinition);
      privatedefinition += "type get_" + get_ada_name() + "_Proxy is new OmniProxyCallDesc.Object with record \n";
      INC_INDENT();
      privatedefinition += ;// pointeur sur le type de retour
      DEC_INDENT();
      INDENTATION(privatedefinition); 
      privatedefinition += "end record ;\n";
    }  
}


IMPL_NARROW_METHODS1(adabe_attribute, AST_Attribute)
IMPL_NARROW_FROM_DECL(adabe_attribute)
IMPL_NARROW_FROM_SCOPE(adabe_attribute)
















