// adabe_attribute 

adabe_attribute::adabe_attribute(idl_bool ro, AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p);
// constructor

IMPL_NARROW_METHODS1(adabe_attribute, AST_Attribute);
IMPL_NARROW_FROM_DECL(adabe_attribute);
IMPL_NARROW_FROM_SCOPE(adabe_attribute);

void
adabe_attribute::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
  name = ada_name.compute();
  cast le field_type en NT
  String += "function get_" + name +"(Self : in Ref) return " 
  String += NT.dump_name(dep_list with,string &String, string &previousdefinition) + ";\n";
  if (!pd_readonly)
  {
    String += "procedure set_" + name +"(Self : in Ref, To : in "
    String += NT.dump_name(dep_list with,string &String, string &previousdefinition);
    String += ");";
  }


*/
void
adabe_operation::produce_adb(dep_list with,string &String, string &previousdefinition);
/*
  
  cast le field_type en NT
  String += "function get_" + get_ada_name() +"(Self : in Ref) return " 
  name = NT.dump_name(dep_list with,string &String, string &previousdefinition);
  String += name + ";\n";  
  name_of_the_package = (cast of 'defined_in()' in an ast_interface).get_ada_name()
  String += "Opcd : " + name_of_the_package + ".Proxies.Get_" + get_ada_name() + "_Proxy ;\n"
  String += "Result : " + name +";\n";
  String += "begin \n";
  String += "Assert_Ref_Not_Nil(Self);";
  String += "Opcd := " + name_of_the_package + ".Proxies.Create();\n";
  String += "OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
  String += "Result := " + name_of_the_package + ".Proxies.Get_Result(Opcd) ;\n";
  String += name_of_the_package + ".Proxies.Free(Opcd) ;\n";
  String += "return Result ;";
  String += "end;";

  if (!pd_readonly)
  {
    String += "procedure set_" + get_ada_name() +"(Self : in Ref, To : in ";
    String += name + ") is \n";
    name_of_the_package = (cast of 'defined_in()' in an ast_interface).get_ada_name()
    String += "Opcd : " + name_of_the_package + ".Proxies." + get_ada_name() + "_Proxy ;\n"
    String += "begin \n";
    String += "Assert_Ref_Not_Nil(Self);";
    String += "Opcd := " + name_of_the_package + ".Proxies.Create(To);";
    String += "OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
    String += name_of_the_package + ".Proxies.Free(Opcd) ;\n";
    String += "return ;";
    String += "end;";    
  }


*/

void
adabe_attribute::produce_impl_ads(dep_list with,string &String, string &previousdefinition);
/*
  cast le field_type en NT
  String += "function get_" + get_ada_name() +"(Self : access Object) return " 
  String += NT.dump_name(dep_list with,string &String, string &previousdefinition) + ";\n";
  if (!pd_readonly)
  {
    String += "procedure set_" + name +"(Self : access Object, To : in "
    String += NT.dump_name(dep_list with,string &String, string &previousdefinition);
    String += ");";
  }


*/

void
adabe_attribute::produce_impl_adb(dep_list with,string &String, string &previousdefinition);
/*
  cast le field_type en NT
  String += "function get_" + get_ada_name() +"(Self : access Object) return " 
  String += NT.dump_name(dep_list with,string &String, string &previousdefinition) + "is\n";
  String += "begin\n\n\n end; \n"; 
  if (!pd_readonly)
  {
    String += "procedure set_" + name +"(Self : access Object, To : in "
    String += NT.dump_name(dep_list with,string &String, string &previousdefinition);
    String += ") is\n";
    String += "begin\n\n\n end; \n"; 
  } 
*/

void
adabe_attribute::produce_proxies_ads(dep_list with,string &String, string &privatedefinition);
/*
  cast le field_type en NT;
  the_type = NT.dump_name(dep_list with,string &String, string &previousdefinition);
  name = get_ada_name();
  String += "type get_" + name +"_Proxy is new OmniProxyCallDesc.Object with private ;\n"
  String += "function Create() return get_" + name +"_Proxy ;\n";
  String += " procedure Free(Self : in out get_" + name + "_Proxy);\n";
  String += " function Aligned_Size(Self : in get_" + name + "_Proxy ; Size_In : in Corba.Unsigned_Long)";
  String += " return Corba.Unsigned_Long ;\n";
  String += " procedure Marshal_Arguments(Self : in get_" + name + "_Proxy ; Giop_Client : in out Giop_C.Object);\n";
  String += " procedure Unmarshal_Returned_Values(Self : in out get_" + name + "_Proxy ; Giop_Client : in Giop_C.Object);\n";
  String += " function Get_Result (Self : in get_" + name + "_Proxy ) return ";
  String += the_type + "; \n"; 

 
  privatedefinition += "type get_" + name + "_Proxy is new OmniProxyCallDesc.Object with record \n";
  privatedefinition += ////////////////////// pointeur sur le type de retour
  privatedefinition += "end record ;\n";
  
if (!pd_readonly)
  {
    String += "type set_" + name +"_Proxy is new OmniProxyCallDesc.Object with private ;\n"
    String += "function Create(Arg : in " + the_type + ") return set_" + name +"_Proxy ;\n";
    String += " procedure Free(Self : in out get_" + name + "_Proxy);\n";
    String += " function Aligned_Size(Self : in get_" + name + "_Proxy ; Size_In : in Corba.Unsigned_Long)";
    String += " return Corba.Unsigned_Long ;\n";
    String += " procedure Marshal_Arguments(Self : in get_" + name + "_Proxy ; Giop_Client : in out Giop_C.Object);\n";
    String += " procedure Unmarshal_Returned_Values(Self : in out get_" + name + "_Proxy ; Giop_Client : in Giop_C.Object);\n";
    
    privatedefinition += "type get_" + name + "_Proxy is new OmniProxyCallDesc.Object with record \n";
    privatedefinition += ////////////////////// pointeur sur le type de retour
    privatedefinition += "end record ;\n";
  }  
*/


















