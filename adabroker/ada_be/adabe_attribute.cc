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
  if (pd_readonly)
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
  name_of_the_package = ???????????????????????????????????????????????????????????????????????????????????????????????
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

  if (pd_readonly)
  {
    String += "procedure set_" + name +"(Self : in Ref, To : in ";
    String += name + ") is \n";
    name_of_the_package = ???????????????????????????????????????????????????????????????????????????????????????????????
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


//  void produce_adb(std::fstream& s);
//  void produce_impl_ads(std::fstream& s);
//  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);


















