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
  String += NT.dump_name(dep_list with,string &String, string &previousdefinition);
  if (pd_readonly)
  {
    String += "procedure get_" + name +"(Self : in Ref, To : in "
    String += NT.dump_name(dep_list with,string &String, string &previousdefinition);
    String += ");";
  }


*/
adabe_attribute::produce_adb(dep_list with,string &String, string &previousdefinition);
/*


//  void produce_adb(std::fstream& s);
//  void produce_impl_ads(std::fstream& s);
//  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);


















