//file  adabe_typedef

adabe_typedef::adabe_typedef(AST_Type *bt, UTL_ScopedName *n, UTL_StrList *p);
//constructor

IMPL_NARROW_METHODS1(adabe_typedef, AST_Typedef);
IMPL_NARROW_FROM_DECL(adabe_typedef);

adabe_typedef::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
  string += "type" + ada_name.compute() + "is new ";
  case of (base_type()->node_type())  in order to cast the base_type
  string += base_type().dump_name( with, &String, &previousdefinition);
*/


//  void produce_adb(std::fstream& s);
//  void produce_impl_ads(std::fstream& s);
//  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);




