// file  adabe_interface_fwd

adabe_interface_fwd(UTL_ScopedName *n, UTL_StrList *p);
//constructor

IMPL_NARROW_METHODS1(adabe_interface_fwd, AST_InterfaceFwd);
IMPL_NARROW_FROM_DECL(adabe_interface_fwd);
IMPL_NARROW_FROM_SCOPE(adabe_interface_fwd);

adabe_interface_fwd::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
ada_name.compute();  
String += "with CORBA.Forward \n"
String += "package" + get_ada_name() + "_Forward is new CORBA.Forward \n" 

 */



//  void produce_adb(std::fstream& s);
  //  void produce_impl_ads(std::fstream& s);
  //  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);










