//file adabe_predefined_type


adabe_predefined_type::adabe_predefined_type(AST_PredefinedType::PredefinedType t, UTL_ScopedName *n, UTL_StrList *p)
  //constructor

void 
adabe_predefined_type::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
  String += get_ada_predefined_type();
*/

string
adabe_predefined_type::dump_name(dep_list with,string &String, string &previousdefinition) {
  /*  
      return get_ada_predefined_type();
  */





string
ada_predefined_type::get_ada_predefined_type();

/*
   case of sur pt(), afin de determiner le type ada
*/

IMPL_NARROW_METHODS1(adabe_predefined_type, AST_PredefinedType);
IMPL_NARROW_FROM_DECL(adabe_predefined_type);
 





