//file adabe_predefined_type


adabe_predefined_type::adabe_predefined_type(AST_PredefinedType::PredefinedType t, UTL_ScopedName *n, UTL_StrList *p)
  //constructor

string  
adabe_predefined_type::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
  String += nom_type // issu d'un case of sur pt(), on n'a pas besoin d'un ada_predefined_type() pour eviter un deuxieme case of
*/
  
IMPL_NARROW_METHODS1(adabe_predefined_type, AST_PredefinedType);
IMPL_NARROW_FROM_DECL(adabe_predefined_type);


