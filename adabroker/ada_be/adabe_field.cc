// File adabe_field.cc

adabe_field::adabe_field(UTL_ScopedName *n, UTL_StrList *p);
//constructor

adabe_field::produce_ads(dep_list with,string &String, string &previousdefinition) {
  /*
    ada_name.compute;
    String +=  ada_name;
    String += " : ";
    cast le field_type en NT puis
    NT.dump_name(dep_list with,string &String, string &previousdefinition);
  */
}

IMPL_NARROW_METHODS1(adabe_field, AST_Field);
IMPL_NARROW_FROM_DECL(adabe_field);
IMPL_NARROW_FROM_SCOPE(adabe_field);












