// File adabe_field.cc

adabe_field::adabe_field(UTL_ScopedName *n, UTL_StrList *p);
//constructor

void
adabe_field::produce_ads(dep_list with,string &String, string &previousdefinition) {
  /*
    ada_name.compute();
    String +=  get_ada_name();
    String += " : ";
    cast le field_type en NT puis
    String += NT.dump_name( with, &String, &previousdefinition);
  */
}
adabe_field::produce_adb(dep_list with,string &String, string &previousdefinition) {
  /*
    produce_ads(with, &String, &previousdefinition)


   */
IMPL_NARROW_METHODS1(adabe_field, AST_Field);
IMPL_NARROW_FROM_DECL(adabe_field);
IMPL_NARROW_FROM_SCOPE(adabe_field);












