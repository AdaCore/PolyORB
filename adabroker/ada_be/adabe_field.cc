// File adabe_struct.cc

adabe_structure::adabe_structure(UTL_ScopedName *n, UTL_StrList *p);
//constructor

adabe_structure::produce_ads(dep_list with,string &String, string &previousdefinition) {
  /*
    ada_name.compute;
    String += "type " + ada_name + "is record\n"
    for each node in the structure:
       {
         cast of the item to the real adabe type
         field.produce_ads (with, String, previousdefinition)
       }
    String += "end record"   
  */
}

IMPL_NARROW_METHODS1(adabe_structure, AST_Structure);
IMPL_NARROW_FROM_DECL(adabe_structure);
IMPL_NARROW_FROM_SCOPE(adabe_structure);




