//file  adabe_argument 

adabe_argument::adabe_argument(AST_Argument::Direction d, AST_Type *ft, UTL_ScopedName *n,
		UTL_StrList *p);
//constructor

adabe_argument::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
  String += ada_name.compute() + " :"
  case of sur pd_direction et String += "direction"
  cast le field_type en NT puis
     String += NT.dump_name( with, &String, &previousdefinition);

 */
adabe_argument::produce_adb(dep_list with,string &String, string &previousdefinition);
/*
   produce_ads(with, &String, &previousdefinition);
 */


adabe_argument::produce_impl_ads(dep_list with,string &String, string &previousdefinition);
/*
produce_ads( with, &String, &previousdefinition);

 */

///////////////perhaps useless////////////////////////
adabe_argument::produce_impl_adb(dep_list with,string &String, string &previousdefinition);
/*
   produce_ads(with, &String, &previousdefinition);
 */




IMPL_NARROW_METHODS1(adabe_argument, AST_Argument);
IMPL_NARROW_FROM_DECL(adabe_argument);
