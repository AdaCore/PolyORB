A//file adabe_union

adabe_union::adabe_union(AST_ConcreteType *dt, UTL_ScopedName *n, UTL_StrList *p);
//constructor

void
adabe_union::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
  ///////////WARNING//////////////
  the type of the discriminant should be check. From this type result a specific solution  



  
  ada_name.compute;
  temp += "type " + ada_name
  name = disc_type().get_ada_name().compute
  temp += "(Switch : "  + disc_type().dump_name() + " := " + name + "'first) is record";
  temp += "\n case Switch is"
  for each node in UTL_scope:    //this
  {
      cast of the item to the real adabe type, if it is a union branch do
      branch.produce_ads(with,&temp,&previousdefinition,disc_type())
     
  }
  temp += "end case \n"
  temp += "end record \n";
  previousdefinition += temp;
  
*/

string
adabe_union::dump_name(dep_list with,string &String, string &previousdefinition) {
  /*
      if (!is_imported(with))
      {
          if (!is_already_defined())
                      produce_ads( with, String, previousdefinition);
           return get_ada_name();}
      return get_ada_full_name();	   
  */
void
adabe_union::produce_adb(dep_list with,string &String, string &previousdefinition) {
  /*
      if (!is_imported(with)) return get_ada_name();}
      return get_ada_full_name();	   
  */

void
adabe_union::produce_impl_ads(dep_list with,string &String, string &previousdefinition);
/*
produce_ads(with, &String, &previousdefinition);
 */
  
IMPL_NARROW_METHODS1(adabe_union, AST_Union);
IMPL_NARROW_FROM_DECL(adabe_union);
IMPL_NARROW_FROM_SCOPE(adabe_union);


//void produce_adb(std::fstream& s);
//void produce_impl_ads(std::fstream& s);
//void produce_impl_adb(std::fstream& s);







