//file adabe_union

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
  temp += "(Switch : "  + get_ada_type(pd_udisc_type) + " := " + name + "'first) is record";
  temp += "\n case Switch is"
  for each node in UTL_scope:    //this
  {
      cast of the item to the real adabe type, if it is a union branch do
      temp += "when "
      if (branche->label()->label_kind() != UL_default)
           temp += branche->label()->label_val().produce(with,&tmp,&previousdefinition)
      else if (branche->label()->label_kind() == UL_default)
           temp =+ "others "

      temp =+ "=> "
      cast branche into field
      field.produce_ads(with,&temp,&previousdefinition)
  }
  temp += "end case \n"
  temp += "end record \n";
  previousdefinition += temp;
  
*/

string
adabe_union::dump_name(dep_list with,string &String, string &previousdefinition) {
  /*  if (!is_already_defined())
         &previousdefinition += produce_ads( with, String, previousdefinition);
      return get_ada_name();
  */

IMPL_NARROW_METHODS1(adabe_union, AST_Union);
IMPL_NARROW_FROM_DECL(adabe_union);
IMPL_NARROW_FROM_SCOPE(adabe_union);


//void produce_adb(std::fstream& s);
//void produce_impl_ads(std::fstream& s);
//void produce_impl_adb(std::fstream& s);

private:

string
adabe_union::get_ada_type(AST_Expression::ExprType pd_udisc_type)
  /*
    determine the ada type of pd_udisc_type with a case of the EV_type
    warning: the EV_any correspond to the enum type
  */

};





