//file adabe_array.cc

#include <adabe.h>
#include <stdio.h>
  
IMPL_NARROW_METHODS1(adabe_enum, AST_Enum);
IMPL_NARROW_FROM_DECL(adabe_enum);
IMPL_NARROW_FROM_SCOPE(adabe_enum);

void adabe_array::produce_ads(dep_list with,string &String, string &previousdefinition) {
  string temp;
  char number[256];
  int i;

  compute_ada_name();
  temp += "type " + get_ada_local_name() + "is array ";
  for (i=0; i < n_dims(); i++) {
    temp += "( 0 ...";  
    AST_Expression::AST_ExprValue* v = dims()[i]->ev();
    switch (v->et) 
    {
      case AST_Expression::EV_short:
      sprintf (number, "%d", v->u.sval);
      break;
      case AST_Expression::EV_ushort:
      sprintf (number, "%d", v->u.usval);
      break;
      case AST_Expression::EV_long:
      sprintf (number, "%ld", v->u.lval);
      break;
      case AST_Expression::EV_ulong:
      sprintf (number, "%ld", v->u.ulval);
      break;
      default:
      throw adabe_internal_error(__FILE__,__LINE__,"unexpected type in array expression");
    }
    temp +=number;
    temp +=")";
  }
  temp+="of\n"+ (adabe_name::narrow_from_decl(base_type())->dump_name(with, String, previousdefinition));
  set_already_defined();
  previousdefinition += temp;
}

string adabe_array::dump_name(dep_list with,string &String, string &previousdefinition) {
  if (!is_already_defined()) {
    string temp;
    produce_ads( with, String, temp);
    previousdefinition += temp;
  }
  return get_ada_full_name();
}

    

