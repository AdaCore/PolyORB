//file adabe_array.cc

#include <adabe.h>
#include <stdio.h>
  
IMPL_NARROW_METHODS1(adabe_array, AST_Array);
IMPL_NARROW_FROM_DECL(adabe_array);

adabe_array::adabe_array(UTL_ScopedName *n, unsigned long ndims, UTL_ExprList *dims):
  AST_Array(n,ndims,dims),
  AST_Decl(AST_Decl::NT_array, n, NULL),
  adabe_name(AST_Decl::NT_array,n,NULL)
{
}

void
adabe_array::produce_ads(dep_list& with,string &body, string &previous) {
  char number[256];
  int i;

  compute_ada_name();
  body += "type " + get_ada_local_name() + "is array ";
  for (i=0; i < n_dims(); i++) {
    body += "( 0 ...";  
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
    body +=number;
    body +=")";
  }
  body+="of\n"+ (dynamic_cast<adabe_name *>(base_type())->dump_name(with, body, previous));
  set_already_defined();
}


void
adabe_array::produce_marshal_ads(dep_list& with,string &body, string &previous)
{
}

void
adabe_array::produce_marshal_adb(dep_list& with,string &body, string &previous)
{
}

string adabe_array::dump_name(dep_list& with,string &body, string &previous) 
{
  if (!is_imported(with))
    {
      if (!is_already_defined())
	{
	  string tmp = "";
	  produce_ads(with, tmp, previous);
	  previous += tmp;
	}
      return get_ada_local_name();
    }
  return get_ada_full_name();	   
}

    

