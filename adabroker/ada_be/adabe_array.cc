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
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) ;\n\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) ;\n\n";

  body += "   function Align_Size (A : in";
  body += get_ada_local_name();
  body += " ;\n";
  body += "               Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "               N : in Corba.Unsigned_Long := 1)\n";
  body += "               return Corba.Unsigned_Long ;\n\n\n";

}

void
adabe_array::produce_marshal_adb(dep_list& with,string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) is\n";
  body += "    i:Integer;\n";
  body += "   begin\n";
  body += "     for i in A'range loop \n";
  body += "      Marshall (A(i); S); \n";
  body += "     end loop;\n ";
  body += "   end Marshall\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) is \n\n";
  body += "    i:Integer;\n";
  body += "   begin\n";
  body += "     for i in A'range loop \n";
  body += "      UnMarshall (A(i); S);\n";
  body += "     end loop;\n ";
  body += "   end UnMarshall\n";

  body += "   function Align_Size (A : in";
  body += get_ada_local_name();
  body += " ;\n";
  body += "               Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "               N : in Corba.Unsigned_Long := 1)\n";
  body += "               return Corba.Unsigned_Long ;\n\n\n";
  body += "   begin\n";
  body += "      return Align_Size (A'First;Initial_Offset; N*A'Length);\n";
  body += "   end Align_Size\n";

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

    

