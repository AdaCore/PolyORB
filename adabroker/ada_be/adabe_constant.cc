#include <adabe.h>
#include <stdio.h>

adabe_constant::adabe_constant(AST_Expression::ExprType et,
			 AST_Expression *v,
			 UTL_ScopedName *n,
			 UTL_StrList *p)
	   : AST_Constant(et, v, n, p),
	     AST_Decl(AST_Decl::NT_const, n, p),
	     adabe_name()
{
}

void
adabe_constant::produce_ads(dep_list with, string & String, string &previousdefinition)
{
  idl_bool intfconst = 0;
  bool initialized = true;
  char value[256];
  compute_ada_name();
  String +=get_ada_local_name() +": constant ";
  AST_Expression::ExprType etype = et();
  switch (etype) {
  case AST_Expression::EV_short:
    String += "Short_Integer :=";
    sprintf(value,"%d",constant_value()->ev()->u.sval);
    break;
  case AST_Expression::EV_ushort:
    String += "Unsigned_Short_Integer :=";
    sprintf(value,"%d",constant_value()->ev()->u.usval);
    break;
  case AST_Expression::EV_long:
    String += "Short_Integer :=";
    sprintf(value,"%ld",constant_value()->ev()->u.lval);	
    break;
  case AST_Expression::EV_ulong:
    String += "Unsigned_Long_Integer";
    sprintf(value,"%ld",constant_value()->ev()->u.ulval);	
    break;
  case AST_Expression::EV_float:
    String += "Real :=";
    sprintf(value,"%f",constant_value()->ev()->u.fval);
    break;
  case AST_Expression::EV_double:
    String += "Long_Real :=";
    sprintf(value,"%f",constant_value()->ev()->u.dval);	
    break;
  case AST_Expression::EV_char:
    String += "Character :=";
    sprintf(value,"%c",constant_value()->ev()->u.cval);	
    break;
  case AST_Expression::EV_octet:
    String += "Byte :=";
    sprintf(value,"%d", constant_value()->ev()->u.oval);	
    break;
  case AST_Expression::EV_bool:
    String += "Boolean :=";
    sprintf(value,"%ld",constant_value()->ev()->u.bval);
  case AST_Expression::EV_string: {
    string temp;
    temp = constant_value()->ev()->u.strval->get_string();
    String += "String :=";
    write_string_to_ada(temp, String);
    initialized = true;
  }
    break;
  default:
    throw adabe_internal_error(__FILE__,__LINE__,"unexpected type under constant class");
    break;
  }
  if (initialized == false) String += value;
  return;
}


void adabe_constant::write_string_to_ada(string &c_string, string &output) {
  int slash_position;
  while ((slash_position= c_string.find('\'')) !=0) {
    string temp1 = c_string.substr(0,slash_position-1);
    switch (c_string[slash_position+1]) {
    case 'n':
      //... 012 Latin_1.LF
      break;
    case 't':
      //... 011 Latin_1.HT
      break;
    case 'v':
      //...013 Latin_1.VT
      break;
    case 'b':
      //...010 Latin_1.BS
      break;
    case 'r':
      //...015 Latin_1.CR
    break;
    case 'f':
      //...014 Latin_1.FF
      break;
    case 'a':
      //...007 Latin_1.BEL
      break;
    case '\\':
      //...134 Latin_1.Reverse_Solidus
      break;
    case '?':
      //...077 Latin_1.Question
      break;
    case '\'':
      //...047 Latin_1.Apostrophe
      break;
    case '"':
      //...042 Latin_1.Quotation
      break;
    case 'o':
      //...'ooo' -> 'ooo' Character'val(8#ooo#);
      break;
    case 'x':
      //Character'val(16#hhh#)
      break;
    default:
      throw adabe_internal_error(__FILE__,__LINE__,"unexpected escape character in string");
    }
  }
}
