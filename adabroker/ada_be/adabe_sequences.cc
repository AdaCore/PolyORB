// File adabe_sequence.cc
#include <adabe.h>

static string to_string(AST_Expression::AST_ExprValue *exp)
{
  char temp[10]; 
  switch( exp->et ) {
  case AST_Expression::EV_short:
    sprintf(temp, "%d",exp->u.sval);
    break;
  case AST_Expression::EV_ushort:
    sprintf(temp, "%d",exp->u.usval);
    break;
  case AST_Expression::EV_long:
    sprintf(temp, "%ld",exp->u.lval);
    break;
  case AST_Expression::EV_ulong:
    sprintf(temp, "%ld",exp->u.ulval);
    break;
  default:
    throw adabe_internal_error (__FILE__,__LINE__,"Unexpected string dimension type");
  }
  //  temp.freeze(temp.pcount());
  return temp;
}

static int evaluate (AST_Expression::AST_ExprValue *exp)
{
  switch( exp->et ) {
  case AST_Expression::EV_short:
    return exp->u.sval;
    break;
  case AST_Expression::EV_ushort:
    return exp->u.usval;
    break;
  case AST_Expression::EV_long:
    return exp->u.lval;
    break;
  case AST_Expression::EV_ulong:
    return exp->u.ulval;
    break;
  default:
    throw adabe_internal_error (__FILE__,__LINE__,"Unexpected string dimension type");
  }
}

void
adabe_sequence::produce_ads(dep_list& with, string &body,
			    string &previous)
{
  no_fixed_size();
  // set a flag of this object and its ancestors saying
  // they have not a fixed size.
  
  string is_bounded;
  adabe_name *adabe_base_type;

  string seq_size_st = to_string(max_size()->ev());
  int seq_size = evaluate(max_size()->ev());
  bool bounded = (seq_size != 0);
  if (bounded)
    is_bounded = "Bounded";
  else
    is_bounded = "Unbounded";

  with.add("Corba.Sequences." + is_bounded);

  // Writing the header :

  adabe_base_type =  dynamic_cast<adabe_name *> (base_type());
  string type_name =  adabe_base_type->dump_name(with, previous);
  string short_type_name = type_name.substr(type_name.find_last_of('.') + 1) ;
  if (short_type_name == "Sequence") 
    {
      short_type_name = type_name.substr(type_name.find_first_of('_') + 1) ;
      short_type_name = short_type_name.substr(0,short_type_name.length()-9);
    }
  set_ada_local_name("IDL_SEQUENCE_" + short_type_name + ".Sequence");
  body += "   type IDL_SEQUENCE_" + short_type_name +"_Array is ";
  if (bounded)
    body += "array (1.." + seq_size_st + ") of " + type_name +" ;\n";
  else 
    body += "array (Integer range <>) of " + type_name +" ;\n";
  body += "\n";
  body += "   package IDL_SEQUENCE_" + short_type_name + " is new\n";
  body += "      Corba.Sequences." + is_bounded;
  if (bounded) {
    body += "(" + type_name + ", ";
    body += seq_size_st + ");\n";
  }
  else
    body += "(" + type_name +");\n";
  set_already_defined();
}
void
adabe_sequence::produce_marshal_ads(dep_list& with, string &body,string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out Giop_C.Object) ;\n\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                         S : in out Giop_C.Object) ;\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long ;\n\n\n";
  set_already_defined();
}

void
adabe_sequence::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  string name = (dynamic_cast<adabe_name *> (base_type()))->marshal_name(with, body); 
  
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name ();
  body += " ;\n"; 
  body += "                       S : in out Giop_c.Object) is\n";
  body += "      Len : Corba.Unsigned_Long := Corba.Unsigned_Long (Length(A)) ;\n";
  body += "   begin\n";
  body += "      Marshall (Len,S) ;\n";
  body += "      for I in 1..Len loop\n";
  body += "         Marshall (Element_Of(A,I),S) ;\n";
  body += "      end loop ;\n";
  body += "   end ;\n\n\n";
  
  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name ();
  body += " ;\n"; 
  body += "                         S : in out Giop_c.Object) is\n";
  body += "      Len : Corba.Unsigned_Long ;\n";
  body += "   begin\n";
  body += "      UnMarshall (Len,S);\n";
  body += "      declare\n";
  body += "         Tmp : ";
  body += get_ada_local_name ();
  body += ".Element_Array (1..Len) ;\n";
  body += "      begin\n";
  body += "         for I in 1..Len loop\n";
  body += "            UnMarshall (Tmp(I),S) ;\n";
  body += "         end loop ;\n";
  body += "         A := To_Sequence (Tmp) ;\n";
  body += "      end ;\n";
  body += "   end ;\n\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name ();
  body += " ;\n"; 
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long is\n";
  body += "      Len : Corba.Unsigned_Long\n";
  body += "        := Corba.Unsigned_Long (Length(A)) ;\n";
  body += "      Tmp : Corba.Unsigned_Long ;\n";
  body += "   begin\n";
  body += "      for I in 1..N loop\n";
  body += "         Tmp := Align_Size (Len,Initial_Offset) ;\n";
  body += "         for I in 1..Len loop\n";
  body += "            Align_Size (Element_Of(A,I),Tmp) ;\n";
  body += "         end loop ;\n";
  body += "      end loop ;\n";
  body += "      return Tmp ;\n";
  body += "   end ;\n\n\n";
  set_already_defined();
}

string
adabe_sequence::dump_name(dep_list& with, string &previous) 
{
  if (!is_imported(with))
    {
      if (!is_already_defined())
	{
	  string tmp = "";
	  produce_ads(with, tmp, previous);
	  previous += tmp;
	}
      return (get_ada_local_name());
    }
  return (get_ada_local_name());	   
}

string
adabe_sequence::marshal_name(dep_list& with, string &previous) 
{
  if (!is_marshal_imported(with))
    {
      if (!is_already_defined())
	{
	  string tmp = "";
	  produce_marshal_adb(with, tmp, previous);
	  previous += tmp;
	}
      return (get_ada_local_name());
    }
  return (get_ada_local_name());	   
}

IMPL_NARROW_METHODS1(adabe_sequence, AST_Sequence)
IMPL_NARROW_FROM_DECL(adabe_sequence)
IMPL_NARROW_FROM_SCOPE(adabe_sequence)

adabe_sequence::adabe_sequence(AST_Expression *v, AST_Type *t)
  : AST_Sequence(v, t),
    AST_Decl(AST_Decl::NT_sequence,new UTL_ScopedName(new Identifier("sequence",1,0,I_FALSE),NULL),NULL), //...
    adabe_name(AST_Decl::NT_sequence,new UTL_ScopedName(new Identifier("sequence",1,0,I_FALSE),NULL),NULL)
{
}


