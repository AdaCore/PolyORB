// File adabe_sequence.cc
#include <adabe.h>

void
adabe_sequence::produce_ads(dep_list& with, string &body,
			    string &previous){
  string is_bounded;
  adabe_name *adabe_base_type;

  
  if (max_size() == 0)
    is_bounded = "Bounded";
  else
    is_bounded = "Unbounded";

  with.add("Corba.Object.Sequences." + is_bounded);

  // Writing the header :

  adabe_base_type =  dynamic_cast<adabe_name *> (base_type());
  string type_name =  adabe_base_type->dump_name(with, previous);
  string short_type_name = type_name.substr(type_name.find_last_of('.') + 1) ;

  body += "   type IDL_SEQUENCE_" + short_type_name +"_Array is ";
  body += "array (Integer range <>) of " + type_name +" ;\n";
  body += "\n";
  body += "   package  IDL_SEQUENCE_" + short_type_name +" is new\n";
  body += "      Corba.Sequences." + is_bounded;
  body += "(Corba." + type_name + ", IDL_SEQUENCE_" + short_type_name +"_Array);\n";
  body += "   type " + get_ada_local_name() + " is new IDL_SEQUENCE_" + short_type_name + ".Sequence ;\n";

}
void
adabe_sequence::produce_marshal_ads(dep_list& with, string &body,string &previous)
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
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long ;\n\n\n";
}

void
adabe_sequence::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  string name = (dynamic_cast<adabe_name *> (base_type()))->marshal_name(with, body); 
  
  body += "   -- Marshall\n";
  body += "   -----------\n";
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name ();
  body += " ;\n"; 
  body += "                       S : in out Object'Class) is\n";
  body += "      Len : Corba.Unsigned_Long\n";
  body += "        := Corba.Unsigned_Long (Length(A)) ;\n";
  body += "   begin\n";
  body += "      Marshall (Len,S) ;\n";
  body += "      for I in (1..Len) loop\n";
  body += "         Marshall (Element_Of(A,I),S) ;\n";
  body += "      end loop ;\n";
  body += "   end ;\n\n\n";
  
  body += "   -- UnMarshall\n";
  body += "   -------------\n";
  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name ();
  body += " ;\n"; 
  body += "                         S : in out Object'Class) is\n";
  body += "      Len : Corba.Unsigned_Long ;\n";
  body += "   begin\n";
  body += "      UnMarshall (Len,S);\n";
  body += "      declare\n";
  body += "         Tmp : Element_Array (1..Len) ;\n";
  body += "      begin\n";
  body += "         for I in (1..Len) loop\n";
  body += "            UnMarshall (Tmp(I),S) ;\n";
  body += "         end loop ;\n";
  body += "         return To_Sequence (Tmp) ;\n";
  body += "      end ;\n";
  body += "   end ;\n\n\n";

  body += "   -- Align_Size\n";
  body += "   -------------\n";
  body += "   function Align_Size (A : in ";
  body += get_ada_local_name ();
  body += " ;\n"; 
  body += "                        Initial_Offset : in Corba.Unsigned_Long)\n";
  body += "                        return Corba.Unsigned_Long is\n";
  body += "      Len : Corba.Unsigned_Long\n";
  body += "        := Corba.Unsigned_Long (Length(A)) ;\n";
  body += "      Tmp : Corba.Unsigned_Long ;\n";
  body += "   begin\n";
  body += "      Tmp := Align_Size (Len,Initial_Offset) ;\n";
  body += "      for I in (1..Len) loop\n";
  body += "         Align_Size (Element_Of(A,I),Tmp) ;\n";
  body += "      end loop ;\n";
  body += "      return Tmp ;\n";
  body += "   end ;\n\n\n";
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
      return get_ada_local_name();
    }
  return get_ada_full_name();	   
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
      return get_ada_local_name();
    }
  return get_ada_full_name();	   
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













