#include <adabe.h>

adabe_typedef::adabe_typedef(AST_Type *bt, UTL_ScopedName *n, UTL_StrList *p)
	  : AST_Typedef(bt, n, p),
	    AST_Decl(AST_Decl::NT_typedef, n, p),
	    adabe_name(AST_Decl::NT_typedef, n, p)

{
}

void
adabe_typedef::produce_ads(dep_list& with, string &body, string &previous)
{
  compute_ada_name();
  body += "   type " + get_ada_local_name() + " is new ";
  AST_Decl *b = base_type();
  string name =  dynamic_cast<adabe_name *>(b)->dump_name(with, body, previous); //virtual method
  body += name;
  body += ";\n";
  body += "   type" + get_ada_local_name() + "_Ptr is access all " + get_ada_local_name() + ";\n";
  body += "   procedure free is new Unchecked_Deallocation(";
  body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr);\n";
  set_already_defined();
}

/*
  void
  adabe_typedef::produce_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  return get_ada_full_name();	   
  }
  
  void
  adabe_typedef::produce_impl_ads(dep_list& with,string &body, string &previous)
  {
  INDENTATION(body);
  body += "type" + get_ada_local_name() + "is new ";
  AST_Decl *b  base_type();
  string name =  dynamic_cast<adabe_name *>(b)->dump_name(with, &body, &previous); //virtual method
  body += name;
  body += ";\n";
  }
  
  void
  adabe_typedef::produce_impl_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  return get_ada_full_name();
  }
*/

void
adabe_typedef::produce_marshal_ads(dep_list& with, string &body, string &previous)
{
  body += "   function Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) ;\n\n";
  body += "   function UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) ;\n\n";
  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long ;\n\n\n";
}

void
adabe_typedef::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  string arg1, arg2 = "";
  string name = (dynamic_cast<adabe_name *> (base_type()))->dump_name(with, arg1, arg2); 
  
  body += "   function Marshall(A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) is\n";
  body += "   begin\n";
  body += "      Marshall(";
  body += name;
  body += "(A), S) ;\n";
  body += "   end ;\n\n\n";
  
  body += "   function UnMarshall(A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) is\n";
  body += "   begin\n";
  body += "      UnMarshall(";
  body += name;
  body += "(A) ,S) ;\n";
  body += "   end ;\n\n\n";
    
  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long is\n";
  body += "      Tmp : Corba.Unsigned_Long := 0 ;\n";
  body += "   begin\n";
  body += "      Tmp := Align_Size(";
  body += name;
  body += "(A) , Tmp) ;\n";
  body += "   end ;\n\n\n";
}


string
adabe_typedef::dump_name(dep_list& with, string &body, string &previous)
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

IMPL_NARROW_METHODS1(adabe_typedef, AST_Typedef)
IMPL_NARROW_FROM_DECL(adabe_typedef)








