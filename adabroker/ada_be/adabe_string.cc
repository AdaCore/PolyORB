// File string.cc
#include <adabe.h>
#include <strstream>

IMPL_NARROW_METHODS1(adabe_string, AST_String);
IMPL_NARROW_FROM_DECL(adabe_string);


adabe_string::adabe_string(AST_Expression *v):
  AST_String(v),
  AST_Decl(AST_Decl::NT_string, new UTL_ScopedName(new Identifier("string", 1, 0, I_FALSE), NULL), NULL),
  adabe_name(AST_Decl::NT_string,new UTL_ScopedName(new Identifier("string",1,0,I_FALSE), NULL), NULL)
{
}
adabe_string::adabe_string(AST_Expression *v, long wide):
  AST_String(v,wide),
  AST_Decl(AST_Decl::NT_string, new UTL_ScopedName(new Identifier("string", 1, 0, I_FALSE), NULL), NULL),
  adabe_name(AST_Decl::NT_string,new UTL_ScopedName(new Identifier("string",1,0,I_FALSE),NULL),NULL)
{
}

void adabe_string::produce_ads (dep_list &with,string &body, string &previous) {
  cout << "PRODUCE ADS OF THE STRING \n";
    compute_ada_name();
#ifdef DEBUG_STRING
    cout << "The name of the string type is : " << get_ada_local_name() <<" in ada" <<endl;
    cout << "and " << local_name()->get_string()  << "in IDL"<< endl;
#endif 
    //look if the string is bounded or not;
    
    if (max_size()->ev()==0)
      {
	    body+= "type " + get_ada_local_name() + " is new CORBA.String";
      }
    else
      {
	ostrstream size;
	size << "package CORBA.Bounded_String_" ;//<< max_size()->ev();
	body +=  size.str();
	body += "is_new CORBA.Bounded_String(";
	body += size.str();
	body += ")\n";
	body += "type "+ get_ada_local_name() + " is new CORBA.Bounded_String_";
	body += size.str();
	body += ".Bounded_String\n";
      }
    set_already_defined();
}


void  
adabe_string::produce_marshal_ads(dep_list& with, string &body, string &previous)
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
adabe_string::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  string tmp="";

  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) is\n";
  body += "   begin\n";
  body += "      Marshall (Corba.String(A); S);";
  body += "   end Marshall\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "      S : in out Giop_C.Object) is \n\n";
  body += "   begin\n";
  body += "      UnMarshall (Corba.String(A); S);";
  body += "   end UnMarshall\n";

  body += "   function Align_Size (A : in";
  body += get_ada_local_name();
  body += " ;\n";
  body += "               Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "               N : in Corba.Unsigned_Long := 1)\n";
  body += "               return Corba.Unsigned_Long ;\n\n\n";

  body += "   begin\n";
  body += "      return Align_Size (Corba.String(A);Initial_Offset;N*(Length(A)));";
  body += "   end Align_Size\n";

}



string adabe_string::dump_name (dep_list &with, string &previous)
{
  cout <<"Valeur de is_imported " <<is_imported(with)<<endl;
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

string adabe_string::marshal_name (dep_list &with, string &previous)
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














