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
    compute_ada_name();

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

string adabe_string::dump_name (dep_list &with,string &body, string &previous)
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

