// File string.cc
#include <adabe.h>

IMPL_NARROW_METHODS1(adabe_string, AST_String);
IMPL_NARROW_FROM_DECL(adabe_string);


adabe_string::adabe_string(AST_Expression *v):
  AST_String(v),
  adabe_name()
{
}
adabe_string::adabe_string(AST_Expression *v, long wide):
  AST_String(v,wide),
  adabe_name()
{
}

void adabe_string::produce_ads (dep_list with,string &String, string &previousdefinition) {
    compute_ada_name();
    string is_bouded;
    string temp;

    //look if the string is bounded or not;
    
    if (max_size()->ev()==0)
      temp = "unbouded";
    else
      temp = "bounded";
    
    temp+= "type " + get_ada_local_name() + " is new CORBA."+is_bouded;
    previousdefinition +=temp;
}

string adabe_string::dump_name (dep_list with,string &String, string &previousdefinition) {
  if (!is_already_defined()) {
    string temp;
    produce_ads (with, String, temp);
    previousdefinition += temp;
  }
  return get_ada_local_name();
}

