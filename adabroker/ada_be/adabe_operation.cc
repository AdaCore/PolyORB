//file adabe_operation

adabe_operation::adabe_operation(AST_Type *rt, AST_Operation::Flags fl,
		 UTL_ScopedName *n,UTL_StrList *p);
adabe_operation::~adabe_operation() {}
// constructor and destructor

IMPL_NARROW_METHODS1(adabe_operation, AST_Operation);
IMPL_NARROW_FROM_DECL(adabe_operation);
IMPL_NARROW_FROM_SCOPE(adabe_operation);

void
adabe_operation::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
  ada_name.compute()
  string += (case of the Flags)              //oneway ...
  cast the return_type() in his NT;
  if (is_function())
  {
    string += " function" + ada.name + "(Self : in Ref ";
    while the UTL_Scope is not empty (make a copy)               //this
    {
       string += ","
       cast the node into his NT type
       if it is an argument
       argument.produce_ads(with, &String, &previousdefinition);
    }
    string += ") return " + return_type_cast.dump_name( with, &String, &previousdefinition) + ";";
  }
  else
  {
    string += " procedure" + ada.name + "(Self : in Ref ";
    while the UTL_Scope is not empty               //this
    {
       string += ","
       cast the node into his NT type
       if it is an argument
       argument.produce_ads(dep_list with,string &String, string &previousdefinition);
    }
    string += ", Returns : out " return_type_cast.dump_name(dep_list with,string &String, string &previousdefinition) + ");";
  }
*/




//void produce_adb(std::fstream& s);
//void produce_impl_ads(std::fstream& s);
//void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);



bool  
adabe_operation::is_function();
//check if the operation is a function or a procedure

/*
  test = AST_Argument.Direction := dir_IN;
  bool = (return_type()==NULL)
  take the UTL_Scope of argument; take the first and cast it into his NT.
  If it is an argument do
  while ((bool)&&(argument.direction()== test)&&(!(Scope->is_done())))
    {
    take another node of the Scope and cast it into an agument
    }
  return(bool)

*/






















