//file adabe_operation

adabe_operation::adabe_operation(AST_Type *rt, AST_Operation::Flags fl,
		 UTL_ScopedName *n,UTL_StrList *p);
adabe_operation::~adabe_operation(){};
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
    string += " function" + get_ada_name() + "(Self : in Ref ";
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
    string += " procedure" + get_ada_name() + "(Self : in Ref ";
    while the UTL_Scope is not empty               //this
    {
       string += ","
       cast the node into his NT type
       if it is an argument
       argument.produce_ads(dep_list with,string &String, string &previousdefinition);
    }
    string += ", Result : out " return_type_cast.dump_name(dep_list with,string &String, string &previousdefinition) + ");";
  }
*/

void
adabe_operation::produce_adb(dep_list with,string &String, string &previousdefinition);
/*
  String += (case of the Flags)              //oneway ...
  cast the return_type() in his NT if not null;
  if (is_function())
  {
    String += " function" + get_ada_name() + "(Self : in Ref ";
    while the UTL_Scope is not empty (make a copy)              
    {
       String += ","
       cast the node into his NT type
       if it is an argument
       argument.produce_ads(with, &String, &previousdefinition);
    }
    name = return_type_cast.dump_name( with, &String, &previousdefinition);
    name_of_the_package = (cast of 'defined_in()' in an ast_interface).get_ada_name()
    String += ") return " + name +" is \n";
    String += "Opcd : " + name_of_the_package + ".Proxies." + get_ada_name() + "_Proxy ;\n"
    String += "Result : " + name +";\n";
    String += "begin \n";
    String += "Assert_Ref_Not_Nil(Self);";
    String += "Opcd := " + name_of_the_package + ".Proxies.Create(";
    while the UTL_Scope is not empty (make a copy)              
    {
       cast the node into his NT type
       if it is an argument
       String += argument.get_ada_name();
       if UTL_Scope not empty String += ", ";
    }
    String += ") ;\n";
    String += "OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
    String += "Result := " + name_of_the_package + ".Proxies.Get_Result(Opcd) ;\n";
    String += name_of_the_package + ".Proxies.Free(Opcd) ;\n";
    String += "return Result ;";
    String += "end;";
  }
  else
  {
    String += " procedure" + ada.name + "(Self : in Ref ";
    while the UTL_Scope is not empty               //this
    {
       String += ","
       cast the node into his NT type
       if it is an argument
       argument.produce_ads(dep_list with,string &String, string &previousdefinition);
    }
    if return_type not null {
           name =  return_type_cast.dump_name(dep_list with,string &String, string &previousdefinition);
           String += ", Result : out " + name + ") is\n";
	   }
    else   String += ") is \n";
    name_of_the_package = (cast of 'defined_in()' in an ast_interface).get_ada_name()
    String += "Opcd : " + name_of_the_package + ".Proxies." + get_ada_name() + "_Proxy ;\n"
    String += "begin \n";
    String += "Assert_Ref_Not_Nil(Self);";
    String += "Opcd := " + name_of_the_package + ".Proxies.Create(";
    while the UTL_Scope is not empty (make a copy)              
    {
       cast the node into his NT type
       if it is an argument
       String += argument.get_ada_name();
       if UTL_Scope not empty String += ", ";
    }
    if return_type() not null String += ", Result) ;\n"
    else String += ");\n";
    String += "OmniProxyCallWrapper.Invoke(Self, Opcd) ;\n";
    String += name_of_the_package + ".Proxies.Free(Opcd) ;\n";
    String += "return ;";
    String += "end;";
    
  }

*/

void
adabe_operation::produce_impl_ads(dep_list with,string &String, string &previousdefinition);
/*
  string += (case of the Flags)              //oneway ...
  cast the return_type() in his NT;
  if (is_function())
  {
    string += " function" + get_ada_name() + "(Self : access Object ";
    while the UTL_Scope is not empty (make a copy)               //this
    {
       string += ","
       cast the node into his NT type
       if it is an argument
       argument.produce_impl_ads(with, &String, &previousdefinition);
    }
    string += ") return " + return_type_cast.dump_name( with, &String, &previousdefinition) + ";";
  }
  else
  {
    string += " procedure" + get_ada_name() + "(Self : access Object ";
    while the UTL_Scope is not empty               //this
    {
       string += ","
       cast the node into his NT type
       if it is an argument
       argument.produce_impl_ads(dep_list with,string &String, string &previousdefinition);
    }
    string += ", Result : out " return_type_cast.dump_name(dep_list with,string &String, string &previousdefinition) + ");";
  }
*/

void
adabe_operation::produce_impl_adb(dep_list with,string &String, string &previousdefinition);
/*
  String += (case of the Flags)              //oneway ...
  cast the return_type() in his NT if not null;
  if (is_function())
  {
    String += " function" + get_ada_name() + "(Self : access Object ";
    while the UTL_Scope is not empty (make a copy)              
    {
       String += ","
       cast the node into his NT type
       if it is an argument
       argument.produce_impl_adb(with, &String, &previousdefinition);
    }
    name = return_type_cast.dump_name( with, &String, &previousdefinition);
    name_of_the_package = (cast of 'defined_in()' in an ast_interface).get_ada_name()
    String += ") return " + name +" is \n";
    String += "begin \n\n";
    String += "end;";
  }
  else
  {
    String += " procedure" + ada.name + "(Self : access Object ";
    while the UTL_Scope is not empty               //this
    {
       String += ","
       cast the node into his NT type
       if it is an argument
       argument.produce_impl_adb(dep_list with,string &String, string &previousdefinition);
    }
    if return_type not null {
           name =  return_type_cast.dump_name(dep_list with,string &String, string &previousdefinition);
           String += ", Result : out " + name + ") is\n";
	   }
    else   String += ") is \n";
    String += "begin \n\n";
    String += "end;";
    
  }

*/

void
adabe_operation::produce_proxies_ads(dep_list with,string &String, string &privatedefinition);
/*
  name = get_ada_full_name();
  String += "type " + name +"_Proxy is new OmniProxyCallDesc.Object with private ;\n"
  String += "function Create(";
  while the UTL_Scope is not empty               //this

///////////////////// agruments du Create ////////////////////////////
  
  String += ") return " + name +"_Proxy ;\n";
  String += " procedure Free(Self : in out " + name + "_Proxy);\n";
  String += " function Aligned_Size(Self : in " + name + "_Proxy ; Size_In : in Corba.Unsigned_Long)";
  String += " return Corba.Unsigned_Long ;\n";
  String += " procedure Marshal_Arguments(Self : in " + name + "_Proxy ; Giop_Client : in out Giop_C.Object);\n";
  String += " procedure Unmarshal_Returned_Values(Self : in out " + name + "_Proxy ; Giop_Client : in Giop_C.Object);\n";
  if (is_function())
  {
     String += " function Get_Result (Self : in " + name + "_Proxy ) return ";
     String += return_type_cast.dump_name( with, &String, &previousdefinition) + "; \n"; 
  }

  privatedefinition += "type " + name + "_Proxy is new OmniProxyCallDesc.Object with record \n";
  while the UTL_Scope is not empty               //this

///////////////////// pointeur sur les arguments ////////////////////////////
  
  privatedefinition += "end record; \n ;";
    
*/


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















