/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_argument.cc                                       ***
***                                                                                            ***
***      This file provides the implementation of class adabe_argument  declared in adabe.h    ***
***   (L 420). This class is the correspondant of the Sun's Front-End class AST_Argument.      ***
***   It provides produce functions for each generated file, a constructor and two little      ***
***   functions : dump_name and marshall_name whose job is to print the name of the types.     ***
***                                                                                            ***
***                                                                                            ***
***   Copyright 1999                                                                           ***
***   Jean Marie Cottin, Laurent Kubler, Vincent Niebel                                        ***
***                                                                                            ***
***   This is free software; you can redistribute it and/or modify it under terms of the GNU   ***
***   General Public License, as published by the Free Software Foundation.                    ***
***                                                                                            ***
***  This back-end is distributed in the hope that it will be usefull, but WITHOUT ANY         ***
***  WARRANTY; without even the implied waranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR ***
***  PURPOSE.                                                                                  ***
***                                                                                            ***
***  See the GNU General Public License for more details.                                      ***
***                                                                                            ***
***                                                                                            ***
*************************************************************************************************/

#include <adabe.h>

////////////////////////////////////////////////////////////////////////
////////////////      constructor    ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
adabe_argument::adabe_argument(AST_Argument::Direction d, AST_Type *ft, UTL_ScopedName *n,UTL_StrList *p)
	   : AST_Argument(d, ft, n, p),
	     AST_Field(AST_Decl::NT_argument, ft, n, p),
	     AST_Decl(AST_Decl::NT_argument, n, p),
             adabe_name(AST_Decl::NT_argument, n, p)
{
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_argument::produce_ads(dep_list& with, string &body, string &previous)
  // This methods produces code for the .ads file
  // Actually, it produces for this argument the code to put in a function
  // declaration
{
  // compute the name of this argument
  compute_ada_name();

  // add the name of the argument to the body
  body += get_ada_local_name() + " : ";

  // add its direction (in, inout or out)
  AST_Argument::Direction dir = direction();
  switch (dir)
    {
    case dir_IN :
      body += "in ";
      break;
    case dir_OUT :
      body += "out ";
      break;
    case dir_INOUT :
      body += "in out ";
      break;
    }

  // get the type of the argument as an AST_Decl object
  AST_Decl *d = field_type();

  // add the name of the type to the body
  body +=  dynamic_cast<adabe_name *>(d)->dump_name(with, previous) ;
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_ads     ///////////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_argument::produce_adb(dep_list& with, bool &no_out, string space,
			    string &in_decls, string &in_args, string &out_args)
  // This methods produces code for the .adb file
  // Actually, it produces several pieces of code and booleans
  //    - the boolean no_out is put to false if the argument is in
  // out or inout mode
  //    - in_decls contains the field declarations to put in the object
  // declaration at the end of .adb file
  //    - in_args contains the list of arguments to pass to the Init
  // function corresponding to the operation
  //    - out_args contains the list of arguments to pass to the Get_Result
  // function corresponding to the operation
  // at least space is used for indentation
{
  // get the direction of the argument (in, out or inout)
  AST_Argument::Direction dir = direction();
  string dir_st = "";
  switch (dir)
    {
    case dir_IN :
      dir_st = "in ";
      break;
    case dir_OUT :
      dir_st = "out ";
      break;
    case dir_INOUT :
      dir_st = "in out ";
      break;
    }

  // get the type of the argument as an AST_Decl object
  AST_Decl *d = field_type();
  
  string previous = "";  // temporary string. unused

  // get the name of the argument via dump_name function
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous);

  // computation of in_decls string
  in_decls += ";\n              " + space + get_ada_local_name() + " : " + dir_st +  name;

  // computation of in_args string if the argument is in in or inout mode
  if ((dir == AST_Argument::dir_IN) || (dir == AST_Argument::dir_INOUT))
    in_args += ", " + get_ada_local_name ();

  // computation of out_args string if the argument is in out or inout mode
  // in this case, put no_out to false
  if ((dir == AST_Argument::dir_OUT) || (dir == AST_Argument::dir_INOUT))
    {
      no_out = false;
      out_args += ", " + get_ada_local_name ();
    }
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_ads     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void 
adabe_argument::produce_proxies_ads(dep_list &with, string &in_decls,bool &no_in,
				    bool &no_out, string &fields, string &out_args)
  // This methods produces code for the proxies.ads file
  // Actually, it produces several pieces of code and booleans
  //    - in_decls contains the list of arguments to put in the declaration
  // of the Init function corresponding to the operation
  //    - the boolean no_in is put to false if the argument is in
  // in or inout mode
  //    - the boolean no_out is put to false if the argument is in
  // out or inout mode
  //    - fields contains the list of declarations to put in the private part,
  // at the end of the proxies.ads file
  //    - out_args contains the list of arguments to put in the declaration
  // of the Get_Result function corresponding to the operation
{
  string previous = "";  // temporary unused string

  // get the argument type as an adabe_name object
  AST_Decl *d = field_type();
  adabe_name *type_adabe_name = dynamic_cast<adabe_name *>(d) ;

  // get the name o the argument via the dump_name function
  string type_name = type_adabe_name->dump_name(with, previous);

  // get the full name of the argument
  string full_type_name = type_adabe_name->get_ada_full_name() ;

  // computation of in_decls if the argument is in in or inout mode
  // in this case, put no_in to false
  if ((direction() == AST_Argument::dir_IN) || (direction() == AST_Argument::dir_INOUT)) {
    no_in = false;
    in_decls += " ;\n                  ";
    in_decls += get_ada_local_name ();
    in_decls += " : in ";
    in_decls += type_name;
  }

  // computation of out_args if the argument is in out or inout mode
  // in this case, put no_out to false
  if ((direction() == AST_Argument::dir_OUT) || (direction() == AST_Argument::dir_INOUT)) {
    no_out = false;
    out_args += "; " + get_ada_local_name () + " : out " + type_name;
  }

  // computation of fields for all arguments 
  fields += "      ";
  fields += get_ada_local_name ();
  fields += " : ";
  fields += type_name;
  fields += ";\n";
}


////////////////////////////////////////////////////////////////////////
////////////////     produce_proxies_adb     ///////////////////////////
////////////////////////////////////////////////////////////////////////
void 
adabe_argument::produce_proxies_adb(dep_list &with, string &in_decls, bool &no_in, bool &no_out, string &init,
                                    string &align_size, string&marshall, string &unmarshall_decls, string &unmarshall,
                                    string &finalize, string &out_args, string &result_decls)
  // This methods produces code for the proxies.adb file
  // Actually, it produces several pieces of code and booleans
  //    - in_decls contains the list of arguments to put in the header
  // of the Init function corresponding to the operation
  //    - the boolean no_in is put to false if the argument is in
  // in or inout mode
  //    - the boolean no_out is put to false if the argument is in
  // out or inout mode
  //    - init contains the code to put in the init function
  //    - align_size contains the code to put in the align_size function
  //    - marshall contains the code to put in the marshall function
  //    - unmarshall_decls contains the list of declarations to put in the header
  // of the unmarshal_returned_values function
  //    - unmarshall contains the code to put in the unmarshall_returned_values function
  //    - finalize contains the code to put in the finalize function
  //    - out_args contains the list of arguments to put in the declaration
  // of the Get_Result function corresponding to the operation
  //    - result_decls contains the code to put in the get_result function
{
  finalize += "      null;\n";
  string body, previous = "";
  AST_Decl *d = field_type();
  string type_name =  dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  dynamic_cast<adabe_name *>(d)->is_marshal_imported(with);
  // in order to include the marshal files


  string full_type_name = dynamic_cast<adabe_name *>(d)->get_ada_full_name() ;
  
  if ((direction() == AST_Argument::dir_IN) || (direction() == AST_Argument::dir_INOUT)) {
    no_in = false;
    in_decls += " ;\n                  ";
    in_decls += get_ada_local_name ();
    in_decls += " : in ";
    in_decls += type_name;

    init += "      Self.";
    init += get_ada_local_name ();
    init += " := ";
    init += get_ada_local_name ();
    init += ";\n";

    align_size += "      Tmp := Align_size(Self.";
    align_size += get_ada_local_name ();
    align_size += ", Tmp) ;\n";

    marshall += "      Marshall(Self.";
    marshall += get_ada_local_name ();
    marshall += ", GIOP_Client) ;\n";
  }
  if ((direction() == AST_Argument::dir_OUT) || (direction() == AST_Argument::dir_INOUT)) {
    dynamic_cast<adabe_name *>(d)->is_marshal_imported(with);
    no_out = false;
    unmarshall_decls += "      ";
    unmarshall_decls += get_ada_local_name ();
    unmarshall_decls += " : ";
    unmarshall_decls += type_name;
    unmarshall_decls += " ;\n";

    if (direction() == dir_INOUT) {
      // unmarshall += "      Free (Self.";
      // unmarshall += get_ada_local_name ();
      // unmarshall += ") ;\n";
}
    unmarshall += "      Unmarshall(";
    unmarshall += get_ada_local_name ();
    unmarshall += ",GIOP_Client) ;\n";
    unmarshall += "      Self.";
    unmarshall += get_ada_local_name ();
    unmarshall += " := ";
    unmarshall += get_ada_local_name ();
    unmarshall += ";\n";
    
    out_args += "; " + get_ada_local_name () + " : out " + type_name;
    
    result_decls += "      " + get_ada_local_name ();
    result_decls += " := Self." + get_ada_local_name () + ";\n";
  }
  
  // finalize += "      Free(Self.";
  // finalize += get_ada_local_name ();
  // finalize += ") ;\n";
}

////////////////////////////////////////////////////////////////////////
////////////////     produce_skel_adb     //////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_argument::produce_skel_adb(dep_list &with, string &in_decls ,
				 bool &no_in, bool no_out,
				 string &unmarshall, string &call_args,
				 string &marshall, string &align_size)
{
  string previous = "";
  AST_Decl *d = field_type();
  adabe_name *e = dynamic_cast<adabe_name *>(d);
  string type_name = e->dump_name(with, previous);
  e->is_marshal_imported(with);

  in_decls += "            ";
  in_decls += get_ada_local_name ();
  in_decls += " : ";
  in_decls += type_name;
  in_decls += " ;\n";
    
  if ((direction() == AST_Argument::dir_IN) || (direction() == AST_Argument::dir_INOUT))
    {
      no_in = false;
      unmarshall += "            UnMarshall(";
      unmarshall += get_ada_local_name ();
      unmarshall += ", Orls) ;\n";
    }

  call_args += ", ";
  call_args += get_ada_local_name ();

  if ((direction() == AST_Argument::dir_OUT)
      || (direction() == AST_Argument::dir_INOUT))
    {
      no_out = false;
      marshall += "            Marshall(";
      marshall += get_ada_local_name ();
      marshall += ", Orls) ;\n";
      align_size += "            Mesg_Size := Align_Size(" ;
      align_size += get_ada_local_name() ;
      align_size += ", Mesg_Size) ;\n" ;
    }      
}

////////////////////////////////////////////////////////////////////////
////////////////     miscellaneous           ///////////////////////////
////////////////////////////////////////////////////////////////////////
IMPL_NARROW_METHODS1(adabe_argument, AST_Argument)
IMPL_NARROW_FROM_DECL(adabe_argument)
  






