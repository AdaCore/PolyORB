#include <adabe.h>

adabe_argument::adabe_argument(AST_Argument::Direction d, AST_Type *ft, UTL_ScopedName *n,UTL_StrList *p)
	   : AST_Argument(d, ft, n, p),
	     AST_Field(AST_Decl::NT_argument, ft, n, p),
	     AST_Decl(AST_Decl::NT_argument, n, p),
             adabe_name(AST_Decl::NT_argument, n, p)
{
}

void
adabe_argument::produce_ads(dep_list& with, string &body, string &previous)
{
  compute_ada_name();
  body += get_ada_local_name() + " : ";
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
  AST_Decl *d = field_type();
  body +=  dynamic_cast<adabe_name *>(d)->dump_name(with, previous); // virtual method
}

void
adabe_argument::produce_adb(dep_list& with, bool &no_out, string space,
			    string &in_decls, string &in_args, string &out_args)
{
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
  AST_Decl *d = field_type();
  string previous = "";
  string name = dynamic_cast<adabe_name *>(d)->dump_name(with, previous); // virtual method

  in_decls += ";\n              " + space + get_ada_local_name() + " : " + dir_st +  name;
  
  if ((dir == AST_Argument::dir_IN) || (dir == AST_Argument::dir_INOUT))
    in_args += ", " + get_ada_local_name ();

  if ((dir == AST_Argument::dir_OUT) || (dir == AST_Argument::dir_INOUT))
    {
      no_out = false;
      out_args += ", " + get_ada_local_name ();
    }
}

/*
  void
  adabe_argument::produce_adb(dep_list& with,string &body, string &previous)
  {
  produce_ads(with, body, previous);
  }

  void
  adabe_argument::produce_impl_ads(dep_list& with,string &body, string &previous)
  {
  produce_ads( with, body, previous); 
  }
  
  ///////////////perhaps useless////////////////////////
  void
  adabe_argument::produce_impl_adb(dep_list& with,string &body, string &previous)
  {
  produce_ads(with, body, previous);
  }
*/



void 
adabe_argument::produce_proxies_ads(dep_list &with, string &in_decls, bool &no_in, bool &no_out, string &fields, string &out_args)
{
  string body, previous = "";
  AST_Decl *d = field_type();
  adabe_name *type_adabe_name = dynamic_cast<adabe_name *>(d) ;
  string type_name = type_adabe_name->dump_name(with, previous);
  string full_type_name = type_adabe_name->get_ada_full_name() ;

  if ((direction() == AST_Argument::dir_IN) || (direction() == AST_Argument::dir_INOUT)) {
    no_in = false;
    in_decls += " ;\n                  ";
    in_decls += get_ada_local_name ();
    in_decls += " : in ";
    in_decls += type_name;
  }
  if ((direction() == AST_Argument::dir_OUT) || (direction() == AST_Argument::dir_INOUT)) {
    no_out = false;
    out_args += "; " + get_ada_local_name () + " : out " + type_name;
  }
  fields += "      ";
  fields += get_ada_local_name ();
  fields += " : ";
  fields += type_name;
  fields += "_Ptr := null ;\n";
}

void 
adabe_argument::produce_proxies_adb(dep_list &with, string &in_decls, bool &no_in, bool &no_out, string &init,
                                    string &align_size, string&marshall, string &unmarshall_decls, string &unmarshall,
                                    string &finalize, string &out_args, string &result_decls)
{
  string body, previous = "";
  AST_Decl *d = field_type();
  string type_name =  dynamic_cast<adabe_name *>(d)->dump_name(with, previous);
  string full_type_name = dynamic_cast<adabe_name *>(d)->get_ada_full_name() ;
  
  if ((direction() == AST_Argument::dir_IN) || (direction() == AST_Argument::dir_INOUT)) {
    no_in = false;
    in_decls += " ;\n                  ";
    in_decls += get_ada_local_name ();
    in_decls += " : in ";
    in_decls += type_name;

    init += "      Self.";
    init += get_ada_local_name ();
    init += " := new ";
    init += type_name;
    init += "'(";
    init += get_ada_local_name ();
    init += ") ;\n";

    align_size += "      Tmp := Align_size(Self.";
    align_size += get_ada_local_name ();
    align_size += ".all, Tmp) ;\n";

    marshall += "      Marshall(Self.";
    marshall += get_ada_local_name ();
    marshall += ".all,Giop_Client) ;\n";
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
      unmarshall += "      Free (Self.";
      unmarshall += get_ada_local_name ();
      unmarshall += ") ;\n";
}
    unmarshall += "      Unmarshall(";
    unmarshall += get_ada_local_name ();
    unmarshall += ",Giop_Client) ;\n";
    unmarshall += "      Self.";
    unmarshall += get_ada_local_name ();
    unmarshall += " := new ";
    unmarshall += type_name;
    unmarshall += "'(";
    unmarshall += get_ada_local_name ();
    unmarshall += ") ;\n";
    
    out_args += "; " + get_ada_local_name () + " : out " + type_name;
    
    result_decls += "      " + get_ada_local_name ();
    result_decls += " := Self." + get_ada_local_name () + ".all ;\n";
  }
  
  finalize += "      Free(Self.";
  finalize += get_ada_local_name ();
  finalize += ") ;\n";
}

void
adabe_argument::produce_skel_adb(dep_list &with, string &in_decls ,
				 bool &no_in, bool no_out,
				 string &unmarshall, string &call_args,
				 string &marshall)
{
  string previous = "";
  AST_Decl *d = field_type();
  adabe_name *e = dynamic_cast<adabe_name *>(d);
  string type_name = e->dump_name(with, previous);

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

  if ((direction() == AST_Argument::dir_OUT) || (direction() == AST_Argument::dir_INOUT))
    {
      no_out = false;
      marshall += "            Marshall(";
      marshall += get_ada_local_name ();
      marshall += ", Orls) ;\n";
    }      
}

IMPL_NARROW_METHODS1(adabe_argument, AST_Argument)
IMPL_NARROW_FROM_DECL(adabe_argument)
  






