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
  body += get_ada_local_name() + " :";
  switch (direction())
    {
    case dir_IN :
      body += " in ";
      break;
    case dir_OUT :
      body += " out ";
      break;
    case dir_INOUT :
      body += " inout ";
      break;
    }
  AST_Decl *d = field_type();
  body +=  dynamic_cast<adabe_name *>(d)->dump_name(with, body, previous); // virtual method
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
adabe_argument::produce_proxies_ads(dep_list &with, string &in_decls, bool &no_in, bool &no_out, string &fields)
{
  string body, previous = "";
  AST_Decl *d = field_type();
  string name = (dynamic_cast<adabe_name *>(d))->dump_name(with, body, previous);
  if ((direction() == dir_IN) | (direction() == dir_INOUT)) {
    no_in = false;
    in_decls += " ;\n                  ";
    in_decls += get_ada_local_name ();
    in_decls += " : in ";
    in_decls += name;
  }
  if ((direction() == dir_OUT) | (direction() == dir_INOUT)) {
    no_out = false;
  }
  fields += "      ";
  fields += get_ada_local_name ();
  fields += " : ";
  fields += name;
  fields += "_Ptr := null ;\n";
}

void 
adabe_argument::produce_proxies_adb(dep_list &with, string &in_decls, bool &no_in, bool &no_out, string &init,
				    string &align_size, string &marshall, string &unmarshall_decls, string &unmarshall, string &finalize)
{
  string body, previous = "";
  AST_Decl *d = field_type();
  string name =  dynamic_cast<adabe_name *>(d)->dump_name(with, body, previous);
  if ((direction() == dir_IN) | (direction() == dir_INOUT)) {
    no_in = false;
    in_decls += " ;\n                  ";
    in_decls += get_ada_local_name ();
    in_decls += " : in ";
    in_decls += name;

    init += "      Self.";
    init += get_ada_local_name ();
    init += " := new ";
    init += name;
    init += "'(";
    init += get_ada_local_name ();
    init += ") ;\n";

    align_size += "      Align_size(Self.";
    align_size += get_ada_local_name ();
    align_size += ".all, tmp) ;\n";

    marshall += "      Marshall(Self.";
    marshall += get_ada_local_name ();
    marshall += ".all,Giop_Client) ;\n";
  }
  if ((direction() == dir_OUT) | (direction() == dir_INOUT)) {
    no_out = false;
    unmarshall_decls += "      ";
    unmarshall_decls += get_ada_local_name ();
    unmarshall_decls += " : ";
    unmarshall_decls += name;
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
    unmarshall += " := new";
    unmarshall += name;
    unmarshall += "'(";
    unmarshall += get_ada_local_name ();
    unmarshall += ") ;\n";
  }

  finalize += "      Free (Self.";
  finalize += get_ada_local_name ();
  finalize += ") ;\n";
}


IMPL_NARROW_METHODS1(adabe_argument, AST_Argument)
IMPL_NARROW_FROM_DECL(adabe_argument)
  






