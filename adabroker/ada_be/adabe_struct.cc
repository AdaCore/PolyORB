#include <adabe.h>

adabe_structure::adabe_structure(UTL_ScopedName *n, UTL_StrList *p)
	    : AST_Decl(AST_Decl::NT_struct, n, p),
	      UTL_Scope(AST_Decl::NT_struct),
	      adabe_name(AST_Decl::NT_struct, n, p)
{
}

void
adabe_structure::produce_ads(dep_list& with, string &body, string &previous)
{
  with.add ("Ada.Unchecked_Deallocation");
  compute_ada_name();
  body += "   type " + get_ada_local_name() + " is record\n";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_field)
	dynamic_cast<adabe_field *>(d)->produce_ads(with, body, previous);
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in structure");
      i.next();
    }
  body += "   end record;\n";
  body += "   type " + get_ada_local_name() + "_Ptr is access " + get_ada_local_name() + ";\n";
  body += "   procedure Free is new Ada.Unchecked_Deallocation(";
  body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr);\n";  
  set_already_defined();
}

/*
  void
  adabe_structure::produce_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  else return get_ada_full_name();
  }
  void
  adabe_structure::produce_impl_ads(dep_list& with,string &body, string &previous)
  {
  INDENT(body);
  body += "type " + get_ada_local_name() + "is record\n";
  INC_INDENT();
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
  {
  INDENT(body);
  AST_Decl *d = i.item();
  if (d->node_type() == AST_Decl::NT_field)
  dynamic_cast<adabe_name *>(d)->produce_impl_ads(with, body, previous);
  else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in structure");
  i.next();
  }
  DEC_INDENT();
  INDENT(body);
  body += "end record;\n";
  
  }


  void  
  adabe_structure::produce_impl_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  else return get_ada_full_name();
  }
*/

void
adabe_structure::produce_marshal_ads(dep_list &with, string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out Giop_C.Object) ;\n\n";
  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                         S : in out Giop_C.Object) ;\n\n";
  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long ;\n\n\n";

  set_already_defined();
}

void
adabe_structure::produce_marshal_adb(dep_list &with, string &body, string &previous)
{
  string marshall = "";
  string unmarshall = "";
  string align_size = "";
  marshall += "   procedure Marshall(A : in ";
  marshall += get_ada_local_name();
  marshall += " ;\n";
  marshall += "                      S : in out Giop_C.Object) is\n";
  marshall += "   begin\n";
  
  unmarshall += "   procedure UnMarshall(A : out ";
  unmarshall += get_ada_local_name();
  unmarshall += " ;\n";
  unmarshall += "                        S : in out Giop_C.Object) is\n";
  unmarshall += "   begin\n";
  
  align_size += "   function Align_Size (A : in ";
  align_size += get_ada_local_name();
  align_size += " ;\n";
  align_size += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  align_size += "                        N : in Corba.Unsigned_Long := 1)\n";
  align_size += "                        return Corba.Unsigned_Long is\n";
  align_size += "      Tmp : Corba.Unsigned_Long := Initial_Offset ;\n";
  align_size += "   begin\n";
  align_size += "      for I in 1..N loop\n";
  
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_field) {
	dynamic_cast<adabe_field *>(d)->produce_marshal_adb(with, body, marshall, unmarshall, align_size);
      }
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in structure");
      i.next();
    }

  marshall += "   end Marshall;\n\n";
  unmarshall += "   end Unmarshall;\n\n";
  align_size += "      end loop ;\n";
  align_size += "      return Tmp ;\n";
  align_size += "   end Align_Size;\n\n\n";

  body += marshall;
  body += unmarshall;
  body += align_size;

  set_already_defined();
}

string
adabe_structure::dump_name(dep_list& with, string &previous)
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
adabe_structure::marshal_name(dep_list& with, string &previous)
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
IMPL_NARROW_METHODS1(adabe_structure, AST_Structure)
IMPL_NARROW_FROM_DECL(adabe_structure)
IMPL_NARROW_FROM_SCOPE(adabe_structure)





