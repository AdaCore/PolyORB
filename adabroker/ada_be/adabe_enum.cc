//file adabe_enum.cc

#include <adabe.h>

  
IMPL_NARROW_METHODS1(adabe_enum, AST_Enum);
IMPL_NARROW_FROM_DECL(adabe_enum);
IMPL_NARROW_FROM_SCOPE(adabe_enum);


adabe_enum::adabe_enum(UTL_ScopedName *n, UTL_StrList *p)
       : AST_Enum(n, p),
	 AST_Decl(AST_Decl::NT_enum, n, p),
	 UTL_Scope(AST_Decl::NT_enum),
	 adabe_name(AST_Decl::NT_enum, n, p)
{
}

void
adabe_enum::produce_ads(dep_list& with,string &body, string &previous) {
  
  compute_ada_name ();
  body += "   type " + get_ada_local_name() + " is ( ";
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  while (!activator.is_done())
    {
      AST_Decl *d = activator.item();
      activator.next();
      switch (d->node_type())
	{
	case AST_Decl::NT_enum_val:
	  body+=adabe_enum_val::narrow_from_decl(d)->dump_name(with, previous);
	  break;
	default:
	  throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope in enumeration type");
	}
      if (!activator.is_done()) body += ", ";
    }
  body +=" ) ;\n";
  body += "   type " + get_ada_local_name() + "_Ptr is access ";
  body += get_ada_local_name() + " ;\n\n";
  body += "   procedure Free is new Ada.Unchecked_Deallocation(";
  body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr) ;\n\n\n";  
  set_already_defined();
}

void  
adabe_enum::produce_marshal_ads(dep_list& with, string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out Netbufferedstream.Object'Class) ;\n\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out Netbufferedstream.Object'Class) ;\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long ;\n\n\n";

  set_already_defined();
}


void 
adabe_enum::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out Netbufferedstream.Object'Class) is\n";
  body += "   begin\n";
  body += "      Marshall (Corba.Unsigned_Long("+get_ada_local_name()+"'Pos(A)), S) ;\n";
  body += "   end Marshall ;\n\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                         S : in out Netbufferedstream.Object'Class) is \n";
  body += "      Tmp : Corba.Unsigned_Long ;\n";
  body += "   begin\n";
  body += "      UnMarshall (Tmp,S) ;\n";
  body += "      A := ";
  body += get_ada_local_name();
  body += "'Val(Tmp) ;\n";
  body += "   end UnMarshall ;\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long is\n";
  body += "   begin\n";
  body += "      return Align_Size (Corba.Unsigned_Long(0), Initial_Offset ,N) ;\n";
  body += "   end Align_Size ;\n\n\n";

  set_already_defined();
}

string
adabe_enum::dump_name(dep_list& with, string &previous) 
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
adabe_enum::marshal_name(dep_list& with, string &previous) 
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



