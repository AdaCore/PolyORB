#include <adabe.h>

adabe_union::adabe_union(AST_ConcreteType *dt, UTL_ScopedName *n, UTL_StrList *p)
	: AST_Union(dt, n, p),
	  AST_Decl(AST_Decl::NT_union, n, p),
          AST_Structure(AST_Decl::NT_union, n, p),
	  UTL_Scope(AST_Decl::NT_union),
	  adabe_name(AST_Decl::NT_union, n, p)
{
}

void
adabe_union::produce_ads(dep_list& with, string &body, string &previous)
{
  ///////////WARNING//////////////
  //  the type of the discriminant should be check. From this type result a specific solution  

  no_fixed_size();
  // set a flag of this object and its ancestors saying
  // they have not a fixed size.
  
  compute_ada_name();
  body += "   type " + get_ada_local_name();
  adabe_name *b = dynamic_cast<adabe_name *>(disc_type());
  b->compute_ada_name();
  string name = b->dump_name(with, previous);
  body += "(Switch : "  + name;
  body += " := " + name + "'first) is record\n";
  body += "      case Switch is\n";
  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_union_branch)
	//adabe_union_branch::narrow_from_decl(d)->produce_ads(with, body, previous, disc_type());
	dynamic_cast<adabe_union_branch *>(d)->produce_ads(with, body, previous, disc_type()); 
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in union");
      i.next ();
    }
  body += "      end case ;\n";
  body += "   end record ;\n";
  body += "   type " + get_ada_local_name() + "_Ptr is access ";
  body += get_ada_local_name() + " ;\n\n";
  body += "   procedure Free is new Ada.Unchecked_Deallocation(";
  body += get_ada_local_name() + ", " + get_ada_local_name ()+ "_Ptr) ;\n\n\n";  
  set_already_defined();
}

/*
  void
  adabe_union::produce_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  return get_ada_full_name();	   
  }
  
  void
  adabe_union::produce_impl_ads(dep_list& with,string &body, string &previous)
  {
  produce_ads(with, body, previous);
  }
  
  void
  adabe_union::produce_impl_adb(dep_list& with,string &body, string &previous)
  {
  if (!is_imported(with)) return get_ada_local_name();
  return get_ada_full_name();	   
  }
*/

void
adabe_union::produce_marshal_ads(dep_list& with, string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out Netbufferedstream.Object'Class) ;\n\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                         S : in out Netbufferedstream.Object'Class) ;\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long)\n";
  body += "                        return Corba.Unsigned_Long ;\n\n\n";

  set_already_defined ();
}

void
adabe_union::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  string disc_name = (dynamic_cast<adabe_name *>(disc_type()))->marshal_name(with, previous); 

  string marshall = "";
  string unmarshall = "";
  string align_size = "";
  marshall += "   procedure Marshall(A : in ";
  marshall += get_ada_local_name();
  marshall += " ;\n";
  marshall += "                      S : in out Netbufferedstream.Object'Class) is\n";
  marshall += "   begin\n";
  marshall += "      Marshall (A.Switch,S) ;\n";
  marshall += "      case A.Switch is\n";
  
  unmarshall += "   procedure UnMarshall(A : out ";
  unmarshall += get_ada_local_name();
  unmarshall += " ;\n";
  unmarshall += "                        S : in out Netbufferedstream.Object'Class) is\n";
  unmarshall += "      Switch : ";
  unmarshall += disc_name;
  unmarshall += " ;\n";
  unmarshall += "   begin\n";
  unmarshall += "      UnMarshall (Switch,S) ;\n";
  unmarshall += "      declare\n";
  unmarshall += "         Tmp : ";
  unmarshall += get_ada_local_name ();
  unmarshall += "(Switch) ;\n";
  unmarshall += "      begin\n";
  unmarshall += "         case Switch is\n";
  
  align_size += "   function Align_Size (A : in ";
  align_size += get_ada_local_name();
  align_size += " ;\n";
  align_size += "                        Initial_Offset : in Corba.Unsigned_Long)\n";
  align_size += "                        return Corba.Unsigned_Long is\n";
  align_size += "      Tmp : Corba.Unsigned_Long := 0 ;\n";
  align_size += "   begin\n";
  align_size += "      Tmp := Align_Size (A.Switch,Initial_Offset) ;\n";
  align_size += "      case A.Switch is\n";

  UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl *d = i.item();
      if (d->node_type() == AST_Decl::NT_union_branch) {
	dynamic_cast<adabe_union_branch *>(d)->produce_marshal_adb(with,marshall, unmarshall, align_size, disc_type());
      }
      else throw adabe_internal_error(__FILE__,__LINE__,"Unexpected node in union");
      i.next();
    }

  marshall += "      end case ;\n";
  marshall += "   end Marshall ;\n\n";

  unmarshall += "         end case ;\n";
  unmarshall += "      A := Tmp ;\n";
  unmarshall += "      end ;\n";
  unmarshall += "   end Unmarshall ;\n\n";
  
  align_size += "      end case ;\n";
  align_size += "      return Tmp ;\n";
  align_size += "   end Align_Size ;\n\n\n";

  body += marshall;
  body += unmarshall;
  body += align_size;

  set_already_defined();
}

string
adabe_union::dump_name(dep_list& with, string &previous)
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
adabe_union::marshal_name(dep_list& with, string &previous)
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

IMPL_NARROW_METHODS1(adabe_union, AST_Union)
IMPL_NARROW_FROM_DECL(adabe_union)
IMPL_NARROW_FROM_SCOPE(adabe_union)
