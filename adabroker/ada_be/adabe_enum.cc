//file adabe_enum.cc

#include <adabe.h>

  
IMPL_NARROW_METHODS1(adabe_enum, AST_Enum);
IMPL_NARROW_FROM_DECL(adabe_enum);
IMPL_NARROW_FROM_SCOPE(adabe_enum);


adabe_enum::adabe_enum(UTL_ScopedName *n, UTL_StrList *p)
       : AST_Enum(n, p),
	 AST_Decl(AST_Decl::NT_enum, n, p),
	 UTL_Scope(AST_Decl::NT_enum),
	 adabe_name()
{
}

void
adabe_enum::produce_ads(dep_list with,string &body, string &previous) {
  
  body += "type " + get_ada_local_name() + "is (\n";
  UTL_ScopeActiveIterator activator(this,UTL_Scope::IK_decls);
  while (!activator.is_done())
    {
      AST_Decl *d = activator.item();
      activator.next();
      switch (d->node_type())
	{
	case AST_Decl::NT_enum_val:
	  body+=adabe_enum_val::narrow_from_decl(d)->dump_name(with, body, previous);
	  break;
	default:
	  throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope in enumeration type");
	}
      if (!activator.is_done()) body += ",";
    }
  body +=");\n";
  set_already_defined();
}

void  
adabe_enum::produce_marshal_ads(dep_list with, string &body, string &previous)
{
  string tmp="";

  tmp+="procedure Marshall (A : in ";
  tmp+= get_ada_local_name();         // question ...N' est-ce ada_global_name
  tmp+=";S : in out Object'Class) ; \n"
  //suggest also to  include a comment like:  Marshalls a enum  into a netbufferedstream object
  
  body+=tmp;
}

void 
adabe_enum::produce_marshal_adb(dep_list with, string &body, string &previous)
{
  string tmp="";

  tmp+="procedure UnMarshall (A : out ";
  tmp+= get_ada_local_name();
  tmp+=";S : in out Object'Class) ; \n";
  body+=tmp;
}

string
adabe_enum::dump_name(dep_list with,string &body, string &previous) {
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
