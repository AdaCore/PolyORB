// File string.cc
#include <adabe.h>
#include <strstream>

IMPL_NARROW_METHODS1(adabe_string, AST_String);
IMPL_NARROW_FROM_DECL(adabe_string);

static string to_string(AST_Expression::AST_ExprValue *exp)
{
  char temp[10]; 
  switch( exp->et ) {
  case AST_Expression::EV_short:
    sprintf(temp, "%d",exp->u.sval);
    break;
  case AST_Expression::EV_ushort:
    sprintf(temp, "%d",exp->u.usval);
    break;
  case AST_Expression::EV_long:
    sprintf(temp, "%ld",exp->u.lval);
    break;
  case AST_Expression::EV_ulong:
    sprintf(temp, "%ld",exp->u.ulval);
    break;
  default:
    throw adabe_internal_error (__FILE__,__LINE__,"Unexpected string dimension type");
  }
  //  temp.freeze(temp.pcount());
  return temp;
}

static int evaluate (AST_Expression::AST_ExprValue *exp)
{
  switch( exp->et ) {
  case AST_Expression::EV_short:
    return exp->u.sval;
    break;
  case AST_Expression::EV_ushort:
    return exp->u.usval;
    break;
  case AST_Expression::EV_long:
    return exp->u.lval;
    break;
  case AST_Expression::EV_ulong:
    return exp->u.ulval;
    break;
  default:
    throw adabe_internal_error (__FILE__,__LINE__,"Unexpected string dimension type");
  }
}

adabe_string::adabe_string(AST_Expression *v):
  AST_String(v),
  AST_Decl(AST_Decl::NT_string, new UTL_ScopedName(new Identifier("string", 1, 0, I_FALSE), NULL), NULL),
  adabe_name(AST_Decl::NT_string,new UTL_ScopedName(new Identifier("string",1,0,I_FALSE), NULL), NULL)
{
}

adabe_string::adabe_string(AST_Expression *v, long wide):
  AST_String(v,wide),
  AST_Decl(AST_Decl::NT_string, new UTL_ScopedName(new Identifier("string", 1, 0, I_FALSE), NULL), NULL),
  adabe_name(AST_Decl::NT_string,new UTL_ScopedName(new Identifier("string",1,0,I_FALSE),NULL),NULL)
{
}

string
adabe_string::local_type()
{
  bool find = false;
  UTL_Scope *parent_scope = defined_in();
  UTL_ScopeActiveIterator parent_scope_activator(parent_scope,UTL_Scope::IK_decls);
  adabe_name *decl = dynamic_cast<adabe_name *>(parent_scope_activator.item());
  do
    {
      switch (decl->node_type())
	{
	case AST_Decl::NT_field:
	case AST_Decl::NT_argument:
	  if (dynamic_cast<AST_Field *>(decl)->field_type() == this)
	    find = true;
	  break;
	case AST_Decl::NT_op:
	  if (dynamic_cast<AST_Operation *>(decl)->return_type() == this)
	    find =true;
	  break;
	default:
	  break;
	}
      parent_scope_activator.next();
      if (!find)
	decl = dynamic_cast<adabe_name *>(parent_scope_activator.item());
    }
  while (!find && !(parent_scope_activator.is_done()));
  if (find)
    return decl->get_ada_local_name() +"_String";

  return "local_type";
}

void adabe_string::produce_ads (dep_list &with,string &body, string &previous)
{
  no_fixed_size();
  // set a flag of this object and its ancestors saying
  // they have not a fixed size.
  
  if (evaluate(max_size()->ev())==0)
    {
      body+= "   type " + get_ada_local_name() + " is new CORBA.String ;\n";
    }
  else
    {
      
      body += "   package CORBA.Bounded_String_" + to_string(max_size()->ev());
      body += " is\n\t\t new CORBA.Bounded_String(";
      body +=  to_string(max_size()->ev());
      body += ") ;\n\n";
      body += "   type "+ get_ada_local_name() + " is\n\t\t new CORBA.Bounded_String_";
      body += to_string(max_size()->ev());
      body += ".Bounded_String ;\n\n";
    }

  set_already_defined();
}


void  
adabe_string::produce_marshal_ads(dep_list& with, string &body, string &previous)
{
  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out Giop_C.Object) ;\n\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out Giop_C.Object) ;\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long ;\n\n\n";

  set_already_defined();
}


void 
adabe_string::produce_marshal_adb(dep_list& with, string &body, string &previous)
{
  string tmp="";

  body += "   procedure Marshall (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                       S : in out Giop_C.Object) is\n";
  body += "   begin\n";
  body += "      Marshall (Corba.String(A), S) ;\n";
  body += "   end Marshall ;\n\n";

  body += "   procedure UnMarshall (A : out ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                         S : in out Giop_C.Object) is \n\n";
  body += "   begin\n";
  body += "      UnMarshall (Corba.String(A), S) ;\n";
  body += "   end UnMarshall ;\n\n";

  body += "   function Align_Size (A : in ";
  body += get_ada_local_name();
  body += " ;\n";
  body += "                        Initial_Offset : in Corba.Unsigned_Long ;\n";
  body += "                        N : in Corba.Unsigned_Long := 1)\n";
  body += "                        return Corba.Unsigned_Long is \n";
  body += "   begin\n";
  body += "      return Align_Size (Corba.String(A), Initial_Offset, N) ;\n";
  body += "   end Align_Size ;\n\n\n";

  set_already_defined();
}

string 
adabe_string::dump_name (dep_list &with, string &previous)
{
  UTL_Scope *temp = defined_in();
  if (!is_imported(with))
    {
      if (evaluate(max_size()->ev())==0)
	{
	  if ((defined_in() == NULL) || (dynamic_cast<AST_Decl *> (defined_in())->node_type() 
					 != AST_Decl::NT_typedef))
	    {
	      set_already_defined();
	      with.check("CORBA.String");
	      return "CORBA.String";
	    }
	}
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

string adabe_string::marshal_name (dep_list &with, string &previous)
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
