#include <adabe.h>
#include <stdio.h>
#include <nr_extern.hh>

adabe_name::adabe_name(AST_Decl::NodeType t,UTL_ScopedName* n, UTL_StrList* up)
  : AST_Decl(t,n,up)
{
  pd_ada_local_name = "";
  pd_ada_full_name  = "";
  pd_defined_type   = 0;
};

string
adabe_name::get_ada_local_name()
{
  return pd_ada_local_name;
} 

string
adabe_name::get_ada_full_name()
{
  return pd_ada_full_name;
}

void
adabe_name::set_ada_local_name(string s)
{
  pd_ada_local_name = s;
} 

void
adabe_name::set_ada_full_name(string s)
{
  pd_ada_full_name = s;
}

void 
adabe_name::produce_ads(dep_list&, string&, string&)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Produce called in adabe_name");
}

void 
adabe_name::produce_adb(dep_list&, string&, string&)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Produce called in adabe_name");
}

void 
adabe_name::produce_impl_ads(dep_list&, string&, string&)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Produce called in adabe_name");
}

void 
adabe_name::produce_impl_adb(dep_list&, string&, string&)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Produce called in adabe_name");
}

void 
adabe_name::produce_proxies_ads(dep_list&, string &in_decls, boolean &no_in, boolean &no_out, string &fields)
{
  if (/* in */) {
    no_in := false;
    in_decls += " ;\n                  ";
    in_decls += get_ada_name ();
    in_decls += " : in ";
    in_decls += name;
  }
  if (/* out */) {
    no_out := false;
  }
  fields += "      ";
  fields += get_ada_name ();
  fields += " : ";
  fields += name;
  fields += "_Ptr := null ;\n";
}

void 
adabe_name::produce_proxies_adb(dep_list&, string &in_decls, boolean &no_in, boolean &no_out, string &init,
				string &align_size, string &marshall, string &unmarshall_decls, string &unmarshall, string &finalize)
{
  if (/* in */) {
    no_in := false;
    in_decls += " ;\n                  ";
    in_decls += get_ada_name ();
    in_decls += " : in ";
    in_decls += name;

    init += "      Self.";
    init += get_ada_name ();
    init += " := new ";
    init += name;
    init += "'(";
    init += get_ada_name ();
    init += ") ;\n";

    align_size += "      Align_size(Self.";
    align_size += get_ada_name ();
    align_size += ".all, tmp) ;\n";

    marshall += "      Marshall(Self.";
    marshall += get_ada_name;
    marshall += ".all,Giop_Client) ;\n";
  }
  if (/* out */) {
    no_out := false;
    unmarshall_decls += "      ";
    unmarshall_decls += get_ada_name;
    unmarshall_decls += " : ";
    unmarshall_decls += name;
    unmarshall_decls += " ;\n";

    if (/* in */) {
      unmarshall += "      Free (Self.";
      unmarshall += get_ada_name;
      unmarshall += ") ;\n";
    }
    unmarshall += "      Unmarshall(";
    unmarshall += get_ada_name;
    unmarshall += ",Giop_Client) ;\n";
    unmarshall += "      Self.";
    unmarshall += get_ada_name;
    unmarshall += " := new";
    unmarshall += name;
    unmarshall += "'(";
    unmarshall += get_ada_name;
    unmarshall += ") ;\n";
  }

  finalize += "      Free (Self.";
  finalize += get_ada_name;
  finalize += ") ;\n";
}

void 
adabe_name::produce_skel_ads(dep_list&, string&, string&)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Produce called in adabe_name");
}

void 
adabe_name::produce_skel_adb(dep_list&, string&, string&)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Produce called in adabe_name");
}

void 
adabe_name::produce_marshal_ads(dep_list&, string&, string&)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Produce called in adabe_name");
}

void 
adabe_name::produce_marshal_adb(dep_list&, string&, string&)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Produce called in adabe_name");
}

string 
adabe_name::dump_name(dep_list&, string&, string&)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Produce called in adabe_name");
}


void
adabe_name::compute_ada_name()
{
  int already_used;
  int loop = 0;
  string temp_name = local_name()->get_string();
#ifdef DEBUG_NAME
  cout << "in adabe_name, befor convert" << endl;
#endif
  convert(temp_name);
  // which must be the ada_name ?
#ifdef DEBUG_NAME
  cout << "in adabe_name, after convert" << endl;
  cout << temp_name << endl;
#endif
  
  pd_ada_local_name = temp_name;
  UTL_Scope *parent_scope = defined_in();
  if (parent_scope != NULL) do {
    // is this name already used ?
#ifdef DEBUG_NAME
    cout << "in adabe_name, node type of the parent in compute :" <<  parent_scope->scope_node_type() << endl;
#endif
    switch (parent_scope->scope_node_type())
      {
      case AST_Decl::NT_op:
      case AST_Decl::NT_interface:
      case AST_Decl::NT_module:
      case AST_Decl::NT_root:
      case AST_Decl::NT_except:
      case AST_Decl::NT_struct:
      case AST_Decl::NT_union:
	already_used = (dynamic_cast<adabe_name *>(parent_scope))->is_name_already_used(pd_ada_local_name, parent_scope);
	pd_ada_full_name =  (dynamic_cast<adabe_name *>(parent_scope))->get_ada_full_name();
	break;
      default:
	throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope");
      }

#ifdef DEBUG_NAME
  cout << "In adabe_name, in compute_ada_name after the switch\n";
#endif
    if (already_used == 1)
      {
	if (loop>999)
	  throw adabe_internal_error(__FILE__,__LINE__,"too many name conflicts");
	cout << "Je suis passe ici\n";
	char extension[4];
	sprintf (extension, "_%d",loop++);
	//      delete pd_ada_local_name;
	//      pd_ada_local_name = new string();
	pd_ada_local_name = temp_name + extension;
	pd_ada_full_name = pd_ada_full_name + "." + pd_ada_local_name;
      }
    /*  try to go the to the root of teh tree, and, each step, try to find 
	a node with the same name. If such a node if found
	try with another name */
  }
  while (already_used == 1);
#ifdef DEBUG_NAME
  cout << "End of compute_ada_name\n";
#endif
}


int
adabe_name::is_name_already_used(string name, UTL_Scope *in_scope)
{
#ifdef DEBUG_NAME
  cout << "in adabe_name, called on an "  << endl;
#endif
  UTL_ScopeActiveIterator i(in_scope ,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      string temp;
      AST_Decl *d = i.item();
#ifdef DEBUG_NAME
      if (d->in_main_file())
	cout << "in adabe_name, in is_name_already_used before the first scope :" << d->node_type() <<endl;
#endif
      i.next();
      if (d->in_main_file())
	{
	   adabe_name *ada = dynamic_cast<adabe_name *>(d);;

	   // adabe_name *ada = adabe_interface::narrow_from_decl(d);
#ifdef DEBUG_NAME
	   cout << "in adabe_name, in is_name_already_used, after the narrow_from_decl"  << endl;
#endif
	   temp = (ada->get_ada_local_name());
#ifdef DEBUG_NAME
	   cout << "in adabe_name, in is_name_already_used, after get_ada_local_name"  << endl;
	   cout << "this vaut " << (int) this << "et ada vaut " << (int) ada << endl;
#endif
	  
	  if ((this != ada) && (temp  != "") && (temp == name)) return 1;
	}
    }
#ifdef DEBUG_NAME
  cout << "in adabe_name, in is_name_already_used, after the first scope" << endl;
#endif

  /*
  bool already_used = false;
  UTL_Scope *parent_scope = defined_in();
  if (parent_scope != NULL)
    switch (parent_scope->scope_node_type())
      {
      case AST_Decl::NT_interface:
	already_used = (adabe_interface::narrow_from_scope(parent_scope))->is_name_already_used(name);
	break;
      case AST_Decl::NT_module:
	already_used = (adabe_module::narrow_from_scope(parent_scope))->is_name_already_used(name);
	break;
      case AST_Decl::NT_root:
	already_used = (adabe_root::narrow_from_scope(parent_scope))->is_name_already_used(name);
	break;
      case AST_Decl::NT_except:
	already_used = (adabe_exception::narrow_from_scope(parent_scope))->is_name_already_used(name);
	break;
      case AST_Decl::NT_struct:
	already_used = (adabe_structure::narrow_from_scope(parent_scope))->is_name_already_used(name);      
	break;
      case AST_Decl::NT_union:
	already_used = (adabe_union::narrow_from_scope(parent_scope))->is_name_already_used(name);
	break;
      default:
	throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope");
      }    
  */
  
  return 0;
} 
 
bool
adabe_name::is_already_defined()
{
  return pd_defined_type;
}

void
adabe_name::set_already_defined()
{
  pd_defined_type = true;
}

void
adabe_name::convert(string &name)
{
  if ((name == "abort") || (name == "abs") ||
      (name == "abstract") || (name == "accept") ||
      (name == "access") || (name == "aliased") ||
      (name == "all") || (name == "and") ||
      (name == "array") || (name == "at") ||
      (name == "begin") || (name =="body") ||
      (name == "case") || (name =="constant") ||
      (name == "declare") || (name == "delay") || 
      (name == "delta") || (name == "digits") || 
      (name == "do") || (name == "else") || 
      (name == "elsif") || (name == "end") || 
      (name == "entry") || (name == "exception") || 
      (name == "exit") || (name == "for") || 
      (name == "function") || (name == "generic") || 
      (name == "goto") || (name == "if") || 
      (name == "in") || (name == "is") || 
      (name == "limited") || (name == "loop") || 
      (name == "mod") || (name == "new") || 
      (name == "not") || (name == "null") || 
      (name == "of") || (name == "or") || 
      (name == "others") || (name == "out") || 
      (name == "package") || (name == "pragma") || 
      (name == "private") || (name == "procedure") || 
      (name == "protected") || (name == "raise") || 
      (name == "range") || (name == "record") || 
      (name == "rem") || (name == "renames") || 
      (name == "requeue") || (name == "return") || 
      (name == "reverse") || (name == "select") || 
      (name == "separate") || (name == "subtype") || 
      (name == "tagged") || (name == "task") || 
      (name == "terminate") || (name == "then") || 
      (name == "type") || (name == "until") || 
      (name == "use") || (name == "when") || 
      (name == "while") || (name == "with") || 
      (name == "xor"))
    name +="_IDL";
  // changes the name if the name is an ADA95 Keyword
  
  while (name[0]=='_')
    {
      string temp = name.substr(1,name.length());
      name = temp;
    }
  int c;
  while ((c = name.find("__")) != -1) 
    name[c+1]='U';
  // Eliminates the '__'
};

void
adabe_name::set_undefined()
{
  pd_defined_type = 0;
#ifdef DEBUG_NAME
  cout << "set_undifined  node_type : " << node_type() << endl;
#endif

  switch (node_type())
    {
      // if it's a complexe type does the same to each type in the node ...
    case AST_Decl::NT_op:
      {
	UTL_ScopeActiveIterator i(adabe_operation::narrow_from_decl(this), UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl *d = i.item();
	    i.next();
	    dynamic_cast<adabe_name *>(d)->set_undefined();
	  };
      }
      break;
    case AST_Decl::NT_interface:
      {
	UTL_ScopeActiveIterator i(adabe_interface::narrow_from_decl(this), UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl *d = i.item();
	    i.next();
	    dynamic_cast<adabe_name *>(d)->set_undefined();
	  };
      }
      break;
    case AST_Decl::NT_module:
      {
	UTL_ScopeActiveIterator i(adabe_module::narrow_from_decl(this), UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl *d = i.item();
	    i.next();
	    dynamic_cast<adabe_name *>(d)->set_undefined();
	  };
      }
      break;
      
    case AST_Decl::NT_root:
      {
	UTL_ScopeActiveIterator i(adabe_root::narrow_from_decl(this), UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl *d = i.item();
	    i.next();
	    if (d->in_main_file())
	      {
		dynamic_cast<adabe_name *>(d)->set_undefined();
	      }
	  };
      }
      break;
    case AST_Decl::NT_except:
      {
	UTL_ScopeActiveIterator i(adabe_exception::narrow_from_decl(this), UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl *d = i.item();
	    i.next();
	    dynamic_cast<adabe_name *>(d)->set_undefined();
	  };
      }
      break;
    case AST_Decl::NT_struct:
      {
	UTL_ScopeActiveIterator i(adabe_structure::narrow_from_decl(this),UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl *d = i.item();
	    i.next();
	    dynamic_cast<adabe_name *>(d)->set_undefined();
	  };
      }
      break;
    case AST_Decl::NT_union:
      {
	UTL_ScopeActiveIterator i(adabe_union::narrow_from_decl(this), UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl *d = i.item();
	    i.next();
	    dynamic_cast<adabe_name *>(d)->set_undefined();
	  };
      }
      break;
    default:
      //...  else do nothing
      break;
    }
}

bool
adabe_name::is_imported (dep_list& with)
{
  AST_Decl::NodeType NT = node_type();
  if (NT == AST_Decl::NT_interface)
    {
      bool temp;
      temp = with.check (get_ada_full_name());
      if (!temp) with.add (get_ada_full_name());
      return temp;
    }
  if (NT == AST_Decl::NT_module)
    {
      bool temp;
      temp = with.check (get_ada_full_name());
      if (!temp) with.add (get_ada_full_name());
      return temp;
    }
  if  ((NT == AST_Decl::NT_root)
       || (NT ==  AST_Decl::NT_except)
       || (NT ==  AST_Decl::NT_struct)
       || (NT ==  AST_Decl::NT_union))
					  
    {
      (dynamic_cast<adabe_name *>(defined_in()))->is_imported (with); 
    }
}


ostream& operator<<(ostream &s, AST_Decl::NodeType x)
{
  switch (x)
    {
    case  AST_Decl::NT_module:
      s << "module";
      break;
    case AST_Decl::NT_root:	
      s << "root";
      break;
    case AST_Decl::NT_interface:
      s << "interface";
      break;
    case AST_Decl::NT_interface_fwd:
      s << "interface fwd";
      break;
    case AST_Decl::NT_const:		
      s << "constant";
      break;
    case AST_Decl::NT_except:
      s << "exception";
      break;
    case AST_Decl::NT_attr:
      s << "attribute";
      break;
    case AST_Decl::NT_op:
      s << "operation";
      break;
    case AST_Decl::NT_argument:
      s << "argument";
      break;
    case AST_Decl::NT_union:
      s << "union";
      break;
    case AST_Decl::NT_union_branch:
      s << "union_branch";
      break;
    case AST_Decl::NT_struct:
      s << "structure";
      break;
    case AST_Decl::NT_field:
      s << "field";
      break;
    case AST_Decl::NT_enum:
      s << "enumeration";
      break;
    case AST_Decl::NT_enum_val:
      s << "enumeration value";
      break;
    case AST_Decl::NT_string:
      s << "string";
      break;
    case AST_Decl::NT_array:
      s << "array";
      break;
    case AST_Decl::NT_sequence:
      s << "sequence";
      break;
    case AST_Decl::NT_typedef:
      s << "typedef";
      break;
    case AST_Decl::NT_pre_defined:
      s << "pre defined type";
      break;
    default: 
      s << " strange node type ";
      break;
    }
  return s;
}

IMPL_NARROW_FROM_DECL(adabe_name)
IMPL_NARROW_FROM_SCOPE(adabe_name)


