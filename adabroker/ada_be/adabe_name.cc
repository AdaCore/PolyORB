#include <adabe.h>
#include <stdio.h>
#include <nr_extern.hh>

static char* internal_produce_repositoryID(AST_Decl* decl,adabe_name* n);
static UTL_String* internal_search_pragma(AST_Decl* decl,char* p);

adabe_name::adabe_name(AST_Decl::NodeType t,UTL_ScopedName* n, UTL_StrList* up)
  : AST_Decl(t,n,up)
{
  pd_ada_local_name = "";
  pd_ada_full_name  = "";
  pd_defined_type   = 0;
  pd_fixed_size = true ;
  pd_repositoryID = internal_produce_repositoryID(this,this);
  compute_ada_name();
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
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_ads called in adabe_name");
}

void 
adabe_name::produce_adb(dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_adb called in adabe_name");
}

void 
adabe_name::produce_impl_ads(dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_impl_ads called in adabe_name");
}

void 
adabe_name::produce_impl_adb(dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_impl_adb called in adabe_name");
}

void 
adabe_name::produce_proxies_ads(dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_proxies_ads called in adabe_name");
}

void 
adabe_name::produce_proxies_adb(dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_proxies_adb called in adabe_name");
}

void 
adabe_name::produce_skel_ads(dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_skel_ads called in adabe_name");
}

void 
adabe_name::produce_skel_adb(dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_skel_adb called in adabe_name");
}

void 
adabe_name::produce_marshal_ads(dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_marshal_ads called in adabe_name");
}

void 
adabe_name::produce_marshal_adb(dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"produce_marshal_adb called in adabe_name");
}

string 
adabe_name::dump_name(dep_list&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"dump_name called in adabe_name");
}

string
adabe_name::local_type(void)
{
  throw adabe_internal_error(__FILE__,__LINE__,"Unxepected local type");
}

string 
adabe_name::marshal_name(dep_list&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error(__FILE__,__LINE__,"marshal_name called in adabe_name");
}

void
adabe_name::compute_ada_name()
{
  string loc_value = local_name()->get_string();
  if ((get_ada_local_name() == "") || (loc_value ==  "local type"))
    {
      pd_ada_full_name = "";
      int already_used;
      int loop = 0;
      string temp_name = local_name()->get_string();
#ifdef DEBUG_NAME
      cout << "in adabe_name, befor convert" << endl;
#endif
      if (temp_name == "local type")
	switch(node_type())
	  {
	  case AST_Decl::NT_array:
	  case AST_Decl::NT_string:	    
	    temp_name = local_type();
	    break;
	  default:
	    break;
	  }
      convert(temp_name);
      // which must be the ada_name ?
#ifdef DEBUG_NAME
      cout << "in adabe_name, after convert" << endl;
      cout << temp_name << endl;
#endif
      
      pd_ada_local_name = temp_name;
      UTL_Scope *parent_scope = defined_in();
      if (parent_scope != NULL) 
	do {
	  // is this name already used ?
#ifdef DEBUG_NAME
	  cout << "in adabe_name, node type of the parent in compute :" <<  parent_scope->scope_node_type() << endl;
#endif
	  switch (parent_scope->scope_node_type())
	    {
	    case AST_Decl::NT_op:
	    case AST_Decl::NT_enum:
	    case AST_Decl::NT_except:
	    case AST_Decl::NT_struct:
	    case AST_Decl::NT_union:
	    case AST_Decl::NT_interface:
	    case AST_Decl::NT_module:
	      pd_ada_full_name =  (dynamic_cast<adabe_name *>(parent_scope))->get_ada_full_name();
	    case AST_Decl::NT_root:
	      already_used = is_name_already_used(pd_ada_local_name, parent_scope);
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
	      char extension[4];
	      sprintf (extension, "_%d",loop++);
	      pd_ada_local_name = temp_name + extension;
	    }
	  if (pd_ada_full_name != "")
	    pd_ada_full_name = pd_ada_full_name + "." + pd_ada_local_name;
	  else  pd_ada_full_name = pd_ada_local_name;
	  /*  try to go the to the root of teh tree, and, each step, try to find 
	      a node with the same name. If such a node if found
	      try with another name */
	} 
	while (already_used == 1);
      
#ifdef DEBUG_NAME
      cout << "End of compute_ada_name from "<< pd_ada_local_name <<" full name is "
	   << pd_ada_full_name <<endl;
#endif
    }
  pd_repositoryID = internal_produce_repositoryID(this,this);
}

void 
adabe_name::add_number_to_type_name(adabe_name *type)
{
  int loop = 0;
  bool already_used;
  string temp_name = type->get_ada_local_name();
  UTL_Scope *parent_scope = defined_in();
  if (parent_scope != NULL) 
    do {
      switch (parent_scope->scope_node_type())
	{
	case AST_Decl::NT_op:
	case AST_Decl::NT_enum:
	case AST_Decl::NT_except:
	case AST_Decl::NT_struct:
	case AST_Decl::NT_union:
	  dynamic_cast<adabe_name *>(parent_scope)->add_number_to_type_name (type);
	  return;
	  break;
	case AST_Decl::NT_root:
	case AST_Decl::NT_interface:
	case AST_Decl::NT_module:
	  pd_ada_full_name =  (dynamic_cast<adabe_name *>(parent_scope))->get_ada_full_name();
	  already_used = is_name_already_used(type->get_ada_local_name(), parent_scope);
	  break;
	default:
	  throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope");
	}
      
      if (already_used == 1)
	{
	  if (loop>999)
	    throw adabe_internal_error(__FILE__,__LINE__,"too many name conflicts");
	  char extension[4];
	  sprintf (extension, "_%d",loop++);
	  type->set_ada_local_name(temp_name + extension);
	}
      type->set_ada_full_name (type->get_ada_local_name());
      /*  try to go the to the root of teh tree, and, each step, try to find 
	  a node with the same name. If such a node if found
	  try with another name */
    }
    while (already_used == 1);
}

int
adabe_name::is_name_already_used(string name, UTL_Scope *in_scope)
{
#ifdef DEBUG_NAME
  cout << "in adabe_name, called on an "  << node_type() <<endl;
#endif
  UTL_ScopeActiveIterator i(in_scope ,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      string temp;
      AST_Decl *d = i.item();
      i.next();
      if (d->in_main_file())
	{
	  adabe_name *ada = dynamic_cast<adabe_name *>(d);;
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

  while (name[0]=='_')
    {
      string temp = name.substr(1,name.length());
      name = temp;
    }
  if (name[name.length()-1]=='_')
      name += 'U';

  // All the '_' at the end or at the beginning of the names are removed

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
	UTL_ScopeActiveIterator i(adabe_operation::narrow_from_decl(this), UTL_Scope::IK_both);
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
	UTL_ScopeActiveIterator i(adabe_interface::narrow_from_decl(this), UTL_Scope::IK_both);
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
	UTL_ScopeActiveIterator i(adabe_module::narrow_from_decl(this), UTL_Scope::IK_both);
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
	UTL_ScopeActiveIterator i(adabe_root::narrow_from_decl(this), UTL_Scope::IK_both);
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
	UTL_ScopeActiveIterator i(adabe_exception::narrow_from_decl(this), UTL_Scope::IK_both);
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
	UTL_ScopeActiveIterator i(adabe_structure::narrow_from_decl(this),UTL_Scope::IK_both);
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
	UTL_ScopeActiveIterator i(adabe_union::narrow_from_decl(this), UTL_Scope::IK_both);
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
  if (this == adabe_global::adabe_current_file()) 
    return 0;
  AST_Decl::NodeType NT = node_type();
  if (NT == AST_Decl::NT_interface)
    {
      bool temp;
      adabe_interface *inter = dynamic_cast<adabe_interface *>(this);
      if (inter->is_forwarded())
	{
	  temp = with.check (get_ada_full_name() + "_forward"); 
	  if (!temp) with.add (get_ada_full_name() + "_forward");
	}
      else
	{
	  temp = with.check (get_ada_full_name()); 
	  if (!temp) with.add (get_ada_full_name());
	}
      return 1;
    }
  if (NT == AST_Decl::NT_module)
    {
      bool temp;
      temp = with.check (get_ada_full_name());
      if (!temp) with.add (get_ada_full_name());
      return 1;
    }
  if (NT == AST_Decl::NT_root)
    {
      bool temp;
      temp = with.check (get_ada_full_name());
      if (!temp) with.add (get_ada_full_name());
      return 1;
    }
  if (defined_in() == NULL) return 0;
  return (dynamic_cast<adabe_name *>(defined_in()))->is_imported (with); 

}

bool
adabe_name::is_marshal_imported (dep_list& with)
{
  if (this == adabe_global::adabe_current_file()) 
    return 0;
  AST_Decl::NodeType NT = node_type();
  if (NT == AST_Decl::NT_interface)
    {
      bool temp = true;
      temp = with.check (get_ada_full_name()+".marshal"); 
      if (!temp) with.add (get_ada_full_name()+".marshal");
      return temp;
    }
  if (NT == AST_Decl::NT_module)
    {
      bool temp = true;
      temp = with.check (get_ada_full_name()+".marshal");
      if (!temp) with.add (get_ada_full_name()+".marshal");
      return temp;
    }
   if (NT == AST_Decl::NT_root)
    {
      bool temp = true;
      temp = with.check (get_ada_full_name()+".marshal");
      if (!temp) with.add (get_ada_full_name()+".marshal");
      return temp;
    }
   if (defined_in() == NULL) return 0;
  //  if  ((NT == AST_Decl::NT_root)
  //       || (NT ==  AST_Decl::NT_except)
  //       || (NT ==  AST_Decl::NT_struct)
  //       || (NT ==  AST_Decl::NT_union))	
     return (dynamic_cast<adabe_name *>(defined_in()))->is_marshal_imported (with); 
      //    }
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

bool
adabe_name::has_fixed_size()
{
  return pd_fixed_size;
};
  // return true if the size of this element is fixed

void
adabe_name::no_fixed_size()
{
  pd_fixed_size = false;
  UTL_Scope *b = defined_in();
  if (b != NULL) {
    adabe_name *e = dynamic_cast<adabe_name *>(b);
    if (e->has_fixed_size()) e->no_fixed_size();
  };
};
  // set fixed size to False and calls no_fixed_size of the parent



// Functions used to provide the repository ID

static
char*
internal_produce_scope_name(UTL_ScopedName *n,char *separator)
{
#define DEFAULT_FQN_SIZE 128
  int  nsep = (separator == NULL) ? 0 : strlen(separator);
  int  n_alloc = DEFAULT_FQN_SIZE;
  int  n_used  = 1;
  char *p = new char[n_alloc];
  *p = '\0';

  UTL_ScopedNameActiveIterator iter(n);
  Identifier *id;
  char *q;

  Identifier *last = n->last_component();
  id = iter.item();
  while (!iter.is_done() && id != last)
    {
      q = id->get_string();
      if (strlen(q) != 0)
	{
	  n_used += strlen(q) + nsep;
	  while (n_used > n_alloc)
	    {
	      n_alloc += DEFAULT_FQN_SIZE;
	      char *t = new char[n_alloc];
	      strcpy(t,p);
	      delete [] p;
	      p = t;
	    }
	  strcat(p,q);
	  if (nsep)
	    strcat(p,separator);
	  if (q != id->get_string())
	    delete [] q;
	}
      iter.next();
      id = iter.item();
    }
  return p;
}

char*
adabe_name::repositoryID() const 
{
  // Check the pragmas attached to this node to see if
  // pragma ID is defined to override the default repositoryID.
  UTL_String* id;
  if ((id = internal_search_pragma((AST_Decl*)this,"ID")) != 0) 
    {
    return id->get_string();
    }
  else if ((id = internal_search_pragma((AST_Decl*)this,"version")) != 0) 
    {
      // Check if pragma version is defined to override the
      // version number in the default repositoryID.
      char* p = strrchr(pd_repositoryID,':') + 1;
      char* result = new char[(p-pd_repositoryID)+strlen(id->get_string())+1];
      strncpy(result,pd_repositoryID,p-pd_repositoryID);
      result[p-pd_repositoryID] = '\0';
      strcat(result,id->get_string());
      return result;
    }
  else 
    {
      return pd_repositoryID;
    }
}

static
char*
internal_produce_repositoryID(AST_Decl *decl,adabe_name *n)
{
  // find if any pragma prefix applies to this node. 
  switch(decl->node_type())
    {
    case AST_Decl::NT_interface:
    case AST_Decl::NT_except:
      {
	UTL_String* prefix;
	UTL_Scope*  prefix_scope = 0;
	
	if (!(prefix = internal_search_pragma(decl,"prefix"))) 
	  {
	    // no pragma prefix defined in this node, search the parents
	    prefix_scope = decl->defined_in();
	    while (prefix_scope) 
	      {
		if (!(prefix = internal_search_pragma(ScopeAsDecl(prefix_scope),
						      "prefix"))) 
		  {
		    prefix_scope = ScopeAsDecl(prefix_scope)->defined_in();
		  }
		else 
		  {
		    break;
		  }
	      }
	  }
	
	UTL_ScopedName* idlist;
	
	if (prefix) 
	  {
	    idlist = new UTL_ScopedName(decl->local_name(),NULL);
	    UTL_Scope* u = decl->defined_in();
	    while (prefix_scope && prefix_scope != u) 
	      {
		idlist = new UTL_ScopedName(ScopeAsDecl(u)->local_name(),idlist);
		u = ScopeAsDecl(u)->defined_in();
	      }
	    idlist = new UTL_ScopedName(new Identifier(prefix->get_string()),idlist);
	    
	  }
	else 
	  {
	    idlist = decl->name();
	  }
	{
	  char* q = internal_produce_scope_name(idlist,"/");
	  char* result = new char [strlen("IDL::1.0") + 
				  strlen(q) +
				  strlen(decl->local_name()->get_string()) + 1];
	  strcpy(result,"IDL:");
	  strcat(result,q);
	  strcat(result,decl->local_name()->get_string());
	  strcat(result,":1.0");
	  return result;
	}
	break;
      }
    default: 
      return NULL;
      break;
    }
}


static
UTL_String*
internal_search_pragma(AST_Decl* decl,char* p) 
{
  UTL_String* result = 0;
  UTL_StrlistActiveIterator l(decl->pragmas());
  while (!l.is_done()) {
    if (strcmp(p,l.item()->get_string()) == 0) 
      {
	l.next();
	result = l.item();
      }
    else 
      {
	l.next();
      }
    l.next();
  }
  return result;
}

char *lower (const char *str)
{
  char *new_str = new char[strlen(str)+1];
  strcpy(new_str,str);
  for (unsigned int i = 0; i < strlen(str); i++) 
    {
      if ((str[i] > 64) && ( str[i] < 91))
	new_str[i] = new_str[i] + 32;
    }
  return new_str;
}

string spaces (int n, char d)
{
  string tmp = "";
  for (i=0;i<n;i++) tmp+=d;
  return tmp;
}

IMPL_NARROW_FROM_DECL(adabe_name)
IMPL_NARROW_FROM_SCOPE(adabe_name)








