#include <adabe.h>
#include <stdio.h>
#include <nr_extern.hh>

adabe_name::adabe_name() {
};

string adabe_name::get_ada_local_name(){
  return pd_ada_local_name;
} 

string adabe_name::get_ada_full_name() {
  return pd_ada_full_name;
}

void adabe_name::compute_ada_name() {
  int already_used;
  int loop = 0;
  string temp_name = local_name()->get_string();
  convert(temp_name);
  // which must be the ada_name ?
  
  pd_ada_local_name = temp_name;
  UTL_Scope *parent_scope = defined_in();
  do {
    // is this name already used ?
    switch (parent_scope->scope_node_type()) {
    case AST_Decl::NT_interface:
      already_used = (adabe_interface::narrow_from_scope(parent_scope))->is_name_already_used(pd_ada_local_name);
      break;
    case AST_Decl::NT_module:
      already_used = (adabe_module::narrow_from_scope(parent_scope))->is_name_already_used(pd_ada_local_name);
      break;
    case AST_Decl::NT_root:
      already_used = (adabe_root::narrow_from_scope(parent_scope))->is_name_already_used(pd_ada_local_name);
      break;
    case AST_Decl::NT_except:
      already_used = (adabe_exception::narrow_from_scope(parent_scope))->is_name_already_used(pd_ada_local_name);
      break;
    case AST_Decl::NT_struct:
      already_used = (adabe_structure::narrow_from_scope(parent_scope))->is_name_already_used(pd_ada_local_name);
      
      break;
    case AST_Decl::NT_union:
      already_used = (adabe_union::narrow_from_scope(parent_scope))->is_name_already_used(pd_ada_local_name);
      break;
    default:
      throw adabe_internal_error(__FILE__,__LINE__,"unexpected contening scope");
    }
    if (already_used == 1) {
      if (loop>999)
	throw adabe_internal_error(__FILE__,__LINE__,"too many name conflicts");
      char extension[4];
      sprintf (extension, "_%d",loop);
      //      delete pd_ada_local_name;
      //      pd_ada_local_name = new string();
      pd_ada_local_name = temp_name + extension;
    }
    /*  try to go the to the root of teh tree, and, each step, try to find 
	a node with the same name. If such a node if found
	try with another name */
  }
  while (already_used == 1);
}


int adabe_name::is_name_already_used(string name) {
  UTL_ScopeActiveIterator i(DeclAsScope(this),UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      string temp;
      AST_Decl *d = i.item();
      adabe_name *ada = adabe_name::narrow_from_decl(d);
      temp == (ada->get_ada_local_name());
      if (temp  != "")
	if (temp == name)
	  return 1;
    }
  bool already_used = false;
  UTL_Scope *parent_scope = defined_in();
  if (parent_scope != NULL)
    switch (parent_scope->scope_node_type()) {
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
}

bool adabe_name::is_already_defined(){
  return pd_defined_type;
}

void adabe_name::set_already_defined(){
  pd_defined_type = true;
}

void adabe_name::convert(string &name) {
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
  
  while (name[0]=='_') {
    string temp = name.substr(1,name.length());
    name = temp;
  }
  int c;
  while ((c = name.find("__")) != -1) 
    name[c+1]='U';
  // Eliminates the '__'
};

void adabe_name::set_undefined() {
  pd_defined_type = 0;
  switch (node_type()) {
    // if it's a complexe type does the same to each type in the node ...
  case AST_Decl::NT_interface:{
    UTL_ScopeActiveIterator i(adabe_interface::narrow_from_decl(this), UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	adabe_name::narrow_from_decl(i.item())->set_undefined();
      };
  }
  break;
  case AST_Decl::NT_module: {
    UTL_ScopeActiveIterator i(adabe_module::narrow_from_decl(this), UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	adabe_name::narrow_from_decl(i.item())->set_undefined();
      };
  }
  break;
    
  case AST_Decl::NT_root: {
    UTL_ScopeActiveIterator i(adabe_root::narrow_from_decl(this), UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	adabe_name::narrow_from_decl(i.item())->set_undefined();
      };
  }
  break;
  case AST_Decl::NT_except: {
    UTL_ScopeActiveIterator i(adabe_exception::narrow_from_decl(this), UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	adabe_name::narrow_from_decl(i.item())->set_undefined();
      };
  }
  break;
  case AST_Decl::NT_struct: {
    UTL_ScopeActiveIterator i(adabe_structure::narrow_from_decl(this),UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	adabe_name::narrow_from_decl(i.item())->set_undefined();
      };
  }
  break;
  case AST_Decl::NT_union:{
    UTL_ScopeActiveIterator i(adabe_union::narrow_from_decl(this), UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	adabe_name::narrow_from_decl(i.item())->set_undefined();
      };
  }
  break;
  default:
    //...  else do nothing
  break;
  }
}
