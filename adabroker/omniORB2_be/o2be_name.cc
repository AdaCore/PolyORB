// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_name.cc             Created on: 12/08/1996
//			    Author    : Sai-Lai Lo (sll)
//
//    Copyright (C) 1996, 1997 Olivetti & Oracle Research Laboratory
//
//  This file is part of omniidl2.
//
//  Omniidl2 is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,
//  USA.
//
// Description:
//

/*
  $Log: o2be_name.cc,v $
  Revision 1.1  1999/02/14 17:45:26  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.12  1999/01/07 09:43:43  djr
  Two new 'names' for declarations, _idname() and canonical_name() which
  produce names guarenteed to be unique for a given identifier and a
  given IDL type respectively.

  Revision 1.11  1998/08/19 15:53:40  sll
  New member function variable_qualifier() replaces the static VarToken.

  Revision 1.10  1998/08/13 22:39:01  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available.

  Revision 1.9  1998/04/07 18:49:01  sll
  Use std::fstream instead of fstream.

// Revision 1.8  1998/01/27  16:46:29  ewc
// *** empty log message ***
//
  Revision 1.7  1997/12/09 19:54:58  sll
  *** empty log message ***

  Revision 1.6  1997/08/21 21:16:26  sll
  Minor cleanup.

// Revision 1.5  1997/05/06  14:01:18  sll
// Public release.
//
  */

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

static char* internal_produce_scope_name(UTL_ScopedName* n,char* separator);
static char* internal_check_name_for_reserved_words(char* p);
static char* internal_produce_repositoryID(AST_Decl* decl,o2be_name* n);
static UTL_String* internal_search_pragma(AST_Decl* decl,char* p);


o2be_name::o2be_name(AST_Decl::NodeType t,UTL_ScopedName* n, UTL_StrList* up)
  : AST_Decl(t,n,up)
{
  pd_scopename = internal_produce_scope_name(name(),"::");
  pd__scopename = internal_produce_scope_name(name(),"_");
  pd_uqname = internal_check_name_for_reserved_words(local_name()
						     ->get_string());
  char *p = new char [strlen(pd_scopename)+strlen(pd_uqname)+1];
  strcpy(p,pd_scopename);
  strcat(p,pd_uqname);
  pd_fqname = p;

  p = new char [strlen(pd__scopename)+strlen(pd_uqname)+1];
  strcpy(p,pd__scopename);
  strcat(p,pd_uqname);
  pd__fqname = p;


  p = new char [strlen((const char*) "_tc_")+strlen(pd_uqname)+1];
  strcpy(p,(const char*) "_tc_");
  strcat(p,pd_uqname);
  pd_tcname = p;

  p = new char [strlen(pd_scopename)+strlen(pd_tcname)+1];
  strcpy(p,pd_scopename);
  strcat(p,pd_tcname);
  pd_fqtcname = p;

  p = new char[strlen(pd__scopename) + strlen(pd_tcname) +1];
  strcpy(p,pd__scopename);
  strcat(p,pd_tcname);
  pd__fqtcname = p;
					      
  pd_repositoryID = internal_produce_repositoryID(this,this);

  pd__idname = o2be_name_mangler::produce_idname(name());

  // this will be initialised when needed
  pd_canonical_name = 0;
}


char*
o2be_name::_idname()
{
  // internal_produce_idname gets prefined types wrong- but it can't
  // do any better at construction time, as it doesn't have access
  // to the AST_PredefinedType members (it hasn't yet been fully
  // constructed).
  //  Thus we trap it here. _p is used as a connective between the
  // components of type names that have more than one component.
  // (like unsigned long).
  if( node_type() == AST_Decl::NT_pre_defined ) {
    switch( AST_PredefinedType::narrow_from_decl(this)->pt() ) {
    case AST_PredefinedType::PT_long:        return "long";
    case AST_PredefinedType::PT_ulong:       return "unsigned_plong";
    case AST_PredefinedType::PT_longlong:    return "long_plong";
    case AST_PredefinedType::PT_ulonglong:   return "unsigned_plong_plong";
    case AST_PredefinedType::PT_short:       return "short";
    case AST_PredefinedType::PT_ushort:      return "unsigned_pshort";
    case AST_PredefinedType::PT_float:       return "float";
    case AST_PredefinedType::PT_double:      return "double";
    case AST_PredefinedType::PT_longdouble:  return "long_pdouble";
    case AST_PredefinedType::PT_char:        return "char";
    case AST_PredefinedType::PT_wchar:       return "wchar";
    case AST_PredefinedType::PT_boolean:     return "boolean";
    case AST_PredefinedType::PT_octet:       return "octet";
    case AST_PredefinedType::PT_any:         return "any";
    case AST_PredefinedType::PT_void:        return "void";
    case AST_PredefinedType::PT_pseudo:      return "pseudo";
    case AST_PredefinedType::PT_TypeCode:    return "TypeCode";
    }
  } else if( node_type() == AST_Decl::NT_string )
    return "string";

  return pd__idname;
}


char*
o2be_name::canonical_name()
{
  if( !pd_canonical_name )
    pd_canonical_name = o2be_name_mangler::produce_canonical_name(this);

  return pd_canonical_name;
}


static
char*
internal_produce_unambiguous_name(AST_Decl* me, AST_Decl* used_in,
				  idl_bool return_scopename,
				  idl_bool use_fqname)
{
  AST_Decl* common_ancestor = 0;

  if (use_fqname)
    goto UNAMBIGUOUS;

  if (used_in->has_ancestor(me)) {
    common_ancestor = me;
  }
  else {
    UTL_Scope* parent = me->defined_in();
    if (parent && ScopeAsDecl(parent)->node_type() != AST_Decl::NT_root) {
      if (used_in->has_ancestor(ScopeAsDecl(parent))) {
	common_ancestor = ScopeAsDecl(parent);
      }
      else {
	if (ScopeAsDecl(parent)->node_type() == AST_Decl::NT_enum) {
	  parent = ScopeAsDecl(parent)->defined_in();
	  if (parent && 
	      ScopeAsDecl(parent)->node_type() !=AST_Decl::NT_root &&
	      used_in->has_ancestor(ScopeAsDecl(parent))) 
	    {
	      common_ancestor = ScopeAsDecl(parent);
	    }
	}
      }
    }
  }

  if (common_ancestor) {
    // We may be able to use the unqualified name provided that there
    // is no ambiguity
    UTL_Scope* check;
    if (DeclAsScope(used_in)) {
      // Check should start in the scope of used_in.
      check = DeclAsScope(used_in);
    }
    else {
      check = used_in->defined_in();
    }
    while (check && ScopeAsDecl(check) != common_ancestor) {
      if (check->lookup_by_name_local(me->local_name(),0) != 0) {
	// There is a name clash, cannot use the unqualified name
	goto UNAMBIGUOUS;
      }
      check = (ScopeAsDecl(check))->defined_in();
    }
    // Reach here means there is no name clash, we can use the unqualified
    // name
    
    char* p;
    if (return_scopename) {
      p = "";
    }
    else {
      p = o2be_name::narrow_and_produce_uqname(me);
    }
    return p;
  }

  // Reach here iff neither this node nor its parent is an ancestor
  // Recursively search the ancestor of this node until we find the common
  // ancestor.
  {
    UTL_Scope* parent = me->defined_in();
    while (parent && ScopeAsDecl(parent)->node_type() != AST_Decl::NT_root) {
      common_ancestor = ScopeAsDecl(parent);
      if (used_in->has_ancestor(common_ancestor)) {
	// bingo, now we have to check if we can use the partially scoped name
	// starting from here. To be able to do so, there should be no
	// name clash with the name of this ancestor node.
	{
	  AST_Decl* startnode = ScopeAsDecl(me->defined_in());
	  while (ScopeAsDecl(startnode->defined_in()) != common_ancestor) {
	    startnode = ScopeAsDecl(startnode->defined_in());
	  }
	  UTL_Scope* check;
	  if (DeclAsScope(used_in)) {
	    // Check should start in the scope of used_in.
	    check = DeclAsScope(used_in);
	  }
	  else {
	    check = used_in->defined_in();
	  }
	  while (check && ScopeAsDecl(check) != common_ancestor) {
	    if (check->lookup_by_name_local(startnode->local_name(),0)) 
	      {
		// There is a name clash, cannot use the unqualified name
		goto UNAMBIGUOUS;
	      }
	    check = (ScopeAsDecl(check))->defined_in();
	  }
	}
	// Reach here means there is no name clash, we can construct
	// the partially scoped name
	UTL_ScopedName* pqname =  new UTL_ScopedName(me->local_name(),NULL);
	parent = me->defined_in();
	if (ScopeAsDecl(parent)->node_type() == AST_Decl::NT_enum) {
	  parent = ScopeAsDecl(parent)->defined_in();
	}
	while (ScopeAsDecl(parent) != common_ancestor) {
	  pqname = new UTL_ScopedName(ScopeAsDecl(parent)->local_name(),
				      pqname);
	  parent = ScopeAsDecl(parent)->defined_in();
	}
	char* pqstring = internal_produce_scope_name(pqname,"::");
	if (return_scopename) {
	  return pqstring;
	}
	else {
	  char* uqname = o2be_name::narrow_and_produce_uqname(me);
	  char* result = new char[strlen(pqstring)+strlen(uqname)+1];
	  strcpy(result,pqstring);
	  strcat(result,uqname);
	  return result;
	}
      }
      parent = common_ancestor->defined_in();
    }
  }

UNAMBIGUOUS:
  // have to generate the fully qualified name.
  // Check if we need to put the prefix :: at the front to avoid a name
  // clash
  char* prefix = 0;
  {
    AST_Decl* topnode = me;
    UTL_Scope* check;

    if (me->node_type() != AST_Decl::NT_enum_val) {
      check = me->defined_in();
    }
    else {
      check = ScopeAsDecl(me->defined_in())->defined_in();
    }
    // Try to get to the first component of the fully qualified name
    // If the backpointer returns by defined_in() terminate in a
    // AST_root, we could just test for that to terminate the following
    // loop. Unfortunately this strategy doesn't seem to work. So
    // we check if the local name is a nil string instead.
    while (check && ScopeAsDecl(check)->local_name()->get_string() &&
	   strlen(ScopeAsDecl(check)->local_name()->get_string())) {
      topnode = ScopeAsDecl(check);
      check = topnode->defined_in();
    }
    if (DeclAsScope(used_in)) {
      // Check should start in the scope of used_in.
      check = DeclAsScope(used_in);
    }
    else {
      check = used_in->defined_in();
    }
    Identifier* nameclash = topnode->local_name();
    if (topnode->node_type() == AST_Decl::NT_enum_val) {
      topnode = ScopeAsDecl(topnode->defined_in());
    }
    while (check != topnode->defined_in()) {
      if (check->lookup_by_name_local(nameclash,0)) 
	{
	  // There is a name clash, must add :: at the front;
	  prefix = "::";
	}
      check = (ScopeAsDecl(check))->defined_in();
    }
  }
  char* result;
  if (return_scopename) {
    result = o2be_name::narrow_and_produce_scopename(me);
  }
  else {
    result = o2be_name::narrow_and_produce_fqname(me);
  }
  if (prefix) {
    char* pp = new char[strlen(result)+strlen(prefix)+1];
    strcpy(pp,prefix);
    strcat(pp,result);
    result = pp;
  }
  return result;
}


char*
o2be_name::unambiguous_name(AST_Decl* used_in,idl_bool use_fqname) const
{
  return internal_produce_unambiguous_name((AST_Decl*)this,
					   used_in,0,use_fqname);
}


char*
o2be_name::unambiguous_scopename(AST_Decl* used_in,idl_bool use_fqname) const
{
  return internal_produce_unambiguous_name((AST_Decl*)this,
					   used_in,1,use_fqname);
}


char*
o2be_name::repositoryID() const {
  // Check the pragmas attached to this node to see if
  // pragma ID is defined to override the default repositoryID.
  UTL_String* id;
  if ((id = internal_search_pragma((AST_Decl*)this,"ID")) != 0) {
    return id->get_string();
  }
  else if ((id = internal_search_pragma((AST_Decl*)this,"version")) != 0) {
    // Check if pragma version is defined to override the
    // version number in the default repositoryID.
    char* p = strrchr(pd_repositoryID,':') + 1;
    char* result = new char[(p-pd_repositoryID)+strlen(id->get_string())+1];
    strncpy(result,pd_repositoryID,p-pd_repositoryID);
    result[p-pd_repositoryID] = '\0';
    strcat(result,id->get_string());
    return result;
  }
  else {
    return pd_repositoryID;
  }
}


static
char*
internal_produce_repositoryID(AST_Decl *decl,o2be_name *n)
{
  // find if any pragma prefix applies to this node. 
  UTL_String* prefix;
  UTL_Scope*  prefix_scope = 0;

  if (!(prefix = internal_search_pragma(decl,"prefix"))) {
    // no pragma prefix defined in this node, search the parents
    prefix_scope = decl->defined_in();
    while (prefix_scope) {
      if (!(prefix = internal_search_pragma(ScopeAsDecl(prefix_scope),
					    "prefix"))) {
	prefix_scope = ScopeAsDecl(prefix_scope)->defined_in();
      }
      else {
	break;
      }
    }
  }

  UTL_ScopedName* idlist;

  if (prefix) {
    idlist = new UTL_ScopedName(decl->local_name(),NULL);
    UTL_Scope* u = decl->defined_in();
    while (prefix_scope && prefix_scope != u) {
      idlist = new UTL_ScopedName(ScopeAsDecl(u)->local_name(),idlist);
      u = ScopeAsDecl(u)->defined_in();
    }
    idlist = new UTL_ScopedName(new Identifier(prefix->get_string()),idlist);

  }
  else {
    idlist = decl->name();
  }
  char* q = internal_produce_scope_name(idlist,"/");
  char* result = new char [strlen("IDL::1.0") + 
			   strlen(q) + 
			   strlen(n->uqname())+1];
  strcpy(result,"IDL:");
  strcat(result,q);
  strcat(result,n->uqname());
  strcat(result,":1.0");
  delete [] q;
  return result;
}


static
UTL_String*
internal_search_pragma(AST_Decl* decl,char* p) 
{
  UTL_String* result = 0;
  UTL_StrlistActiveIterator l(decl->pragmas());
  while (!l.is_done()) {
    if (strcmp(p,l.item()->get_string()) == 0) {
      l.next();
      result = l.item();
    }
    else {
      l.next();
    }
    l.next();
  }
  return result;
}


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
      q = internal_check_name_for_reserved_words(id->get_string());
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


// reserved words are the keywords of C++ minus the keywords of CORBA IDL.
static char *reserved_words[] = {
  "and", "and_eq", "asm", "auto", 
  "bitand", "bitor", "break", 
  "catch",  "class", "compl", "const_cast", "continue",
  "delete", "do", "dynamic_cast",
  "else", "explicit", "extern",
  "false", "for", "friend", 
  "goto", "if", "inline", "int", 
  "mutable", 
  "namespace", "new", "not", "not_eq",
  "operator", "or", "or_eq",
  "private", "protected", "public",
  "register", "reinterpret_cast", "return",
  "signed", "sizeof", "static", "static_cast",
  "template", "this", "throw", "true", "try", "typeid", "typename",
  "using", 
  "virtual", "volatile", "wchar_t", "while",
  "xor", "xor_eq",
  NULL
};

char*
internal_check_name_for_reserved_words(char *keyw)
{
  char **rp = reserved_words;

  while (*rp != NULL)
    {
      if (!strcmp(*rp,keyw))
	{
	  // This is a reserved word, prefix it by '_'
	  char *str = new char[strlen(keyw)+2];
	  strcpy(str,"_");
	  strcat(str,keyw);
	  return str;
	}
      rp++;
    }
  return keyw;
}


char*
o2be_name::narrow_and_produce_fqname(AST_Decl *decl)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_module:
      return o2be_module::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_root:
      return o2be_root::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_interface:
      return o2be_interface::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_interface_fwd:
      return o2be_interface_fwd::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_const:
      return o2be_constant::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_except:
      return o2be_exception::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_attr:
      return o2be_attribute::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_op:
      return o2be_operation::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_argument:
      return o2be_argument::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_union:
      return o2be_union::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_union_branch:
      return o2be_union_branch::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_struct:
      return o2be_structure::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_field:
      return o2be_field::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_enum:
      return o2be_enum::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_enum_val:
      return o2be_enum_val::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_string:
      return o2be_string::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_array:
      return o2be_array::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_sequence:
      return o2be_sequence::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_typedef:
      return o2be_typedef::narrow_from_decl(decl)->fqname();
    case AST_Decl::NT_pre_defined:
      return o2be_predefined_type::narrow_from_decl(decl)->fqname();
    default:
      throw o2be_internal_error(__FILE__,__LINE__,"Unrecognised argument type");
    }
return 0; // For MSVC++ 4.2
}


char*
o2be_name::narrow_and_produce__fqname(AST_Decl *decl)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_module:
      return o2be_module::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_root:
      return o2be_root::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_interface:
      return o2be_interface::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_interface_fwd:
      return o2be_interface_fwd::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_const:
      return o2be_constant::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_except:
      return o2be_exception::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_attr:
      return o2be_attribute::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_op:
      return o2be_operation::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_argument:
      return o2be_argument::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_union:
      return o2be_union::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_union_branch:
      return o2be_union_branch::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_struct:
      return o2be_structure::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_field:
      return o2be_field::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_enum:
      return o2be_enum::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_enum_val:
      return o2be_enum_val::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_string:
      return o2be_string::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_array:
      return o2be_array::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_sequence:
      return o2be_sequence::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_typedef:
      return o2be_typedef::narrow_from_decl(decl)->_fqname();
    case AST_Decl::NT_pre_defined:
      return o2be_predefined_type::narrow_from_decl(decl)->_fqname();
    default:
      throw o2be_internal_error(__FILE__, __LINE__,
				"Unrecognised argument type");
    }
  return 0; // For MSVC++ 4.2
}


char*
o2be_name::narrow_and_produce_scopename(AST_Decl *decl)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_module:
      return o2be_module::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_root:
      return o2be_root::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_interface:
      return o2be_interface::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_interface_fwd:
      return o2be_interface_fwd::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_const:
      return o2be_constant::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_except:
      return o2be_exception::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_attr:
      return o2be_attribute::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_op:
      return o2be_operation::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_argument:
      return o2be_argument::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_union:
      return o2be_union::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_union_branch:
      return o2be_union_branch::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_struct:
      return o2be_structure::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_field:
      return o2be_field::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_enum:
      return o2be_enum::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_enum_val:
      return o2be_enum_val::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_string:
      return o2be_string::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_array:
      return o2be_array::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_sequence:
      return o2be_sequence::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_typedef:
      return o2be_typedef::narrow_from_decl(decl)->scopename();
    case AST_Decl::NT_pre_defined:
      return o2be_predefined_type::narrow_from_decl(decl)->scopename();
    default:
      throw o2be_internal_error(__FILE__, __LINE__,
				"Unrecognised argument type");
    }
  return 0; // For MSVC++ 4.2
}


char*
o2be_name::narrow_and_produce__scopename(AST_Decl *decl)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_module:
      return o2be_module::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_root:
      return o2be_root::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_interface:
      return o2be_interface::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_interface_fwd:
      return o2be_interface_fwd::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_const:
      return o2be_constant::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_except:
      return o2be_exception::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_attr:
      return o2be_attribute::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_op:
      return o2be_operation::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_argument:
      return o2be_argument::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_union:
      return o2be_union::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_union_branch:
      return o2be_union_branch::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_struct:
      return o2be_structure::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_field:
      return o2be_field::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_enum:
      return o2be_enum::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_enum_val:
      return o2be_enum_val::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_string:
      return o2be_string::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_array:
      return o2be_array::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_sequence:
      return o2be_sequence::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_typedef:
      return o2be_typedef::narrow_from_decl(decl)->_scopename();
    case AST_Decl::NT_pre_defined:
      return o2be_predefined_type::narrow_from_decl(decl)->_scopename();
    default:
      throw o2be_internal_error(__FILE__,__LINE__,"Unrecognised argument type");
    }
return 0; // For MSVC++ 4.2
}


char*
o2be_name::narrow_and_produce_uqname(AST_Decl *decl)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_module:
      return o2be_module::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_root:
      return o2be_root::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_interface:
      return o2be_interface::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_interface_fwd:
      return o2be_interface_fwd::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_const:
      return o2be_constant::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_except:
      return o2be_exception::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_attr:
      return o2be_attribute::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_op:
      return o2be_operation::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_argument:
      return o2be_argument::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_union:
      return o2be_union::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_union_branch:
      return o2be_union_branch::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_struct:
      return o2be_structure::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_field:
      return o2be_field::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_enum:
      return o2be_enum::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_enum_val:
      return o2be_enum_val::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_string:
      return o2be_string::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_array:
      return o2be_array::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_sequence:
      return o2be_sequence::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_typedef:
      return o2be_typedef::narrow_from_decl(decl)->uqname();
    case AST_Decl::NT_pre_defined:
      return o2be_predefined_type::narrow_from_decl(decl)->uqname();
    default:
      throw o2be_internal_error(__FILE__,__LINE__,"Unrecognised argument type");
    }
return 0; // For MSVC++ 4.2
}


char*
o2be_name::narrow_and_produce__idname(AST_Decl *decl)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_module:
      return o2be_module::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_root:
      return o2be_root::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_interface:
      return o2be_interface::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_interface_fwd:
      return o2be_interface_fwd::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_const:
      return o2be_constant::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_except:
      return o2be_exception::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_attr:
      return o2be_attribute::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_op:
      return o2be_operation::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_argument:
      return o2be_argument::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_union:
      return o2be_union::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_union_branch:
      return o2be_union_branch::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_struct:
      return o2be_structure::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_field:
      return o2be_field::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_enum:
      return o2be_enum::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_enum_val:
      return o2be_enum_val::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_string:
      return o2be_string::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_array:
      return o2be_array::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_sequence:
      return o2be_sequence::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_typedef:
      return o2be_typedef::narrow_from_decl(decl)->_idname();
    case AST_Decl::NT_pre_defined:
      return o2be_predefined_type::narrow_from_decl(decl)->_idname();
    default:
      throw o2be_internal_error(__FILE__, __LINE__,
				"Unrecognised argument type");
    }
  return 0; // For MSVC++ 4.2
}


char*
o2be_name::narrow_and_produce_canonical_name(AST_Decl *decl)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_module:
      return o2be_module::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_root:
      return o2be_root::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_interface:
      return o2be_interface::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_interface_fwd:
      return o2be_interface_fwd::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_const:
      return o2be_constant::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_except:
      return o2be_exception::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_attr:
      return o2be_attribute::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_op:
      return o2be_operation::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_argument:
      return o2be_argument::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_union:
      return o2be_union::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_union_branch:
      return o2be_union_branch::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_struct:
      return o2be_structure::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_field:
      return o2be_field::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_enum:
      return o2be_enum::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_enum_val:
      return o2be_enum_val::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_string:
      return o2be_string::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_array:
      return o2be_array::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_sequence:
      return o2be_sequence::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_typedef:
      return o2be_typedef::narrow_from_decl(decl)->canonical_name();
    case AST_Decl::NT_pre_defined:
      return o2be_predefined_type::narrow_from_decl(decl)->canonical_name();
    default:
      throw o2be_internal_error(__FILE__, __LINE__,
				"Unrecognised argument type");
    }
  return 0; // For MSVC++ 4.2
}


char*
o2be_name::narrow_and_produce_unambiguous_name(AST_Decl *decl,
					       AST_Decl *used_in,
					       idl_bool use_fqname)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_module:
      return o2be_module::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_root:
      return o2be_root::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_interface:
      return o2be_interface::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_interface_fwd:
      return o2be_interface_fwd::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_const:
      return o2be_constant::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_except:
      return o2be_exception::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_attr:
      return o2be_attribute::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_op:
      return o2be_operation::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_argument:
      return o2be_argument::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_union:
      return o2be_union::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_union_branch:
      return o2be_union_branch::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_struct:
      return o2be_structure::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_field:
      return o2be_field::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_enum:
      return o2be_enum::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_enum_val:
      return o2be_enum_val::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_string:
      return o2be_string::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_array:
      return o2be_array::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_sequence:
      return o2be_sequence::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_typedef:
      return o2be_typedef::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    case AST_Decl::NT_pre_defined:
      return o2be_predefined_type::narrow_from_decl(decl)
	->unambiguous_name(used_in, use_fqname);
    default:
      throw o2be_internal_error(__FILE__, __LINE__,
				"Unrecognised argument type");
    }
  return 0; // For MSVC++ 4.2
}


char*
o2be_name::narrow_and_produce_unambiguous_scopename(AST_Decl *decl,
						    AST_Decl *used_in,
						    idl_bool use_fqname)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_module:
      return o2be_module::narrow_from_decl(decl)->unambiguous_scopename(used_in,
									use_fqname);
    case AST_Decl::NT_root:
      return o2be_root::narrow_from_decl(decl)->unambiguous_scopename(used_in,
								      use_fqname);
    case AST_Decl::NT_interface:
      return o2be_interface::narrow_from_decl(decl)->unambiguous_scopename(used_in,
									   use_fqname);
    case AST_Decl::NT_interface_fwd:
      return o2be_interface_fwd::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_const:
      return o2be_constant::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_except:
      return o2be_exception::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_attr:
      return o2be_attribute::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_op:
      return o2be_operation::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_argument:
      return o2be_argument::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_union:
      return o2be_union::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_union_branch:
      return o2be_union_branch::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_struct:
      return o2be_structure::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_field:
      return o2be_field::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_enum:
      return o2be_enum::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_enum_val:
      return o2be_enum_val::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_string:
      return o2be_string::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_array:
      return o2be_array::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_sequence:
      return o2be_sequence::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_typedef:
      return o2be_typedef::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    case AST_Decl::NT_pre_defined:
      return o2be_predefined_type::narrow_from_decl(decl)->unambiguous_scopename(used_in,use_fqname);
    default:
      throw o2be_internal_error(__FILE__, __LINE__,
				"Unrecognised argument type");
    }
  return 0; // For MSVC++ 4.2
}


void
o2be_name::narrow_and_produce_typecode_skel(AST_Decl *decl, std::fstream& s)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_pre_defined:
    case AST_Decl::NT_string:
    case AST_Decl::NT_interface:
    case AST_Decl::NT_interface_fwd:
      break;

    case AST_Decl::NT_except:
      o2be_exception::narrow_from_decl(decl)->produce_typecode_skel(s);
      break;

    case AST_Decl::NT_union:
      o2be_union::narrow_from_decl(decl)->produce_typecode_skel(s);
      break;

    case AST_Decl::NT_struct:
      o2be_structure::narrow_from_decl(decl)->produce_typecode_skel(s);
      break;

    case AST_Decl::NT_enum:
      o2be_enum::narrow_from_decl(decl)->produce_typecode_skel(s);
      break;

    case AST_Decl::NT_array:
      o2be_array::narrow_from_decl(decl)->produce_typecode_skel(s);
      break;
     
    case AST_Decl::NT_sequence:
      o2be_sequence::narrow_from_decl(decl)->produce_typecode_skel(s);
      break;
      
    case AST_Decl::NT_typedef:
      o2be_typedef::narrow_from_decl(decl)->produce_typecode_skel(s);
      break;
      
    default:
      throw o2be_internal_error(__FILE__, __LINE__,
				"Unrecognised argument type");
    }
}


void 
o2be_name::produce_typecode_member(AST_Decl *decl, std::fstream& s)
{
  switch(decl->node_type())
    {
    case AST_Decl::NT_pre_defined:
      o2be_predefined_type::narrow_from_decl(decl)->produce_typecode_member(s);
      break;

    case AST_Decl::NT_string:
      s << "CORBA::TypeCode::PR_string_tc("
	<< o2be_string::narrow_from_decl(decl)->max_length()
	<< ")";
      break;

    case AST_Decl::NT_interface:
      {
	char* intf_name =o2be_interface::narrow_from_decl(decl)->uqname();
	if( strcmp(intf_name,"Object") == 0 ||
	    strcmp(intf_name,"CORBA::Object") == 0 )
	  s << "CORBA::TypeCode::PR_Object_tc()";
	else {
	  s << "CORBA::TypeCode::PR_interface_tc(\""
	    << o2be_interface::narrow_from_decl(decl)->repositoryID()
	    << "\", \"" << intf_name << "\")";
	}
	break;
      }

    case AST_Decl::NT_interface_fwd:
      s << "CORBA::TypeCode::PR_interface_tc(\"" 
	<< o2be_interface_fwd::narrow_from_decl(decl)->repositoryID()
	<< "\", \"" << o2be_interface_fwd::narrow_from_decl(decl)->uqname()
	<< "\")";
      break;

    case AST_Decl::NT_except:
    case AST_Decl::NT_union:
    case AST_Decl::NT_struct:
    case AST_Decl::NT_enum:
    case AST_Decl::NT_typedef:
      s << "_0RL_tc_" << o2be_name::narrow_and_produce__idname(decl);
      break;

    case AST_Decl::NT_array:
      o2be_array::narrow_from_decl(decl)->produce_typecode_member(s);
      break;

    case AST_Decl::NT_sequence:
      o2be_sequence::narrow_from_decl(decl)->produce_typecode_member(s);
      break;

    default:
      throw o2be_internal_error(__FILE__,__LINE__,
				"Unrecognised argument type");
    }
}  


char const* o2be_name::variable_qualifier() {
  if( defined_in() == idl_global->root() )
    return "_CORBA_GLOBAL_VAR";
  else if( defined_in()->scope_node_type() == AST_Decl::NT_module )
    return "_CORBA_MODULE_VAR";
  else
    return "static _LC_attr";
}
