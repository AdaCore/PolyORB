//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.35 $
//                                                                          //
//         Copyright (C) 1999-2000 ENST Paris University, France.           //
//                                                                          //
// AdaBroker is free software; you  can  redistribute  it and/or modify it  //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. AdaBroker  is distributed  in the hope that it will be  useful, //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with AdaBroker; see file COPYING. If  //
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, //
// Boston, MA 02111-1307, USA.                                              //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//             AdaBroker is maintained by ENST Paris University.            //
//                     (email: broker@inf.enst.fr)                          //
//                                                                          //
//--------------------------------------------------------------------------//
#include <adabe.h>
#include <stdio.h>
#include <nr_extern.hh>

// used in this file to add _i at the end of
// forbidden names
#ifdef INT_MAX
#define MAX_NUMBER_OF_EXTENSIONS INT_MAX
#else
#define MAX_NUMBER_OF_EXTENSIONS 100
#endif


static char* internal_produce_repositoryID (AST_Decl* decl, adabe_name* n);
static UTL_String* internal_search_pragma (AST_Decl* decl, char* p);

adabe_name::adabe_name (AST_Decl::NodeType t,
			UTL_ScopedName* n,
			UTL_StrList* up)
  : AST_Decl (t, n, up)
{
  // Initialisation of the variables
  pd_ada_local_name = "";
  pd_ada_full_name  = "";

  // initialise the flag
  pd_defined_type   = 0;
  
  switch (node_type ())
    {
      // if the size of the type may vary, put the
      // fixed_false to false
    case AST_Decl::NT_string :
    case AST_Decl::NT_sequence :
    case AST_Decl::NT_union :
      pd_fixed_size = false;
      break;
    default:
      // else put it to true
      pd_fixed_size = true;
      break;
    }

  // compute the repository ID name (it is based on the IDL
  // name and not on the ADA name)
  pd_repositoryID = internal_produce_repositoryID (this, this);

  // when the initialisation is complete, compute the
  // ada name
  compute_ada_name ();
};

string
adabe_name::get_ada_local_name ()
{
  return pd_ada_local_name;
} 

string
adabe_name::get_ada_full_name ()
{
  return pd_ada_full_name;
}

void
adabe_name::set_ada_local_name (string s)
{
  pd_ada_local_name = s;
} 

void
adabe_name::set_ada_full_name (string s)
{
  pd_ada_full_name = s;
}

// The following function are here to ensure that no function will be
// called on a type that must not be mapped.
void 
adabe_name::produce_ads (dep_list&, string&, string&)
{
  cerr << "node type is : " << (dynamic_cast<AST_Decl *>(this))->node_type () << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"produce_ads called in adabe_name");
}

void 
adabe_name::produce_adb (dep_list&, string&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"produce_adb called in adabe_name");
}

void 
adabe_name::produce_impl_ads (dep_list&, string&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error (__FILE__,__LINE__,"produce_impl_ads called in adabe_name");
}

void 
adabe_name::produce_impl_adb (dep_list&, string&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"produce_impl_adb called in adabe_name");
}

void 
adabe_name::produce_proxy_ads (dep_list&, string&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"produce_proxy_ads called in adabe_name");
}

void 
adabe_name::produce_proxy_adb (dep_list&, string&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"produce_proxy_adb called in adabe_name");
}

void 
adabe_name::produce_skel_ads (dep_list&, string&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"produce_skel_ads called in adabe_name");
}

void 
adabe_name::produce_skel_adb (dep_list&, string&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"produce_skel_adb called in adabe_name");
}

void 
adabe_name::produce_stream_ads (dep_list&, string&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"produce_stream_ads called in adabe_name");
}

void 
adabe_name::produce_stream_adb (dep_list&, string&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"produce_stream_adb called in adabe_name");
}

string 
adabe_name::dump_name (dep_list&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"dump_name called in adabe_name");
}

string
adabe_name::local_type (void)
{
  throw adabe_internal_error
    (__FILE__,__LINE__,"Unexpected local type");
}

string 
adabe_name::marshal_name (dep_list&, string&)
{
  cerr << "node type is : "
       << (dynamic_cast<AST_Decl *>(this))->node_type ()
       << endl;
  throw adabe_internal_error
    (__FILE__,__LINE__,"marshal_name called in adabe_name");
}


/////////////////////////////////////////
//////// compute_ada_name      //////////
/////////////////////////////////////////

void
adabe_name::compute_ada_name ()
{
  if (node_type () == AST_Decl::NT_root) 
    {
      // if the node is the root, the name is given
      // with the IDL file name
      
      string name      = idl_global->stripped_filename ()->get_string ();
      // it is the file name
      
      int end_of_name = name.find (".idl");
      if (end_of_name > 0) name = name.substr (0, end_of_name);
      string idl_file_name = "";
      idl_file_name =  name + "_IDL_FILE";

      // compute the ada name
      set_ada_local_name (idl_file_name);
      set_ada_full_name (idl_file_name);

      // then exit the function
      return;
    }
  
  string temp_name = local_name ()->get_string ();
  // it is the idl name
  
  if ((get_ada_local_name () == "") || (temp_name ==  "local type"))
    {
      // the following must be done only if there's no local name
      // or if the name is local type (the compute name can have
      // given a wrong name in the initialisation

      // initialisation of the variables
      pd_ada_full_name = "";
      int already_used;
      int loop = 0;
      
#ifdef DEBUG_NAME
      cout << "in adabe_name, before convert" << endl;
#endif
      // if the type is a local type, a special
      // function is applied if the node type
      // is string or array. (sequences can
      // also be local type, but they don't are
      // a problem).
      if (temp_name == "local type")
	switch (node_type ())
	  {
	  case AST_Decl::NT_array:
	  case AST_Decl::NT_string:
	    temp_name = local_type ();
	    break;
	  default:
	    break;
	  }

#ifdef DEBUG_NAME
      cout << "in adabe_name. The idl name is" << temp_name;
#endif
      convert (temp_name);
      // Looks if the name is valid in ADA, and
      // if not, convert it to an acceptable name
      
#ifdef DEBUG_NAME
      cout << " and the ada name is " << temp_name <<endl;
#endif

      // set the local to temp_name before checking for
      // conflicts
      
      pd_ada_local_name = temp_name;

      // we now must look in the containing scope
      // to verify that the name does not conflict
      UTL_Scope *parent_scope = defined_in ();
      if (parent_scope != NULL) 
	do {
	  switch (parent_scope->scope_node_type ())
	    {
	    case AST_Decl::NT_op:
	    case AST_Decl::NT_enum:
	    case AST_Decl::NT_except:
	    case AST_Decl::NT_struct:
	    case AST_Decl::NT_union:
	    case AST_Decl::NT_interface:
	    case AST_Decl::NT_module:
	    case AST_Decl::NT_root:
	      // list of node that can have nodes declared
	      // inside
	      
	      pd_ada_full_name = 
		(dynamic_cast<adabe_name *>(parent_scope))->get_ada_full_name ();

	      // is this name already used ?
	      already_used =
		is_name_already_used (pd_ada_local_name, parent_scope);
	      break;
	      
	    default:
	      // if the node type is none of the previous ones throw an exception
	      throw adabe_internal_error
		(__FILE__,__LINE__,"unexpected contening scope");
	    }

	  // if the name is already used add a string at the
	  // end of the name
	  if (already_used == 1)
	    {
	      // no name can be found:
	      // bailling out
	      if (loop >  MAX_NUMBER_OF_EXTENSIONS)
		throw adabe_internal_error
		  (__FILE__,__LINE__,"too many name conflicts");
	      char extension[10];

	      // adding an extension to the name,
	      // before continuing
	      sprintf (extension, "_%d", loop++);
	      pd_ada_local_name = temp_name + extension;
	    }

	  // If the node is an interface or a Module or an
	  // InterfaceFwd (all the typed that are mapped in individual
	  // files)
	  if ((node_type () != AST_Decl::NT_module) &&
	      (node_type () != AST_Decl::NT_interface)
	      && (node_type () != AST_Decl::NT_interface_fwd))
	    if (pd_ada_full_name != "")   //  perhaps useless
	      pd_ada_full_name = pd_ada_full_name + "." + pd_ada_local_name;
	    else  
	      {
		pd_ada_full_name = pd_ada_local_name; // perhaps useless
	      }
	  else
	    // the nodes contained in the root does not need to have a
	    // full-name
	    if (parent_scope->scope_node_type () != AST_Decl::NT_root)
	      pd_ada_full_name = pd_ada_full_name + "." + pd_ada_local_name;
	    else  pd_ada_full_name = pd_ada_local_name;

	} 
	while (already_used == 1);      
    }
}


/////////////////////////////////
///// is_name_already_used //////
/////////////////////////////////

int
adabe_name::is_name_already_used (string name, UTL_Scope *in_scope)
{
  // this function looks in the scope in_scope a node that has the
  // same name
  UTL_ScopeActiveIterator i (in_scope, UTL_Scope::IK_decls);
  while (!i.is_done ())
    {
      string temp;
      AST_Decl *d = i.item ();
      i.next ();
      if (d->in_main_file ())
	{
	  // for all the file in the node compares the ADA names
	  adabe_name *adanode = dynamic_cast<adabe_name *>(d);;
	  temp = (adanode->get_ada_local_name ());

	  // if there's a node other than this one which has the
	  // same name, the function returns 1
	  if ((this != adanode) && (temp  != "")
	      && (temp == name))
	    return 1;
	}
    }
  // no problem has been found
  // returns 0
  return 0;
} 

bool
adabe_name::is_already_defined ()
{
  return pd_defined_type;
}

void
adabe_name::set_already_defined ()
{
  pd_defined_type = true;
}


/////////////////////////////
///////// convert ///////////
/////////////////////////////

void
adabe_name::convert (string &name)
{
  // first we must remove the '_' at the beginning
  // of the name
  while (name[0]=='_')
    {
      string temp = name.substr (1, name.length ());
      name = temp;
    }
  
  int c;
  // to '_' that follow each other must be transformed
  // into "_U"
  while ((c = name.find ("__")) != -1) 
    name[c+1]='U';
  // Eliminates the '__'

  // then, the '_' at the end of the name is
  // transformed in a U
  if (name[name.length ()-1]=='_')
      name += 'U';

  // since now, ther isn't any '_' at the end or at the
  // beginning of the name. We must now verify that the
  // name isn't an ADA95 keyword
  
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
  // if so, the suffix _IDL is added

  // we are now sure that the name is valid in ADA
};

///////////////////////////////
//////// set_undefined ////////
///////////////////////////////

void
adabe_name::set_undefined ()
{
  // this function set the pd_defined_type flag to false
  // and do the same for all node into it (recursively)
  pd_defined_type = 0;

  switch (node_type ())
    {
    case AST_Decl::NT_op:
      {
	UTL_ScopeActiveIterator i (adabe_operation::narrow_from_decl (this), UTL_Scope::IK_both);
	while (!i.is_done ())
	  {
	    AST_Decl *d = i.item ();
	    i.next ();
	    dynamic_cast<adabe_name *>(d)->set_undefined ();
	  };
      }
      break;
    case AST_Decl::NT_interface:
      {
	UTL_ScopeActiveIterator i (adabe_interface::narrow_from_decl (this),
				   UTL_Scope::IK_both);
	while (!i.is_done ())
	  {
	    AST_Decl *d = i.item ();
	    i.next ();
	    dynamic_cast<adabe_name *>(d)->set_undefined ();
	  };
      }
      break;
    case AST_Decl::NT_module:
      {
	UTL_ScopeActiveIterator i (adabe_module::narrow_from_decl (this),
				   UTL_Scope::IK_both);
	while (!i.is_done ())
	  {
	    AST_Decl *d = i.item ();
	    i.next ();
	    dynamic_cast<adabe_name *>(d)->set_undefined ();
	  };
      }
      break;
      
    case AST_Decl::NT_root:
      {
	UTL_ScopeActiveIterator i (adabe_root::narrow_from_decl (this),
				   UTL_Scope::IK_both);
	while (!i.is_done ())
	  {
	    AST_Decl *d = i.item ();
	    i.next ();
	    if (d->in_main_file ())
	      {
		dynamic_cast<adabe_name *>(d)->set_undefined ();
	      }
	  };
      }
      break;
    case AST_Decl::NT_except:
      {
	UTL_ScopeActiveIterator i (adabe_exception::narrow_from_decl (this),
				   UTL_Scope::IK_both);
	while (!i.is_done ())
	  {
	    AST_Decl *d = i.item ();
	    i.next ();
	    dynamic_cast<adabe_name *>(d)->set_undefined ();
	  };
      }
      break;
    case AST_Decl::NT_struct:
      {
	UTL_ScopeActiveIterator i (adabe_structure::narrow_from_decl (this),
				   UTL_Scope::IK_both);
	while (!i.is_done ())
	  {
	    AST_Decl *d = i.item ();
	    i.next ();
	    dynamic_cast<adabe_name *>(d)->set_undefined ();
	  };
      }
      break;
    case AST_Decl::NT_union:
      {
	UTL_ScopeActiveIterator i (adabe_union::narrow_from_decl (this),
				   UTL_Scope::IK_both);
	while (!i.is_done ())
	  {
	    AST_Decl *d = i.item ();
	    i.next ();
	    dynamic_cast<adabe_name *>(d)->set_undefined ();
	  };
      }
      break;
    default:
      //...  else do nothing
      break;
    }
}

////////////////////////
///// is_imported //////
////////////////////////

bool
adabe_name::is_imported (dep_list& with)
{
  if (node_type () == AST_Decl::NT_pre_defined) return 0;
    // the predefined type are not in main file
    // but they are declared in th root and not imported
    // so the root must not be included in those cases
  
  if (this == adabe_global::adabe_current_file ())
    // we are in athe scope that has defined the type
    // this type isn't imported
    return 0;
  
  AST_Decl::NodeType NT = node_type ();

  // if the node type is an interface, a module
  // or the root, the type is imported
  if (NT == AST_Decl::NT_interface)
    {
      bool temp;
      adabe_interface *inter = dynamic_cast<adabe_interface *>(this);
      if ((string) local_name ()->get_string () == "Object") 
	{
	  with.add ("CORBA.Object");
	}
      else
	if (inter->is_forwarded ())
	  {
	    // if the interface is forwarded, the full
	    // name of the file is the interface
	    // name+"_forward"
	    with.add (get_ada_full_name () + "_Forward");
	  }
	else
	  {
	    // else simply add the interface file
	    with.add (get_ada_full_name ());
	  }
      return 1;
    }
  if (NT == AST_Decl::NT_module)
    {
      // add the module file name to the dependency list
      with.add (get_ada_full_name ());
      return 1;
    }
  if (NT == AST_Decl::NT_root)
    {
      // add the root file name to the dependency list      
      with.add (get_ada_full_name ());
      return 1;
    }
  
  
  // else go to the containing scope
  return (dynamic_cast<adabe_name *>(defined_in ()))->is_imported (with); 

}

//////////////////////////////////
////// is_marshal_imported ///////
//////////////////////////////////
bool
adabe_name::is_marshal_imported (dep_list& with)
{
  // this function is the same as the previous one
  // except, that the added file is the same
  // with a ".marshal" at the end
  if (node_type () == AST_Decl::NT_pre_defined)
    {
      return 0;
    }
  if (this == adabe_global::adabe_current_file ()) 
    return 0;
  AST_Decl::NodeType NT = node_type ();
  if (NT == AST_Decl::NT_interface)
    {
      if ((string) local_name ()->get_string () == "Object")  return 1;
      else
	with.add (get_ada_full_name ()+".Stream");
      return 1;
    }
  if (NT == AST_Decl::NT_module)
    {
      with.add (get_ada_full_name ()+".Stream");
      return 1;
    }
   if (NT == AST_Decl::NT_root)
    {
      with.add (get_ada_full_name ()+".Stream");
      return 1;
    }
   return (dynamic_cast<adabe_name *>(defined_in ()))->is_marshal_imported (with); 
}

///////////////////////////////////////
////// Output for the node_type ///////
///////////////////////////////////////

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
adabe_name::has_fixed_size ()
{
  // return true if the size of this element is fixed
  return pd_fixed_size;
};

void
adabe_name::no_fixed_size ()
{
  // set fixed size to False
  pd_fixed_size = false;
};


/////////////////////////////////////////////
///////// repository ID functions ///////////
/////////////////////////////////////////////

static
char*
internal_produce_scope_name (UTL_ScopedName *n, char *separator)
{
#define DEFAULT_FQN_SIZE 128
  int  nsep = (separator == NULL) ? 0 : strlen (separator);
  int  n_alloc = DEFAULT_FQN_SIZE;
  int  n_used  = 1;
  char *p = new char[n_alloc];
  *p = '\0';

  UTL_ScopedNameActiveIterator iter (n);
  Identifier *id;
  char *q;

  Identifier *last = n->last_component ();
  id = iter.item ();
  while (!iter.is_done () && id != last)
    {
      q = id->get_string ();
      if (strlen (q) != 0)
	{
	  n_used += strlen (q) + nsep;
	  while (n_used > n_alloc)
	    {
	      n_alloc += DEFAULT_FQN_SIZE;
	      char *t = new char[n_alloc];
	      strcpy (t, p);
	      delete [] p;
	      p = t;
	    }
	  strcat (p, q);
	  if (nsep)
	    strcat (p, separator);
	  if (q != id->get_string ())
	    delete [] q;
	}
      iter.next ();
      id = iter.item ();
    }
  return p;
}

char*
adabe_name::repositoryID () const 
{
  // Check the pragmas attached to this node to see if
  // pragma ID is defined to override the default repositoryID.
  UTL_String* id;
  if ((id = internal_search_pragma ((AST_Decl*)this,"ID")) != 0) 
    {
    return id->get_string ();
    }
  else if ((id = internal_search_pragma ((AST_Decl*)this,"version")) != 0) 
    {
      // Check if pragma version is defined to override the
      // version number in the default repositoryID.
      char* p = strrchr (pd_repositoryID,':') + 1;
      char* result = new char[(p-pd_repositoryID)+strlen (id->get_string ())+1];
      strncpy (result, pd_repositoryID, p-pd_repositoryID);
      result[p-pd_repositoryID] = '\0';
      strcat (result, id->get_string ());
      return result;
    }
  else 
    {
      return pd_repositoryID;
    }
}

static
char*
internal_produce_repositoryID (AST_Decl *decl, adabe_name *n)
{
  // find if any pragma prefix applies to this node. 
  switch (decl->node_type ())
    {
    case AST_Decl::NT_interface:
    case AST_Decl::NT_except:
      {
	UTL_String* prefix;
	UTL_Scope*  prefix_scope = 0;
	
	if (!(prefix = internal_search_pragma (decl,"prefix"))) 
	  {
	    // no pragma prefix defined in this node, search the parents
	    prefix_scope = decl->defined_in ();
	    while (prefix_scope) 
	      {
		if (!(prefix = internal_search_pragma (ScopeAsDecl (prefix_scope),
						      "prefix"))) 
		  {
		    prefix_scope = ScopeAsDecl (prefix_scope)->defined_in ();
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
	    idlist = new UTL_ScopedName (decl->local_name (), NULL);
	    UTL_Scope* u = decl->defined_in ();
	    while (prefix_scope && prefix_scope != u) 
	      {
		idlist = new UTL_ScopedName (ScopeAsDecl (u)->local_name (), idlist);
		u = ScopeAsDecl (u)->defined_in ();
	      }
	    idlist = new UTL_ScopedName (new Identifier (prefix->get_string ()), idlist);
	    
	  }
	else 
	  {
	    idlist = decl->name ();
	  }
	{
	  char* q = internal_produce_scope_name (idlist,"/");
	  char* result = new char [strlen ("IDL::1.0") + 
				  strlen (q) +
				  strlen (decl->local_name ()->get_string ()) + 1];
	  strcpy (result,"IDL:");
	  strcat (result, q);
	  strcat (result, decl->local_name ()->get_string ());
	  strcat (result,":1.0");
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
internal_search_pragma (AST_Decl* decl, char* p) 
{
  UTL_String* result = 0;
  UTL_StrlistActiveIterator l (decl->pragmas ());
  while (!l.is_done ()) {
    if (strcmp (p, l.item ()->get_string ()) == 0) 
      {
	l.next ();
	result = l.item ();
      }
    else 
      {
	l.next ();
      }
    l.next ();
  }
  return result;
}

char *lower (const char *str)
{
  char *new_str = new char[strlen (str)+1];
  strcpy (new_str, str);
  for (unsigned int i = 0; i < strlen (str); i++) 
    {
      if ((str[i] > 64) && ( str[i] < 91))
	new_str[i] = new_str[i] + 32;
    }
  return new_str;
}

string spaces (int n, char d)
{
  string tmp = "";
  int i;
  for (i=0;i<n;i++) tmp+=d;
  return tmp;
}

IMPL_NARROW_FROM_DECL (adabe_name)
IMPL_NARROW_FROM_SCOPE (adabe_name)



#undef MAX_NUMBER_OF_EXTENSIONS




