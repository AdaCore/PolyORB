/*************************************************************************************************
***                              ADA BACK-END COMPILER HEADER                                  ***
***                               file:  adabe.h                                               ***
***                                                                                            ***
***      This file is the single header file of all AdaBroker Back_End.                        ***
***   It contains all class and type definitions that are implemented in .cc files.            ***
***   Most of the class defined here are children of classes of the Sun Front-End. The rule    ***
***   is that a AST_Toto class in the front_end has a correspondant adabe_toto class in the    ***
***   AdaBroker Back-End.                                                                      ***
***                                                                                            ***
***   Copyright 1999                                                                           ***
***   Jean Marie Cottin, Laurent Kubler, Vincent Niebel                                        ***
***                                                                                            ***
***   This is free software; you can redistribute it and/or modify it under terms of the GNU   ***
***   General Public License, as published by the Free Software Foundation.                    ***
***                                                                                            ***
***  This back-end is distributed in the hope that it will be usefull, but WITHOUT ANY         ***
***  WARRANTY; without even the implied waranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR ***
***  PURPOSE.                                                                                  ***
***                                                                                            ***
***  See the GNU General Public License for more details.                                      ***
***                                                                                            ***
***                                                                                            ***
*************************************************************************************************/

#ifndef _ADABE_CLASSES_H_
#define _ADABE_CLASSES_H_

#include <string>
#include <stream.h> 
#include <idl.hh>
#include <idl_extern.hh>
#include "debug.h"

string spaces (int n, char d);
// return a string of n identical d chars
// used for indentation

char *lower (const char *str);
// Used to removed the upper case in a string

class string_list
{
 public:
  string_list ();
  ~string_list ();
  // constructor and destructor
  
  void add(string str);
  // add a string to the list
  
  bool check(string str);
  // check for the presence of the string in the list, and add it

  string *produce();
  string *produce(string);
  // dump the content of the list in a string
  
 private:
  int nb_item_in_list;    // number of strings in the list
  int max_item_in_list;   // dimension of the string array
  typedef string *str_ptr;
  str_ptr *str_list;
};

typedef string_list dep_list;



class adabe_name : public virtual AST_Decl
// Abstract class, root from the adabe classes
// contains the ada names, and some adabe-specific attributes 
{
public:
  adabe_name(AST_Decl::NodeType t,UTL_ScopedName* n, UTL_StrList* up);
  // the constructor of this class
  
  string get_ada_local_name(void); 
  // give the local ADA name of the AST node
  
  string get_ada_full_name(void);
  // give the complete ADA name of the AST node

  void set_ada_local_name(string); 
  // give the local ADA name of the AST node
  
  void set_ada_full_name(string);
  // give the complete ADA name of the AST node

  void compute_ada_name(void);
  // determine the ADA local and complete name
  
  bool is_already_defined();
  // has this object already been defined in the ada files ?
  
  void set_already_defined();
  // set already_defined to true

  void set_undefined();
  // set already_defined to false
  
  int is_name_already_used(string name, UTL_Scope *in_scope);
  // is this name already used in the current scope ?

  void add_number_to_type_name(adabe_name *type);
  // add a \x to the end of a type, if the name conflicts with another name

  bool has_fixed_size();
  // return true if the size of this element is fixed

  void no_fixed_size() ;
  // set fixed size to False and calls no_fixed_size of the parent
  
  bool is_imported(dep_list &with);
  // if the node is imported; 

  virtual string local_type (void);
  //  Used to compute the name of a type know as "local type";

  bool is_marshal_imported(dep_list &with);
  // if the node is imported and include the corresponding marshal file in "with"; 
  
  virtual void produce_ads(dep_list&, string&, string&);  
  virtual void produce_adb(dep_list&, string&, string&);
  // functions used to produce the body and specification for the main files
  // the dep-list contains the dependencies from other packages
  // the first string& is the body of the produce
  // the second contains the part that should be mapped before the body
  
  virtual void produce_impl_ads(dep_list&, string&, string&);
  virtual void produce_impl_adb(dep_list&, string&, string&);
  // functions used to produce the body and specification for the
  

  virtual void produce_proxies_ads(dep_list&, string&, string&);
  virtual void produce_proxies_adb(dep_list&, string&, string&);
  // functions used to produce the body and specification for the
  // proxy calls
  // the second string is here not used for previous addition
  // it is used to get only the IN or only the OUT argument of an operation
  
  virtual void produce_skel_ads(dep_list&, string&, string&);
  virtual void produce_skel_adb(dep_list&, string&, string&);
  // functions used to produce the body and specification for the
  // package used to select the right function called (dispatch)
  
  virtual void produce_marshal_ads(dep_list&, string&, string&);
  virtual void produce_marshal_adb(dep_list&, string&, string&);
  // function used to produce the body and specification for the
  // marshalling functions of the non predefined types

  virtual string dump_name(dep_list&, string&);
  virtual string marshal_name(dep_list&, string&);
  // this function drops the name in the return string,
  // and if necessary defines the type in the last ones (or the marshal function) 
  // add a dependency in the dep_list
  
  char *repositoryID(void) const;
  // this function give the repository ID of the object

  DEF_NARROW_FROM_DECL(adabe_name);
  DEF_NARROW_FROM_SCOPE(adabe_name);
  // narrowing functions (from AST_Decl or UTL_Scope

  
 private:
  char *pd_repositoryID;            // the repositoryID
  string pd_ada_local_name;        // the local name of the type or identifier
  string pd_ada_full_name;         // the full name ...
  bool pd_defined_type;            // is the type already defined
  
  void convert(string &);        
  // give the ADA name given by the OMG mapping rules of the AST node

  bool pd_fixed_size;
  // true if the size of this element is fixed

  //  bool is_reserved_name(void);
  // determines if the name of the node is an ADA reserved name
};

class adabe_predefined_type : public virtual AST_PredefinedType,
			     public virtual adabe_name
{
public:

  adabe_predefined_type(AST_PredefinedType::PredefinedType t,
		       UTL_ScopedName *n,
		       UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_predefined_type, AST_PredefinedType);
  DEF_NARROW_FROM_DECL(adabe_predefined_type);

  virtual void produce_ads (dep_list &with, string &body, string &previous);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);
 
private:
  string get_ada_predefined_type(void);
  //determine the ada name of the type
};


class adabe_constant : public virtual AST_Constant,
		      public virtual adabe_name
{
public:

  adabe_constant(AST_Expression::ExprType et,
		AST_Expression *v,
		UTL_ScopedName *n,
		 UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_constant, AST_Constant);
  DEF_NARROW_FROM_DECL(adabe_constant);

  virtual void produce_ads (dep_list &with, string &body, string &previous);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);
  
private:
  void  write_string_to_ada(string c_string, string &ada_string);

};

class adabe_enum : public virtual AST_Enum,
		  public virtual adabe_name
{
public:

  adabe_enum(UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_enum, AST_Enum);
  DEF_NARROW_FROM_DECL(adabe_enum);
  DEF_NARROW_FROM_SCOPE(adabe_enum);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);
    
};


class adabe_enum_val : public virtual AST_EnumVal,
		      public virtual adabe_name
{
public:

  adabe_enum_val(unsigned long v, UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_enum_val, AST_EnumVal);
  DEF_NARROW_FROM_DECL(adabe_enum_val);

  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);
  
};


class adabe_string : public virtual AST_String,
		    public virtual adabe_name
{
public:

  adabe_string(AST_Expression *v);
  adabe_string(AST_Expression *v, long wide);

  DEF_NARROW_METHODS1(adabe_string, AST_String);
  DEF_NARROW_FROM_DECL(adabe_string);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
  virtual string local_type (void);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);
  
};


class adabe_field : public virtual AST_Field,
		   public virtual adabe_name
{
public:

  adabe_field(AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_field, AST_Field);
  DEF_NARROW_FROM_DECL(adabe_field);

  virtual void produce_ads (dep_list &with, string &body, string &previous);
  void produce_marshal_adb(dep_list& with, string &body, string &marshall, string &unmarshall, string &align_size);

};


class adabe_union : public virtual AST_Union,
		   public virtual adabe_name
{
public:

  adabe_union(AST_ConcreteType *dt, UTL_ScopedName *n, UTL_StrList *p);
  
  DEF_NARROW_METHODS1(adabe_union, AST_Union);
  DEF_NARROW_FROM_DECL(adabe_union);
  DEF_NARROW_FROM_SCOPE(adabe_union);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);

};


class adabe_union_branch : public virtual AST_UnionBranch,
			  public virtual adabe_field
{
public:

  adabe_union_branch(AST_UnionLabel *lab, AST_Type *ft, UTL_ScopedName *n,
		  UTL_StrList *p);
  
  DEF_NARROW_METHODS1(adabe_union_branch, AST_UnionBranch);
  DEF_NARROW_FROM_DECL(adabe_union_branch);

  void produce_ads(dep_list &with, string &body, string &previous,
		   AST_ConcreteType* concrete);
  void produce_marshal_adb(dep_list &with,
			   string &marshall,
			   string &unmarshall,
			   string &align_size,
			   AST_ConcreteType *concrete);
};


class adabe_structure : public virtual AST_Structure,
		       public virtual adabe_name
{
public:

  adabe_structure(UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_structure, AST_Structure);
  DEF_NARROW_FROM_DECL(adabe_structure);
  DEF_NARROW_FROM_SCOPE(adabe_structure);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);
};


class adabe_exception : public virtual AST_Exception,
		       public virtual adabe_name
{
public:

  adabe_exception(UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_exception, AST_Exception);
  DEF_NARROW_FROM_DECL(adabe_exception);
  DEF_NARROW_FROM_SCOPE(adabe_exception);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_adb(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
  virtual void produce_skel_adb(dep_list &with, string &body);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);

};


class adabe_array : public virtual AST_Array,
		   public virtual adabe_name
{
public:

  adabe_array(UTL_ScopedName *n, unsigned long ndims, UTL_ExprList *dims);

  DEF_NARROW_METHODS1(adabe_array, AST_Array);
  DEF_NARROW_FROM_DECL(adabe_array);

  virtual void produce_ads (dep_list &with, string &body, string &previous);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
  virtual string local_type (void);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);

};


class adabe_sequence : public virtual AST_Sequence,
		      public virtual adabe_name
{
public:

  adabe_sequence(AST_Expression *v, AST_Type *bt);

  DEF_NARROW_METHODS1(adabe_sequence, AST_Sequence);
  DEF_NARROW_FROM_DECL(adabe_sequence);
  DEF_NARROW_FROM_SCOPE(adabe_sequence);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);
};


class adabe_argument : public virtual AST_Argument,
		      public virtual adabe_name
{
public:
  adabe_argument(AST_Argument::Direction d, AST_Type *ft, UTL_ScopedName *n,
		UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_argument, AST_Argument);
  DEF_NARROW_FROM_DECL(adabe_argument);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  void produce_adb(dep_list &with, bool &no_out, string space, string &in_decls, string &in_args, string &out_args);
  void produce_proxies_ads(dep_list &with, string &in_decls, bool &no_in, bool &no_out, string &fields, string &out_args);
  void produce_proxies_adb(dep_list &with, string &in_decls,
			   bool &no_in, bool &no_out, string &init,
			   string &align_size, string &marshall,
			   string &unmarshall_decls,
			   string &unmarshall, string &finalize,
			   string &out_args, string &result_decls);
  void produce_skel_adb(dep_list &with, string &in_decls , bool &no_in, bool no_out, string &unmarshall, string &call_args, string &marshall);
};


class adabe_attribute : public virtual AST_Attribute,
		       public virtual adabe_name
{
public:

  adabe_attribute(idl_bool ro, AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_attribute, AST_Attribute);
  DEF_NARROW_FROM_DECL(adabe_attribute);
  DEF_NARROW_FROM_SCOPE(adabe_attribute);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_adb(dep_list &with, string &body, string &previous);
  virtual void produce_impl_ads(dep_list &with, string &body, string &previous);
  virtual void produce_impl_adb(dep_list &with, string &body, string &previous);
  virtual void produce_proxies_ads(dep_list &with, string &body, string &private_definition);
  virtual void produce_proxies_adb(dep_list &with, string &body, string &private_definition);
  virtual void produce_skel_adb(dep_list &with, string &body, string &private_definition);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);

};


class adabe_operation : public virtual AST_Operation,
		       public virtual adabe_name
{
public:

  adabe_operation(AST_Type *rt, AST_Operation::Flags fl,
		 UTL_ScopedName *n,UTL_StrList *p);
  
  DEF_NARROW_METHODS1(adabe_operation, AST_Operation);
  DEF_NARROW_FROM_DECL(adabe_operation);
  DEF_NARROW_FROM_SCOPE(adabe_operation);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_adb(dep_list &with, string &body, string &previous);
  virtual void produce_impl_ads(dep_list &with, string &body, string &previous);
  virtual void produce_impl_adb(dep_list &with, string &body, string &previous);
  virtual void produce_proxies_ads(dep_list &with, string &body, string &private_definition);
  virtual void produce_proxies_adb(dep_list &with, string &body, string &private_definition);
  virtual void produce_skel_adb(dep_list &with, string &body, string &private_definition);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);


private:
  bool is_function();
  //to check if the operation is a function or not

  bool return_is_void();
  //is the return type void or not
};


class adabe_typedef : public virtual AST_Typedef,
		     public virtual adabe_name
{
public:

  adabe_typedef(AST_Type *bt, UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_typedef, AST_Typedef);
  DEF_NARROW_FROM_DECL(adabe_typedef);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);

};


class adabe_interface : public virtual AST_Interface,
		       public virtual adabe_name
{
 public:

  adabe_interface(UTL_ScopedName *n, AST_Interface **ih, long nih,
		  UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_interface, AST_Interface);
  DEF_NARROW_FROM_DECL(adabe_interface);
  DEF_NARROW_FROM_SCOPE(adabe_interface);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_adb(dep_list &with, string &body, string &previous);
  virtual void produce_impl_ads(dep_list &with, string &body, string &previous);
  virtual void produce_impl_adb(dep_list &with, string &body, string &previous);
  virtual void produce_proxies_ads(dep_list &with, string &body, string &private_definition);
  virtual void produce_proxies_adb(dep_list &with, string &body, string &private_definition);
  virtual void produce_skel_ads(dep_list &with, string &body, string &previous);
  virtual void produce_skel_adb(dep_list &with, string &body, string &private_definition);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
  virtual string dump_name(dep_list &with, string &previous);
  virtual string marshal_name(dep_list &with, string &previous);

  bool is_forwarded(void) { return pd_is_forwarded; };

 private:

  bool pd_is_forwarded;
  // is this interface forward declared?
};


class adabe_interface_fwd : public virtual AST_InterfaceFwd,
			   public virtual adabe_name
{
public:

  adabe_interface_fwd(UTL_ScopedName *n, UTL_StrList *p);
  
  DEF_NARROW_METHODS1(adabe_interface_fwd, AST_InterfaceFwd);
  DEF_NARROW_FROM_DECL(adabe_interface_fwd);
  DEF_NARROW_FROM_SCOPE(adabe_interface_fwd);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
};


class adabe_module : public virtual AST_Module,
		    public virtual adabe_name
{
public:
  adabe_module(UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS3(adabe_module, AST_Module, UTL_Scope, adabe_name);
  DEF_NARROW_FROM_DECL(adabe_module);
  DEF_NARROW_FROM_SCOPE(adabe_module);

  virtual void produce_ads(dep_list &with, string &body, string &previous);
  virtual void produce_adb(dep_list &with, string &body, string &previous);
  virtual void produce_impl_ads(dep_list &with, string &body, string &previous);
  virtual void produce_impl_adb(dep_list &with, string &body, string &previous);
  virtual void produce_proxies_ads(dep_list &with, string &body, string &private_definition);
  virtual void produce_proxies_adb(dep_list &with, string &body, string &private_definition);
  virtual void produce_skel_ads(dep_list &with, string &body, string &previous);
  virtual void produce_skel_adb(dep_list &with, string &body, string &private_definition);
  virtual void produce_marshal_ads(dep_list &with, string &body, string &previous);
  virtual void produce_marshal_adb(dep_list &with, string &body, string &previous);
};


class adabe_root :   public virtual AST_Root,
		     public virtual adabe_name
		
{
public:

  adabe_root(UTL_ScopedName *n, UTL_StrList *p);
  ~adabe_root() {}

  DEF_NARROW_METHODS1(adabe_root, AST_Root);
  DEF_NARROW_FROM_DECL(adabe_root);
  DEF_NARROW_FROM_SCOPE(adabe_root);

  void produce();
};


class adabe_global {

private:
  static adabe_root* myself;
 public:
  static adabe_name* pd_adabe_current_file;


  static void set_adabe_current_file(adabe_name *new_file) { pd_adabe_current_file = new_file; };
 
  static void set_root(adabe_root *v) { myself = v; };
  // set the root from the AST
  
  static adabe_root *root() { return myself; };
  
  static adabe_name *adabe_current_file() {
    return pd_adabe_current_file;
  };
  
  
  // which is the root ?

};


class adabe_generator : public AST_Generator {
public:
  virtual AST_Root *create_root(UTL_ScopedName *n,
				UTL_StrList *p);

  virtual AST_PredefinedType *create_predefined_type(AST_PredefinedType::PredefinedType t,
						     UTL_ScopedName *n,
						     UTL_StrList *p);

  virtual AST_Module *create_module(UTL_ScopedName *n, UTL_StrList *p);

  virtual AST_Interface	*create_interface(UTL_ScopedName *n,
					  AST_Interface **ih,
					  long nih,
					  UTL_StrList *p);

  virtual AST_InterfaceFwd *create_interface_fwd(UTL_ScopedName *n,
						 UTL_StrList *p);

  virtual AST_Exception	*create_exception(UTL_ScopedName *n, UTL_StrList *p);

  virtual AST_Structure	*create_structure(UTL_ScopedName *n, UTL_StrList *p);

  virtual AST_Enum *create_enum(UTL_ScopedName *n, UTL_StrList *p);

  virtual AST_Operation	*create_operation(AST_Type *rt,
					  AST_Operation::Flags fl,
					  UTL_ScopedName *n,
					  UTL_StrList *p);
  
  virtual AST_Field *create_field(AST_Type *ft,
				  UTL_ScopedName *n,
				  UTL_StrList *p);

  virtual AST_Argument *create_argument(AST_Argument::Direction d,
					AST_Type *ft,
					UTL_ScopedName *n,
					UTL_StrList *p);

  virtual AST_Attribute	*create_attribute(idl_bool ro,
					  AST_Type *ft,
					  UTL_ScopedName *n,
					  UTL_StrList *p);

  virtual AST_Union *create_union(AST_ConcreteType *dt,
				  UTL_ScopedName *n,
				  UTL_StrList *p);

  virtual AST_UnionBranch *create_union_branch(AST_UnionLabel *lab,
					       AST_Type *ft,
					       UTL_ScopedName *n,
					       UTL_StrList *p);

  virtual AST_Constant *create_constant(AST_Expression::ExprType et,
					AST_Expression *ev,
					UTL_ScopedName *n,
					UTL_StrList *p);

  virtual AST_EnumVal *create_enum_val(unsigned long v,
				       UTL_ScopedName *n,
				       UTL_StrList *p);
  
  virtual AST_Array *create_array(UTL_ScopedName *n,
				  unsigned long ndims,
				  UTL_ExprList *dims);

  virtual AST_Sequence *create_sequence(AST_Expression *v, AST_Type *bt);

  virtual AST_String *create_string(AST_Expression *v);

  virtual AST_Typedef *create_typedef(AST_Type *bt,
				      UTL_ScopedName *n,
				      UTL_StrList *p);
};

/***********************************************************************************************
****                                   Lists of exceptions                                  ****
***********************************************************************************************/
												
class adabe_unsupported {
  // exception raised when the back end encounters an IDL facility not supported (any )
public:
  adabe_unsupported(const char* idlfile,int line,const char* msg) {
    pd_file = idlfile;
    pd_line = line;
    pd_msg = msg;
  }
  ~adabe_unsupported() {}
  const char* file() const { return pd_file; }
  int line() const { return pd_line; }
  const char* msg() const { return pd_msg; }
private:
  const char* pd_file;
  int	pd_line;
  const char* pd_msg;
  adabe_unsupported();
};


class adabe_internal_error {
  // exception raised when something has gone wrong in the back end (type not expected ...)
public:
  adabe_internal_error(const char* file,int line,const char* errmsg) {
    pd_file = file;
    pd_line = line;
    pd_errmsg = errmsg;
  }
  ~adabe_internal_error(){}
  const char* file() const { return pd_file; }
  int line() const { return pd_line; }
  const char* errmsg() const { return pd_errmsg; }
private:
  const char* pd_file;
  int	      pd_line;
  const char* pd_errmsg;
  adabe_internal_error();
};


class adabe_fileio_error {
  // exception raised when an error has occured during an input/output
public:
  adabe_fileio_error(const char* errmsg) {
    pd_errmsg = errmsg;
  }
  ~adabe_fileio_error() {}
  const char* errmsg() const { return pd_errmsg; }
private:
  const char* pd_errmsg;
  adabe_fileio_error();
};

ostream& operator<<(ostream &s, AST_Decl::NodeType x);

#endif

