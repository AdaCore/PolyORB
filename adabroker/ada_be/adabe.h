#ifndef _ADABE_CLASSES_H_
#define _ADABE_CLASSES_H_

#include <string.h>
class string_list
{
 public:
  string_list();
  ~string_list();
  // constructor and destructor
  add (string str);
  // add a string to the list
  bool check (string str);
  // check for the presence of the string in the list, and add it 
  string produce();
  // dump the content of the list in a string
 private:
  string *list;
  int nb_item_in_list;
  int max_item_in_list;
}

typedef string_list dep_list;

class adabe_name : public virtual AST_Decl
{
public:
  adabe_name(AST_Decl::NodeType t,UTL_ScopedName* n, UTL_StrList* p);

  char *get_ada_local_name(void); 
  // give the local ADA name of the AST node
  
  char *get_ada_full_name(void);
  // give the complete ADA name of the AST node

  void compute_ada_names(void);
  // determine the ADA local and complete name
  bool is_already_defined();
  void set_already_defined();
private:
  adabe_name();
  char *pd_ada_local_name;
  char *pd_ada_full_name;
  bool pd_defined_type;
  void convert(void);
  // give the ADA name given by the OMG mapping rules of the AST node

  bool is_reserved_name(void);
  // determines if the name of the node is an ADA reserved name
};


class adabe_typedef;
class adabe_sequence;
class adabe_sequence_chain : public virtual AST_Decl {
public:
  adabe_sequence_chain(AST_Decl::NodeType t,UTL_ScopedName* n, UTL_StrList* p) :
    AST_Decl(t,n,p) { pd_seq_decl = 0; }
  virtual ~adabe_sequence_chain() {}
  void set_seq_decl(adabe_sequence *d);
  adabe_sequence *get_seq_decl()  { return pd_seq_decl; }
  void produce_seq_hdr_if_defined(std::fstream& s);
private:
  adabe_sequence_chain();
  adabe_sequence *pd_seq_decl;
};


class adabe_predefined_type : public virtual AST_PredefinedType,
			     public virtual adabe_name,
			     public virtual adabe_sequence_chain
{
public:

  adabe_predefined_type(AST_PredefinedType::PredefinedType t,
		       UTL_ScopedName *n,
		       UTL_StrList *p);
  //constructor
  DEF_NARROW_METHODS1(adabe_predefined_type, AST_PredefinedType);
  DEF_NARROW_FROM_DECL(adabe_predefined_type);

  void produce_ads (dep_list with,string &String, string &previousdefinition);
  void produce_adb (dep_list with,string &String, string &previousdefinition);
  void produce_impl_ads (dep_list with,string &String, string &previousdefinition);
  void produce_impl_adb (dep_list with,string &String, string &previousdefinition);
  //produce the ada name of the type
  
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

  void produce_ads (std::fstream& s, adabe_typedef* tdef);
  void produce_adb (std::fstream& s, adabe_typedef* tdef);
  void produce_impl_ads (std::fstream& s, adabe_typedef* tdef);
  void produce_impl_adb (std::fstream& s, adabe_typedef* tdef);

private:
  adabe_constant();

};


class adabe_enum : public virtual AST_Enum,
		  public virtual adabe_name,
		  public virtual adabe_sequence_chain
{
public:

  adabe_enum(UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_enum, AST_Enum);
  DEF_NARROW_FROM_DECL(adabe_enum);
  DEF_NARROW_FROM_SCOPE(adabe_enum);

  void produce_ads(std::fstream& s);
  void produce_ads(std::fstream& s);
  void produce_impl_ads(std::fstream& s);
  void produce_impl_adb(std::fstream& s);
  
private:
  adabe_enum();
};


class adabe_enum_val : public virtual AST_EnumVal,
		      public virtual adabe_name
{
public:

  adabe_enum_val(unsigned long v, UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_enum_val, AST_EnumVal);
  DEF_NARROW_FROM_DECL(adabe_enum_val);

private:
  adabe_enum_val();
};


class adabe_string : public virtual AST_String,
		    public virtual adabe_name,
		    public virtual adabe_sequence_chain
{
public:

  adabe_string(AST_Expression *v);
  adabe_string(AST_Expression *v, long wide);

  DEF_NARROW_METHODS1(adabe_string, AST_String);
  DEF_NARROW_FROM_DECL(adabe_string);

  static const char* fieldMemberTypeName();

private:
  adabe_string();

};


class adabe_field : public virtual AST_Field,
		   public virtual adabe_name
{
public:

  adabe_field(AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p);
  // constructor
  produce_ads (dep_list with,string &String, string &previousdefinition);
  //produce a field in the header
  produce_adb (dep_list with,string &String, string &previousdefinition);    
  //produce a field in the body
  produce_impl_ads (dep_list with,string &String, string &previousdefinition); /////useless
  //produce a field in the implementation header
  produce_impl_adb (dep_list with,string &String, string &previousdefinition); ////// useless   
  //produce a field in the implementation body

  DEF_NARROW_METHODS1(adabe_field, AST_Field);
  DEF_NARROW_FROM_DECL(adabe_field);


};


class adabe_union : public virtual AST_Union,
		   public virtual adabe_name,
		   public virtual adabe_sequence_chain
{
public:

  adabe_union(AST_ConcreteType *dt, UTL_ScopedName *n, UTL_StrList *p);
  //constructor
  
  DEF_NARROW_METHODS1(adabe_union, AST_Union);
  DEF_NARROW_FROM_DECL(adabe_union);
  DEF_NARROW_FROM_SCOPE(adabe_union);

  void produce_ads(dep_list with,string &String, string &previousdefinition);
  //produce an union in the header
  void produce_adb(dep_list with,string &String, string &previousdefinition);
  //produce the name of the union in the body
  void produce_impl_ads(dep_list with,string &String, string &previousdefinition);
  //produce an union in the implementation header
  void produce_adb(dep_list with,string &String, string &previousdefinition); 
  //produce the name of the union in the implementation body
  
  //  void produce_adb(std::fstream& s);
  //  void produce_impl_ads(std::fstream& s);
  //  void produce_impl_adb(std::fstream& s);

};


class adabe_union_branch : public virtual AST_UnionBranch,
			  public virtual adabe_field
{
public:

  adabe_union_branch(AST_UnionLabel *lab, AST_Type *ft, UTL_ScopedName *n,
		  UTL_StrList *p);
  //constructor
  
  produce_ads(dep_list, string, string, AST_Concretetype*);
  //produce a branch of the union in the header
  produce_impl_ads(dep_list, string, string, AST_Concretetype*); //////////// useless
  //produce a branch of the union in the implementation header

  DEF_NARROW_METHODS1(adabe_union_branch, AST_UnionBranch);
  DEF_NARROW_FROM_DECL(adabe_union_branch);

private:
  string produce_disc_value(AST_ConcreteType, AST_Expression)  
  //produce the value of the branch

};


class adabe_structure : public virtual AST_Structure,
		       public virtual adabe_name,
		       public virtual adabe_sequence_chain
{
public:

  adabe_structure(UTL_ScopedName *n, UTL_StrList *p);
  //constructor
  DEF_NARROW_METHODS1(adabe_structure, AST_Structure);
  DEF_NARROW_FROM_DECL(adabe_structure);
  DEF_NARROW_FROM_SCOPE(adabe_structure);

  void produce_ads(dep_list with,string &String, string &previousdefinition);
  //produce the structure in the header
  void produce_adb(dep_list with,string &String, string &previousdefinition);
  //produce the name of the structure in the body
  void produce_impl_ads(dep_list with,string &String, string &previousdefinition);
  //produce the structure in the implementation header
  void produce_impl_adb(dep_list with,string &String, string &previousdefinition);
  //produce the name of the structure in the implementation body
  string dump_name(dep_list with,string &String, string &previousdefinition);
  //produce the name of the structure

};


class adabe_exception : public virtual AST_Exception,
		       public virtual adabe_name
{
public:

  adabe_exception(UTL_ScopedName *n, UTL_StrList *p);

  DEF_NARROW_METHODS1(adabe_exception, AST_Exception);
  DEF_NARROW_FROM_DECL(adabe_exception);
  DEF_NARROW_FROM_SCOPE(adabe_exception);

  void produce_ads(std::fstream& s);
  void produce_adb(std::fstream& s);
  void produce_impl_ads(std::fstream& s);
  void produce_impl_adb(std::fstream& s);

private:
  adabe_exception();
};


class adabe_typedef;
class adabe_array : public virtual AST_Array,
		   public virtual adabe_name
{
public:

  adabe_array(UTL_ScopedName *n, unsigned long ndims, UTL_ExprList *dims);

  DEF_NARROW_METHODS1(adabe_array, AST_Array);
  DEF_NARROW_FROM_DECL(adabe_array);

  void produce_ads (std::fstream& s, adabe_typedef* tdef);
  void produce_adb(std::fstream& s, adabe_typedef* tdef);
  void produce_impl_ads(std::fstream& s, adabe_typedef* tdef);
  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);



private:
  adabe_array();

};


class adabe_sequence : public virtual AST_Sequence,
		      public virtual adabe_name,
		      public virtual adabe_sequence_chain
{
public:

  adabe_sequence(AST_Expression *v, AST_Type *bt);
  ~adabe_sequence() {}

  DEF_NARROW_METHODS1(adabe_sequence, AST_Sequence);
  DEF_NARROW_FROM_DECL(adabe_sequence);
  DEF_NARROW_FROM_SCOPE(adabe_sequence);

  void produce_ads(std::fstream& s);
  void produce_adb(std::fstream& s);
  void produce_impl_ads(std::fstream& s);
  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);

private:
  adabe_sequence();
};


class adabe_argument : public virtual AST_Argument,
		      public virtual adabe_name
{
public:
  adabe_argument(AST_Argument::Direction d, AST_Type *ft, UTL_ScopedName *n,
		UTL_StrList *p);
  //constructor
  produce_ads(dep_list with,string &String, string &previousdefinition);
  //produce an argument of an operation in the header
  produce_adb(dep_list with,string &String, string &previousdefinition);
  //produce an argument of an operation in the body
  produce_impl_ads(dep_list with,string &String, string &previousdefinition); 
  //produce an argument of an operation in the implementation header
  produce_impl_adb(dep_list with,string &String, string &previousdefinition);
  //produce an argument of an operation in the implementation body

  DEF_NARROW_METHODS1(adabe_argument, AST_Argument);
  DEF_NARROW_FROM_DECL(adabe_argument);

};


class adabe_interface;
class adabe_attribute : public virtual AST_Attribute,
		       public virtual adabe_name
{
public:

  adabe_attribute(idl_bool ro, AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p);
  //constructor
  DEF_NARROW_METHODS1(adabe_attribute, AST_Attribute);
  DEF_NARROW_FROM_DECL(adabe_attribute);
  DEF_NARROW_FROM_SCOPE(adabe_attribute);

  void produce_ads(dep_list with,string &String, string &previousdefinition);
  //produce an attribute in the header
  void produce_adb(dep_list with,string &String, string &previousdefinition);
  //produce an attribute in the body
  void produce_impl_ads(dep_list with,string &String, string &previousdefinition);
  //produce an attribute in the implementation header
  void produce_impl_adb(dep_list with,string &String, string &previousdefinition);
  //produce an attribute in the implementation body

  //  void produce_adb(std::fstream& s);
  //  void produce_impl_ads(std::fstream& s);
  //  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);

};


class adabe_operation : public virtual AST_Operation,
		       public virtual adabe_name
{
public:

  adabe_operation(AST_Type *rt, AST_Operation::Flags fl,
		 UTL_ScopedName *n,UTL_StrList *p);
  ~adabe_operation() {}
  //constructor and destructor

  
  DEF_NARROW_METHODS1(adabe_operation, AST_Operation);
  DEF_NARROW_FROM_DECL(adabe_operation);
  DEF_NARROW_FROM_SCOPE(adabe_operation);

  void produce_ads(dep_list with,string &String, string &previousdefinition);
  //produce an operation in the header
  void produce_adb(dep_list with,string &String, string &previousdefinition);
  //produce an operation in the body
  void produce_impl_ads(dep_list with,string &String, string &previousdefinition);
  //produce an operation in the implementation header
  void produce_impl_adb(dep_list with,string &String, string &previousdefinition);
  //produce an operation in the implementation body

  //  void produce_adb(std::fstream& s);
  //  void produce_impl_ads(std::fstream& s);
  //  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);


private:
  bool is_function();
  //to check if the operation is a function or not
};


class adabe_typedef : public virtual AST_Typedef,
		     public virtual adabe_name,
		     public virtual adabe_sequence_chain
{
public:

  adabe_typedef(AST_Type *bt, UTL_ScopedName *n, UTL_StrList *p);
  //constructor
  DEF_NARROW_METHODS1(adabe_typedef, AST_Typedef);
  DEF_NARROW_FROM_DECL(adabe_typedef);

  void produce_ads(dep_list with,string &String, string &previousdefinition);
  //produce a typedef in the header
  void produce_impl_ads(dep_list with,string &String, string &previousdefinition);
  //produce a typedef in the implementation header

  //  void produce_adb(std::fstream& s);
  //  void produce_impl_ads(std::fstream& s);
  //  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);

};


class adabe_interface : public virtual AST_Interface,
		       public virtual adabe_name,
		       public virtual adabe_sequence_chain
{
public:

  adabe_interface(UTL_ScopedName *n, AST_Interface **ih, long nih,
	       UTL_StrList *p);
  //constructor
  DEF_NARROW_METHODS1(adabe_interface, AST_Interface);
  DEF_NARROW_FROM_DECL(adabe_interface);
  DEF_NARROW_FROM_SCOPE(adabe_interface);

  void produce_ads(dep_list with,string &String, string &previousdefinition);
  //produce an interface in the header
  void produce_adb(dep_list with,string &String, string &previousdefinition);
  //produce an interface in the body
  void produce_impl_ads(dep_list with,string &String, string &previousdefinition);
  //produce an interface in the implementation header
  void produce_impl_adb(dep_list with,string &String, string &previousdefinition);
  //produce an interface in the implementation body
  
  //  void produce_adb(std::fstream& s);
  //  void produce_impl_ads(std::fstream& s);
  //  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);

};


class adabe_interface_fwd : public virtual AST_InterfaceFwd,
			   public virtual adabe_name
{
public:

  adabe_interface_fwd(UTL_ScopedName *n, UTL_StrList *p);
  //constructor
  
  DEF_NARROW_METHODS1(adabe_interface_fwd, AST_InterfaceFwd);
  DEF_NARROW_FROM_DECL(adabe_interface_fwd);
  DEF_NARROW_FROM_SCOPE(adabe_interface_fwd);

  void produce_ads(dep_list with,string &String, string &previousdefinition);
  //produce the interface forward in the header
  void produce_impl_ads(dep_list with,string &String, string &previousdefinition);
  //produce the interface forward in the implementation header

  //  void produce_adb(std::fstream& s);
  //  void produce_impl_ads(std::fstream& s);
  //  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);



};


class adabe_module : public virtual AST_Module,
		    public virtual adabe_name
{
public:
  adabe_module(UTL_ScopedName *n, UTL_StrList *p);
  ~adabe_module() {}

  DEF_NARROW_METHODS1(adabe_module, AST_Module);
  DEF_NARROW_FROM_DECL(adabe_module);
  DEF_NARROW_FROM_SCOPE(adabe_module);

  void produce_ads(std::fstream& s);
  void produce_adb(std::fstream& s);
  void produce_impl_ads(std::fstream& s);
  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);

private:
  adabe_module();

};


class adabe_root : public virtual AST_Root,
		  public virtual adabe_module
{
public:

  adabe_root(UTL_ScopedName *n, UTL_StrList *p);
  ~adabe_root() {}

  virtual AST_Sequence *add_sequence(AST_Sequence *s);

  DEF_NARROW_METHODS1(adabe_root, AST_Root);
  DEF_NARROW_FROM_DECL(adabe_root);
  DEF_NARROW_FROM_SCOPE(adabe_root);

  void produce();

private:
  adabe_root();

  char* basename;
  int   baselen;
  std::fstream pd_hdr;
  std::fstream pd_skel;
  std::fstream pd_dynskel;

  void produce_ads(std::fstream& s);
  void produce_adb(std::fstream& s);
  void produce_impl_ads(std::fstream& s);
  void produce_impl_adb(std::fstream& s, adabe_typedef* tdef);
};


class adabe_global {
private:
  static adabe_root* myself;
 //////////////  static char*      pd_hdrsuffix;
 //////////////  static char*      pd_skelsuffix;
 //////////////  static char*      pd_dynskelsuffix;
 //////////////  static size_t     pd_suffixlen;
 //////////////  static int        pd_aflag;      // generate stub for 'any' type
 //////////////  static int        pd_fflag;      // generate stub for float and double
 //////////////  static int        pd_qflag;      // always use fully qualified name
 //////////////  static int        pd_mflag;      // generate stub to work around MSVC bugs

public:
 //////////////  static void set_aflag(int f) { pd_aflag = f; }
 //////////////  static int aflag() { return pd_aflag; }

 //////////////  static void set_fflag(int f) { pd_fflag = f; }
 //////////////  static int fflag() { return pd_fflag; }

 //////////////  static void set_qflag(int f) { pd_qflag = f; }
 //////////////  static int qflag() { return pd_qflag; }

 //////////////  static void set_mflag(int f) { pd_mflag = f; }
 //////////////  static int mflag() { return pd_mflag; }

 //////////////  static int suffixlen() { return pd_suffixlen; }

 //////////////  static void set_hdrsuffix(char* h) {
 //////////////    pd_hdrsuffix = new char[strlen(h)+1];
 //////////////    if (strlen(h) > pd_suffixlen)
 //////////////      pd_suffixlen = strlen(h);
 //////////////   strcpy(pd_hdrsuffix,h);
 //////////////   return;
 ////////////// }

 //////////////  static void set_skelsuffix(char* c) {
 //////////////  pd_skelsuffix = new char[strlen(c) + 1];
 //////////////  if (strlen(c) > pd_suffixlen)
 //////////////    pd_suffixlen = strlen(c);
 //////////////  strcpy(pd_skelsuffix, c);
 //////////////  }
 //////////////  static void set_dynskelsuffix(char* c) {
 //////////////  pd_dynskelsuffix = new char[strlen(c) + 1];
 //////////////  if (strlen(c) > pd_suffixlen)
 //////////////    pd_suffixlen = strlen(c);
 //////////////  strcpy(pd_dynskelsuffix, c);
 //////////////  }

 //////////////  static char* hdrsuffix()     { return pd_hdrsuffix;     }
 //////////////  static char* skelsuffix()    { return pd_skelsuffix;    }
 //////////////  static char* dynskelsuffix() { return pd_dynskelsuffix; }

  static void set_root(adabe_root *v) { myself = v; }
  static adabe_root *root() { return myself; }
};


#define INC_INDENT_LEVEL()  idl_global->indent()->increase();
#define DEC_INDENT_LEVEL()  idl_global->indent()->decrease();
#define IND(s) idl_global->indent()->skip_to(s);


class adabe_generator : public AST_Generator {
public:
  virtual AST_Root *create_root(UTL_ScopedName *n,
				UTL_StrList *p);

  virtual AST_PredefinedType *
          create_predefined_type(AST_PredefinedType::PredefinedType t,
				 UTL_ScopedName *n,
				 UTL_StrList *p);

  virtual AST_Module *
          create_module(UTL_ScopedName *n, UTL_StrList *p);

  virtual AST_Interface	*
          create_interface(UTL_ScopedName *n,
			   AST_Interface **ih,
			   long nih,
			   UTL_StrList *p);

  virtual AST_InterfaceFwd *
          create_interface_fwd(UTL_ScopedName *n,
			       UTL_StrList *p);

  virtual AST_Exception	*
          create_exception(UTL_ScopedName *n, UTL_StrList *p);

  virtual AST_Structure	*
          create_structure(UTL_ScopedName *n, UTL_StrList *p);

  virtual AST_Enum *
          create_enum(UTL_ScopedName *n, UTL_StrList *p);

  virtual AST_Operation	*
          create_operation(AST_Type *rt,
			   AST_Operation::Flags fl,
			   UTL_ScopedName *n,
			   UTL_StrList *p);

  virtual AST_Field *
          create_field(AST_Type *ft,
		       UTL_ScopedName *n,
		       UTL_StrList *p);

  virtual AST_Argument *
          create_argument(AST_Argument::Direction d,
			  AST_Type *ft,
			  UTL_ScopedName *n,
			  UTL_StrList *p);

  virtual AST_Attribute	*
          create_attribute(idl_bool ro,
			   AST_Type *ft,
			   UTL_ScopedName *n,
			   UTL_StrList *p);

  virtual AST_Union *
          create_union(AST_ConcreteType *dt,
		       UTL_ScopedName *n,
		       UTL_StrList *p);

  virtual AST_UnionBranch *
          create_union_branch(AST_UnionLabel *lab,
			      AST_Type *ft,
			      UTL_ScopedName *n,
			      UTL_StrList *p);

  virtual AST_Constant *
          create_constant(AST_Expression::ExprType et,
			  AST_Expression *ev,
			  UTL_ScopedName *n,
			  UTL_StrList *p);

  virtual AST_EnumVal *
          create_enum_val(unsigned long v,
			  UTL_ScopedName *n,
			  UTL_StrList *p);

  virtual AST_Array *
          create_array(UTL_ScopedName *n,
		       unsigned long ndims,
		       UTL_ExprList *dims);

  virtual AST_Sequence *
          create_sequence(AST_Expression *v, AST_Type *bt);

  virtual AST_String *
          create_string(AST_Expression *v);

  virtual AST_Typedef *
          create_typedef(AST_Type *bt,
			 UTL_ScopedName *n,
			 UTL_StrList *p);
};

////////////////////////////////////////////// classe pas connue /////////////////////
class adabe_unsupported {
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


class adabe_fe_error {
public:
  adabe_fe_error(const char* errmsg) {
    pd_errmsg = errmsg;
  }
  ~adabe_fe_error() {}
  const char* errmsg() const { return pd_errmsg; }
private:
  const char* pd_errmsg;
  adabe_fe_error();
};

#endif









