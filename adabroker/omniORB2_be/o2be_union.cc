// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_union.cc            Created on: 12/08/1996
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
//   OMNI BE for the class AST_Union
//

/*
  $Log: o2be_union.cc,v $
  Revision 1.1  1999/02/14 17:45:30  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.20  1999/01/14 10:19:17  djr
  Fixed bug with name scoping problems in IDL unions which are not
  declared at the global scope.

  Revision 1.19  1999/01/07 09:35:34  djr
  Changes to support new TypeCode/Any implementation, which is now
  placed in a new file ...DynSK.cc (by default).

  Revision 1.18  1998/10/16 11:26:21  sll
  Previously, if a fixed size union branch is followed by an array of union
  branch, the data member of the fixed size branch is not defined in the
  generated stub. This is now fixed.

  Revision 1.17  1998/10/14 14:13:28  sll
  Do not put fixed-size struct inside the anonymous union data member in a
  union.

  Revision 1.16  1998/08/19 15:54:50  sll
  New member functions void produce_binary_operators_in_hdr and the like
  are responsible for generating binary operators <<= etc in the global
  namespace.

  Revision 1.15  1998/08/13 22:47:28  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available.

  Revision 1.14  1998/08/10 15:34:12  sll
  Now include octet as a valid discriminant type.

  Revision 1.13  1998/04/07 18:53:50  sll
  Stub code modified to accommodate the use of namespace to represent module.
  Use std::fstream instead of fstream.

// Revision 1.12  1998/03/02  14:13:18  ewc
// OpenVMS union patch applied.
//
// Revision 1.11  1998/01/27  16:51:01  ewc
//  Added support for type Any and TypeCode
//
// Revision 1.10  1998/01/20  19:13:56  sll
// Added support for OpenVMS.
//
  Revision 1.9  1997/12/23 19:26:58  sll
  Fixed a number of bugs related to unions that have array members.

  Revision 1.8  1997/12/18 17:28:54  sll
  *** empty log message ***

  Revision 1.7  1997/12/09 19:55:11  sll
  *** empty log message ***

  Revision 1.6  1997/08/21 21:13:57  sll
  Names of internal variables inside the stub code now all start with the
  prefix __ to avoid potential clash with identifiers defined in IDL.

// Revision 1.5  1997/05/06  14:10:02  sll
// Public release.
//
  */

/*
  Example:

  // IDL
  typedef octet Bytes[64];
  Struct S { long len; };
  interface A;
  union U switch (long) {
      case 1: long x;
      case 2: Bytes y;
      case 3: string z;
      case 4:
      case 5: S w;
      default: A obj;
  };
  
  // C++
  typedef CORBA::Octet Bytes[64];
  typedef CORBA::Octet Bytes_slice;
  class Bytes_forany { ... };
  struct S { CORBA::Long len; };
  typedef ... A_ptr;

  class U {
  public:
    U();
    U(const U&);
    ~U();
    U &operator= (const U&);
    
    void _d(CORBA::Long);
    Long _d() const;

    void x(CORBA::Long);
    CORBA::Long x() const;

    void y(Bytes);
    Bytes_slice *y() const;
    
    void z(char *);
    void z(const char *);
    void z(const CORBA::String_var &);
    const char *z() const;

    void w(const S &);
    const S &w() const;
    S &w();

    void obj(A_ptr); 	 // release old objref, duplicate
    A_ptr obj() const;   // no duplicate
  };


  class U_var {
  public:
    U_var();
    U_var(U *);
    U_var(const U_var &);
    ~U_var();
    U_var & operator= (U *);
    U_var & operator= (const U_var &);
    U *operator->();
    operator U *&();
    operator U &();
    operator const U *() const;
  };
*/

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

#define ADPT_CLASS_TEMPLATE  "_CORBA_ConstrType_Variable_OUT_arg"

static void
produce_disc_value(std::fstream& s,AST_ConcreteType *t,AST_Expression *exp,
		   AST_Decl* used_in, idl_bool use_fqname=I_FALSE);

static void
produce_default_value(o2be_union& u,std::fstream& s);

// added to address a problem with "default: break;" being generated when the
// discriminator is a boolean:
// BCV 23-FEB-1998 12:50:30.66
static void
produce_default_break(o2be_union& u, std::fstream& s);


typedef union {
  int i_val;
  unsigned int ui_val;
  short s_val;
  unsigned short us_val;
  char c_val;
  idl_bool b_val;
  AST_Decl* e_val;
} disc_value_t;

static idl_bool
match_disc_value(o2be_union_branch& b,AST_Decl* disc_type,disc_value_t v);

static o2be_union_branch*
lookup_by_disc_value(o2be_union& u,disc_value_t v);

o2be_union::o2be_union(AST_ConcreteType *dt,
		       UTL_ScopedName *n, UTL_StrList *p)
	: AST_Union(dt, n, p),
	  AST_Decl(AST_Decl::NT_union, n, p),
          AST_Structure(AST_Decl::NT_union, n, p),
	  UTL_Scope(AST_Decl::NT_union),
	  o2be_name(AST_Decl::NT_union, n, p),
	  o2be_sequence_chain(AST_Decl::NT_union, n, p)
{
  pd_isvar = I_FALSE;
  pd_nodefault = I_TRUE;
  pd_hdr_produced_in_field = I_FALSE;
  pd_skel_produced_in_field = I_FALSE;
  pd_binary_operators_hdr_produced_in_field = I_FALSE;
  pd_binary_operators_skel_produced_in_field = I_FALSE;
  pd_have_produced_typecode_skel = I_FALSE;

  pd_out_adptarg_name = new char[strlen(ADPT_CLASS_TEMPLATE)+strlen("<,>")+
				 strlen(fqname())+
				 strlen(fqname())+strlen("_var")+1];
  strcpy(pd_out_adptarg_name,ADPT_CLASS_TEMPLATE);
  strcat(pd_out_adptarg_name,"<");
  strcat(pd_out_adptarg_name,fqname());
  strcat(pd_out_adptarg_name,",");
  strcat(pd_out_adptarg_name,fqname());
  strcat(pd_out_adptarg_name,"_var>");  
}

AST_UnionBranch *
o2be_union::add_union_branch(AST_UnionBranch *un)
{
  // Check that the CFE operation succeeds. If it returns 0,
  // stop any further work.
  if (AST_Union::add_union_branch(un) == 0)
    return 0;

  if ((un->label())->label_kind() == AST_UnionLabel::UL_default)
    pd_nodefault = I_FALSE;

  // Now check if the field is of variable size.
  if (isVariable())
    return un;

  AST_Decl* decl = un->field_type();
  while (decl->node_type() == AST_Decl::NT_typedef)
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();

  switch (decl->node_type())
    {
    case AST_Decl::NT_struct:
      pd_isvar = o2be_structure::narrow_from_decl(decl)->isVariable();
      break;
    case AST_Decl::NT_union:
      pd_isvar = o2be_union::narrow_from_decl(decl)->isVariable();
      break;
    case AST_Decl::NT_string:
    case AST_Decl::NT_sequence:
    case AST_Decl::NT_interface:
      pd_isvar = I_TRUE;
      break;
    case AST_Decl::NT_pre_defined:
      if (o2be_predefined_type::narrow_from_decl(decl)->pt()
	  == AST_PredefinedType::PT_any ||
	  o2be_predefined_type::narrow_from_decl(decl)->pt()
	  == AST_PredefinedType::PT_TypeCode) 
	{
	  pd_isvar = I_TRUE;
	}
      break;
    case AST_Decl::NT_array:
      pd_isvar = o2be_array::narrow_from_decl(decl)->isVariable();
      break;
    default:
      break;
    }
  return un;
}

// Returns TRUE if the field is an array of fixed size
// unions. This case is important as such a field cannot
// be placed inside the anonymous union.
static idl_bool
is_array_of_fixed_size_union(o2be_operation::argType ntype, o2be_field* f)
{
  if( ntype == o2be_operation::tArrayFixed ) {
    AST_Decl* dd = f->field_type();
    // Skip all typedef to get to the array node
    while( dd->node_type() == AST_Decl::NT_typedef ) {
      dd = o2be_typedef::narrow_from_decl(dd)->base_type();
    }
    // Get the element type
    dd = o2be_array::narrow_from_decl(dd)->getElementType();
    // Skip all typedef to get to the real element node
    while( dd->node_type() == AST_Decl::NT_typedef ) {
      dd = o2be_typedef::narrow_from_decl(dd)->base_type();
    }
    if( dd->node_type() == AST_Decl::NT_union ) {
      // The element is a union. Do not define the data member
      // in the anonymous union.
      return I_TRUE;
    }
  }
  return I_FALSE;
}

void
o2be_union::produce_hdr(std::fstream& s)
{
  // Front end erroneously lets Octet through as a
  // discriminator type. Catch here.
  {
    AST_Decl* decl = disc_type();

    while( decl->node_type() == AST_Decl::NT_typedef )
      decl = o2be_typedef::narrow_from_decl(decl)->base_type();

    if( decl->node_type() == AST_Decl::NT_pre_defined &&
	AST_PredefinedType::narrow_from_decl(decl)->pt() ==
	AST_PredefinedType::PT_octet ) {
      UTL_String msg("octet may not be used as a union discriminator.");
      idl_global->err()->back_end(line(), &msg);
      return;
    }
  }

  if (!nodefault())
    {
      if (no_missing_disc_value())
	{
	  // Wrong, all legal discriminant value has been specified
	  // there should not be a default case.
	  UTL_String msg("default case defined where none is needed.");
	  idl_global->err()->back_end(line(), &msg);
	  return;
	}
    }


  idl_bool has_fix_member = I_FALSE;

  IND(s); s << "class " << uqname() << " {\n";
  IND(s); s << "public:\n\n";
  INC_INDENT_LEVEL();
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_Decl* decl=AST_UnionBranch::narrow_from_decl(d)->field_type();
	    if (decl->has_ancestor(this))
	      {
		switch (decl->node_type())
		  {
		  case AST_Decl::NT_enum:
		    if (!o2be_enum::narrow_from_decl(decl)
			       ->get_hdr_produced_in_field()) 
		      {
			o2be_enum::narrow_from_decl(decl)
			       ->set_hdr_produced_in_field();
			o2be_enum::narrow_from_decl(decl)->produce_hdr(s);
		      }
		    break;
		  case AST_Decl::NT_struct:
		    if (!o2be_structure::narrow_from_decl(decl)
			       ->get_hdr_produced_in_field()) 
		      {
			o2be_structure::narrow_from_decl(decl)
			       ->set_hdr_produced_in_field();
			o2be_structure::narrow_from_decl(decl)->produce_hdr(s);
		      }
		    break;
		  case AST_Decl::NT_union:
		    if (!o2be_union::narrow_from_decl(decl)
			       ->get_hdr_produced_in_field()) 
		      {
			o2be_union::narrow_from_decl(decl)
			       ->set_hdr_produced_in_field();
			o2be_union::narrow_from_decl(decl)->produce_hdr(s);
		      }
		    break;
		  default:
		    break;
		  }
	      }
	  }
	i.next();
      }
  }

  IND(s); s << uqname() << "() {\n";
  INC_INDENT_LEVEL();
  if (nodefault())
    {
      if (!no_missing_disc_value()) {
	IND(s); s << "_default();\n";
      }
    }
  else
    {
      IND(s); s << "pd__default = 1;\n";
      IND(s); s << "pd__d = ";
      produce_default_value(*this,s);
      s << ";\n";
    }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << uqname() << "(const " << uqname() << "& _value) {\n";
  INC_INDENT_LEVEL();
  if (!(nodefault() && no_missing_disc_value()))
    {
      IND(s); s << "if ((pd__default = _value.pd__default)) {\n";
      INC_INDENT_LEVEL();
      {
	IND(s); s << "pd__d = _value.pd__d;\n";
	UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl* d = i.item();
	    if (d->node_type() == AST_Decl::NT_union_branch)
	      {
		AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
		if (l->label_kind() == AST_UnionLabel::UL_default)
		  {
		    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		    IND(s); s << f->uqname() << "(_value.pd_"
			      << f->uqname() << ");\n";
		    break;
		  }
	      }
	    i.next();
	  }
      }
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      IND(s); s << "else {\n";
    }
  INC_INDENT_LEVEL();
  IND(s); s << "switch(_value.pd__d) {\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
	    if (l->label_kind() == AST_UnionLabel::UL_label)
	      {
		o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		IND(s); s << "case ";
		produce_disc_value(s,disc_type(),l->label_val(),this);
		s << ": "
		  << f->uqname() << "(_value.pd_"
		  << f->uqname() << "); break;\n";
	      }
	  }
	i.next();
      }
  }
  produce_default_break(*this, s);
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  if (!(nodefault() && no_missing_disc_value())) {
    IND(s); s << "}\n";
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "~" << uqname() << "() {}\n\n";

  IND(s); s << uqname() << "& operator=(const " << uqname() << "& _value) {\n";
  INC_INDENT_LEVEL();
  if (!(nodefault() && no_missing_disc_value()))
    {
      IND(s); s << "if ((pd__default = _value.pd__default)) {\n";
      INC_INDENT_LEVEL();
      {
	IND(s); s << "pd__d = _value.pd__d;\n";
	UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl* d = i.item();
	    if (d->node_type() == AST_Decl::NT_union_branch)
	      {
		AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
		if (l->label_kind() == AST_UnionLabel::UL_default)
		  {
		    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		    IND(s); s << f->uqname() << "(_value.pd_"
			      << f->uqname() << ");\n";
		    break;
		  }
	      }
	    i.next();
	  }
      }
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      IND(s); s << "else {\n";
    }
  INC_INDENT_LEVEL();
  IND(s); s << "switch(_value.pd__d) {\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
	    if (l->label_kind() == AST_UnionLabel::UL_label)
	      {
		o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		IND(s); s << "case ";
		produce_disc_value(s,disc_type(),l->label_val(),this);
		s << ": "
		  << f->uqname() << "(_value.pd_"
		  << f->uqname() << "); break;\n";
	      }
	  }
	i.next();
      }
  }
  produce_default_break(*this, s);
  DEC_INDENT_LEVEL();
  if (!(nodefault() && no_missing_disc_value())) {
    IND(s); s << "}\n";
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "return *this;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
  
  IND(s); s << o2be_name::narrow_and_produce_unambiguous_name(disc_type(),this) 
	    << " _d () const { return pd__d;}\n";
  IND(s); s << "void _d("
	    << o2be_name::narrow_and_produce_unambiguous_name(disc_type(),this)
	    << " _value) {}\n\n";

  if (nodefault() && !no_missing_disc_value())
    {
      // No default case declared.
      IND(s); s << "void _default()\n";
      IND(s); s << "{\n";
      INC_INDENT_LEVEL();
      IND(s); s << "pd__d = ";
      produce_default_value(*this,s);
      s << ";\n";
      IND(s); s << "pd__default = 1;\n";
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n\n";
    }

  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype;

	    ntype =  o2be_operation::ast2ArgMapping(f->field_type(),
					   o2be_operation::wResult,mapping);

	    if (ntype != o2be_operation::tString &&
		ntype != o2be_operation::tObjref &&
		ntype != o2be_operation::tStructFixed &&
		ntype != o2be_operation::tStructVariable &&
		ntype != o2be_operation::tUnionVariable &&
		ntype != o2be_operation::tUnionFixed &&
		ntype != o2be_operation::tSequence &&
		ntype != o2be_operation::tArrayVariable &&
		ntype != o2be_operation::tAny &&
		ntype != o2be_operation::tTypeCode &&
		!is_array_of_fixed_size_union(ntype, f) ) {

	      has_fix_member = I_TRUE;
	    }

	    switch (ntype)
	      {
	      case o2be_operation::tString:
		IND(s); s << "const char * " << f->uqname() << " () const { return (const char*) pd_" << f->uqname() << "; }\n";
		break;
	      case o2be_operation::tObjref:
		{
		  AST_Decl* decl = f->field_type();
		  while (decl->node_type() == AST_Decl::NT_typedef) {
		    decl = o2be_typedef::narrow_from_decl(decl)->base_type();
		  }
		  IND(s); s << o2be_interface::narrow_from_decl(decl)->unambiguous_objref_name(this)
			    << " " << f->uqname() << " () const { return "
			    << "pd_" << f->uqname() << "._ptr; }\n";
		  break;
		}
	      case o2be_operation::tTypeCode:
		{
		  IND(s); s << "CORBA::TypeCode_ptr " << f->uqname()
			    << " () const { return pd_" << f->uqname()
			    << "._ptr; }\n";
		  break;
		}
	      case o2be_operation::tSequence:
		{
		  if (f->field_type()->node_type() == AST_Decl::NT_sequence)
		    {
		      IND(s); s << "const "
				<< o2be_sequence::narrow_from_decl(f->field_type())->seq_template_name(this)
				<< " &"
				<< f->uqname() << " () const { return pd_"
				<< f->uqname() << "; }\n";
		      IND(s); s << o2be_sequence::narrow_from_decl(f->field_type())->seq_template_name(this)
				<< " &"
				<< f->uqname() << " () { return pd_"
				<< f->uqname() << "; }\n";
		    }
		  else
		    {
		      IND(s); s << "const "
				<< o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
				<< " &"
				<< f->uqname() << " () const { return pd_"
				<< f->uqname() << "; }\n";
		      IND(s); s << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
				<< " &"
				<< f->uqname() << " () { return pd_"
				<< f->uqname() << "; }\n";
		    }
		  break;
		}
	      case o2be_operation::tStructFixed:
	      case o2be_operation::tStructVariable:
	      case o2be_operation::tUnionFixed:
	      case o2be_operation::tUnionVariable:
	      case o2be_operation::tAny:
		IND(s); s << "const "
			  << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			  << " &"
			  << f->uqname() << " () const { return pd_"
			  << f->uqname() << "; }\n";
		IND(s); s << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			  << " &"
			  << f->uqname() << " () { return pd_"
			  << f->uqname() << "; }\n";
		break;
	      case o2be_operation::tArrayFixed:
	      case o2be_operation::tArrayVariable:
		{
		  // Check if this is an anonymous array type, if so
		  // generate the supporting typedef for the array slice
		  AST_Decl* decl = f->field_type();
		  if (decl->node_type() == AST_Decl::NT_array &&
		      decl->has_ancestor(this)) 
		    {
		      char * tmpname = new char [strlen(f->uqname()) + 2];
		      strcpy(tmpname,"_");
		      strcat(tmpname,f->uqname());
		      o2be_array::narrow_from_decl(decl)->produce_typedef_in_union(s,tmpname,this);
		      IND(s); s << "const " << tmpname << "_slice* " 
				<< f->uqname() << "() const { return pd_"
				<< f->uqname() << "; }\n";
		      delete [] tmpname;
		    }
		  else
		    {
		      IND(s); s << "const "
				<< o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
				<< ((mapping.is_arrayslice) ? "_slice":"")
				<< " "
				<< ((mapping.is_pointer)    ? "*":"")
				<< ((mapping.is_reference)  ? "&":"")
				<< f->uqname() << " () const { return pd_"
				<< f->uqname() << "; }\n";
		    }
		  break;
		}
	      default: 
		IND(s); s << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			  << " "
			  << ((mapping.is_pointer)    ? "*":"")
			  << ((mapping.is_reference)  ? "&":"")
			  << f->uqname() << " () const { return pd_"
			  << f->uqname() << "; }\n";
		break;
	      }

	    ntype =  o2be_operation::ast2ArgMapping(f->field_type(),
						  o2be_operation::wIN,mapping);


	    AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();

	    switch (ntype)
	      {
	      case o2be_operation::tString:
		IND(s); s << "void "
			  << f->uqname() << "(char* _value) {\n";
		INC_INDENT_LEVEL();
		if (l->label_kind() == AST_UnionLabel::UL_label)
		  {
		    IND(s); s << "pd__d = ";
		    produce_disc_value(s,disc_type(),l->label_val(),this);
		    s << ";\n";
		    IND(s); s << "pd__default = 0;\n";
		  }
		else
		  {
		    IND(s); s << "pd__d = ";
		    produce_default_value(*this,s);
		    s << ";\n";
		    IND(s); s << "pd__default = 1;\n";
		  }
		IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		DEC_INDENT_LEVEL();
		IND(s); s << "}\n";
		IND(s); s << "void "
			  << f->uqname() << "(const char*  _value) {\n";
		INC_INDENT_LEVEL();
		if (l->label_kind() == AST_UnionLabel::UL_label)
		  {
		    IND(s); s << "pd__d = ";
		    produce_disc_value(s,disc_type(),l->label_val(),this);
		    s << ";\n";
		    IND(s); s << "pd__default = 0;\n";
		  }
		else
		  {
		    IND(s); s << "pd__d = ";
		    produce_default_value(*this,s);
		    s << ";\n";
		    IND(s); s << "pd__default = 1;\n";
		  }
		IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		DEC_INDENT_LEVEL();
		IND(s); s << "}\n";
		IND(s); s << "void "
			  << f->uqname() << "(const CORBA::String_var& _value) {\n";
		INC_INDENT_LEVEL();
		if (l->label_kind() == AST_UnionLabel::UL_label)
		  {
		    IND(s); s << "pd__d = ";
		    produce_disc_value(s,disc_type(),l->label_val(),this);
		    s << ";\n";
		    IND(s); s << "pd__default = 0;\n";
		  }
		else
		  {
		    IND(s); s << "pd__d = ";
		    produce_default_value(*this,s);
		    s << ";\n";
		    IND(s); s << "pd__default = 1;\n";
		  }
		IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		DEC_INDENT_LEVEL();
		IND(s); s << "}\n";
		IND(s); s << "void "
			  << f->uqname() << "(const "
			  << o2be_string::fieldMemberTypeName() 
			  << "& _value) {\n";
		INC_INDENT_LEVEL();
		if (l->label_kind() == AST_UnionLabel::UL_label)
		  {
		    IND(s); s << "pd__d = ";
		    produce_disc_value(s,disc_type(),l->label_val(),this);
		    s << ";\n";
		    IND(s); s << "pd__default = 0;\n";
		  }
		else
		  {
		    IND(s); s << "pd__d = ";
		    produce_default_value(*this,s);
		    s << ";\n";
		    IND(s); s << "pd__default = 1;\n";
		  }
		IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		DEC_INDENT_LEVEL();
		IND(s); s << "}\n";
		break;
	      case o2be_operation::tObjref:
		{
		  AST_Decl* decl = f->field_type();
		  while (decl->node_type() == AST_Decl::NT_typedef) {
		    decl = o2be_typedef::narrow_from_decl(decl)->base_type();
		  }
		  IND(s); s << "void " << f->uqname() << "(" 
			    << o2be_interface::narrow_from_decl(decl)->unambiguous_objref_name(this)
			    << " _value) {\n";
		  INC_INDENT_LEVEL();
		  if (l->label_kind() == AST_UnionLabel::UL_label)
		    {
		      IND(s); s << "pd__d = ";
		      produce_disc_value(s,disc_type(),l->label_val(),this);
		      s << ";\n";
		      IND(s); s << "pd__default = 0;\n";
		    }
		  else
		    {
		      IND(s); s << "pd__d = ";
		      produce_default_value(*this,s);
		      s << ";\n";
		      IND(s); s << "pd__default = 1;\n";
		    }
		  IND(s); s << o2be_interface::narrow_from_decl(decl)->unambiguous_name(this)
			    << "_Helper::duplicate(_value);\n";
		  IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		  DEC_INDENT_LEVEL();
		  IND(s); s << "}\n";
		  IND(s); s << "void " << f->uqname() << "(const " 
			    << o2be_interface::narrow_from_decl(decl)->fieldMemberType_fqname(this)
			    << "& _value) {\n";
		  INC_INDENT_LEVEL();
		  if (l->label_kind() == AST_UnionLabel::UL_label)
		    {
		      IND(s); s << "pd__d = ";
		      produce_disc_value(s,disc_type(),l->label_val(),this);
		      s << ";\n";
		      IND(s); s << "pd__default = 0;\n";
		    }
		  else
		    {
		      IND(s); s << "pd__d = ";
		      produce_default_value(*this,s);
		      s << ";\n";
		      IND(s); s << "pd__default = 1;\n";
		    }
		  IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		  DEC_INDENT_LEVEL();
		  IND(s); s << "}\n";
		  IND(s); s << "void " << f->uqname() << "(const " 
			    << o2be_interface::narrow_from_decl(decl)->unambiguous_name(this)
			    << "_var&  _value) {\n";
		  INC_INDENT_LEVEL();
		  if (l->label_kind() == AST_UnionLabel::UL_label)
		    {
		      IND(s); s << "pd__d = ";
		      produce_disc_value(s,disc_type(),l->label_val(),this);
		      s << ";\n";
		      IND(s); s << "pd__default = 0;\n";
		    }
		  else
		    {
		      IND(s); s << "pd__d = ";
		      produce_default_value(*this,s);
		      s << ";\n";
		      IND(s); s << "pd__default = 1;\n";
		    }
		  IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		  DEC_INDENT_LEVEL();
		  IND(s); s << "}\n";
		  break;
		}
	      case o2be_operation::tTypeCode:
		{
		IND(s); s << "void "
			  << f->uqname() << "(CORBA::TypeCode_ptr _value) {\n";
		INC_INDENT_LEVEL();
		if (l->label_kind() == AST_UnionLabel::UL_label)
		  {
		    IND(s); s << "pd__d = ";
		    produce_disc_value(s,disc_type(),l->label_val(),this);
		    s << ";\n";
		    IND(s); s << "pd__default = 0;\n";
		  }
		else
		  {
		    IND(s); s << "pd__d = ";
		    produce_default_value(*this,s);
		    s << ";\n";
		    IND(s); s << "pd__default = 1;\n";
		  }
		IND(s); s << "pd_" << f->uqname() << " = CORBA::TypeCode::_duplicate(_value);\n";
		DEC_INDENT_LEVEL();
		IND(s); s << "}\n";
		IND(s); s << "void "
			  << f->uqname() << "(const " 
			  << o2be_predefined_type::TypeCodeMemberName() 
			  << "& _value) {\n";
		INC_INDENT_LEVEL();
		if (l->label_kind() == AST_UnionLabel::UL_label)
		  {
		    IND(s); s << "pd__d = ";
		    produce_disc_value(s,disc_type(),l->label_val(),this);
		    s << ";\n";
		    IND(s); s << "pd__default = 0;\n";
		  }
		else
		  {
		    IND(s); s << "pd__d = ";
		    produce_default_value(*this,s);
		    s << ";\n";
		    IND(s); s << "pd__default = 1;\n";
		  }
		IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		DEC_INDENT_LEVEL();
		IND(s); s << "}\n";
		IND(s); s << "void "
			  << f->uqname() << "(const CORBA::TypeCode_var& _value) {\n";
		INC_INDENT_LEVEL();
		if (l->label_kind() == AST_UnionLabel::UL_label)
		  {
		    IND(s); s << "pd__d = ";
		    produce_disc_value(s,disc_type(),l->label_val(),this);
		    s << ";\n";
		    IND(s); s << "pd__default = 0;\n";
		  }
		else
		  {
		    IND(s); s << "pd__d = ";
		    produce_default_value(*this,s);
		    s << ";\n";
		    IND(s); s << "pd__default = 1;\n";
		  }
		IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		DEC_INDENT_LEVEL();
		IND(s); s << "}\n";
		break;
		}
	      case o2be_operation::tSequence:
		if (f->field_type()->node_type() == AST_Decl::NT_sequence)
		  {
		    IND(s); s << "void "
			      << f->uqname() << " (const "
			      << o2be_sequence::narrow_from_decl(f->field_type())->seq_template_name(this)
			      << "& _value) {\n";
		    INC_INDENT_LEVEL();
		    if (l->label_kind() == AST_UnionLabel::UL_label)
		      {
			IND(s); s << "pd__d = ";
			produce_disc_value(s,disc_type(),l->label_val(),this);
			s << ";\n";
			IND(s); s << "pd__default = 0;\n";
		      }
		    else
		      {
			IND(s); s << "pd__d = ";
			produce_default_value(*this,s);
			s << ";\n";
			IND(s); s << "pd__default = 1;\n";
		      }
		    IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		    DEC_INDENT_LEVEL();
		    IND(s); s << "}\n";
		  }
		else
		  {
		    IND(s); s << "void "
			      << f->uqname() << " (const "
			      << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			      << "& _value) {\n";
		    INC_INDENT_LEVEL();
		    if (l->label_kind() == AST_UnionLabel::UL_label)
		      {
			IND(s); s << "pd__d = ";
			produce_disc_value(s,disc_type(),l->label_val(),this);
			s << ";\n";
			IND(s); s << "pd__default = 0;\n";
		      }
		    else
		      {
			IND(s); s << "pd__d = ";
			produce_default_value(*this,s);
			s << ";\n";
			IND(s); s << "pd__default = 1;\n";
		      }
		    IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		    DEC_INDENT_LEVEL();
		    IND(s); s << "}\n";
		  }
		break;
	      case o2be_operation::tStructFixed:
	      case o2be_operation::tStructVariable:
	      case o2be_operation::tUnionFixed:
	      case o2be_operation::tUnionVariable:
	      case o2be_operation::tAny:
		IND(s); s << "void "
			  << f->uqname() << " (const "
			  << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			  << "& _value) {\n";
		INC_INDENT_LEVEL();
		if (l->label_kind() == AST_UnionLabel::UL_label)
		  {
		    IND(s); s << "pd__d = ";
		    produce_disc_value(s,disc_type(),l->label_val(),this);
		    s << ";\n";
		    IND(s); s << "pd__default = 0;\n";
		  }
		else
		  {
		    IND(s); s << "pd__d = ";
		    produce_default_value(*this,s);
		    s << ";\n";
		    IND(s); s << "pd__default = 1;\n";
		  }
		IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		DEC_INDENT_LEVEL();
		IND(s); s << "}\n";
		break;
	      case o2be_operation::tArrayFixed:
	      case o2be_operation::tArrayVariable:
		{
		  // Check if this is an anonymous array type, if so
		  // generate the supporting typedef for the array slice
		  AST_Decl* decl = f->field_type();
		  if (decl->node_type() == AST_Decl::NT_array &&
		      decl->has_ancestor(this)) 
		    {
		      IND(s); s << "void "
				<< f->uqname() << " (const "
				<< "_0RL_" << f->uqname() << " _value) {\n";
		    }
		  else
		    {
		      IND(s); s << "void "
				<< f->uqname() << " (const "
				<< o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
				<< " _value) {\n";
		    }
		  INC_INDENT_LEVEL();
		  if (l->label_kind() == AST_UnionLabel::UL_label)
		    {
		      IND(s); s << "pd__d = ";
		      produce_disc_value(s,disc_type(),l->label_val(),this);
		      s << ";\n";
		      IND(s); s << "pd__default = 0;\n";
		    }
		  else
		    {
		      IND(s); s << "pd__d = ";
		      produce_default_value(*this,s);
		      s << ";\n";
		      IND(s); s << "pd__default = 1;\n";
		    }
		  {
		    unsigned int ndim = 0;
		    unsigned int dimval;

		    while (decl->node_type() == AST_Decl::NT_typedef)
		      decl = o2be_typedef::narrow_from_decl(decl)->base_type();

		    o2be_array::dim_iterator next(o2be_array::narrow_from_decl(decl));
		    while (ndim < o2be_array::narrow_from_decl(decl)->getNumOfDims())
		      {
			dimval = next();
			IND(s); s << "for (unsigned int _i" << ndim << " =0;"
				  << "_i" << ndim << " < " << dimval << ";"
				  << "_i" << ndim << "++) {\n";
			INC_INDENT_LEVEL();
			ndim++;
		      }
		    IND(s); s << "pd_" << f->uqname();
		    ndim = 0;
		    while (ndim < o2be_array::narrow_from_decl(decl)->getNumOfDims())
		      {
			s << "[_i" << ndim << "]";
			ndim++;
		      }
		    s << " = _value";
		    ndim = 0;
		    while (ndim < o2be_array::narrow_from_decl(decl)->getNumOfDims())
		      {
			s << "[_i" << ndim << "]";
			ndim++;
		      }
		    s << ";\n";
		    ndim = 0;
		    while (ndim < o2be_array::narrow_from_decl(decl)->getNumOfDims())
		      {
			DEC_INDENT_LEVEL();
			IND(s); s << "}\n";
			ndim++;
		      }
		  }
		  DEC_INDENT_LEVEL();
		  IND(s); s << "}\n";
		  break;
		}
	      default:
		IND(s); s << "void " 
			  << f->uqname() << " ("
			  << ((mapping.is_const) ? "const ":"")
			  << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			  << " "
			  << ((mapping.is_pointer)    ? "*":"")
			  << ((mapping.is_reference)  ? "&":"")
			  << " _value) {\n";
		INC_INDENT_LEVEL();
		if (l->label_kind() == AST_UnionLabel::UL_label)
		  {
		    IND(s); s << "pd__d = ";
		    produce_disc_value(s,disc_type(),l->label_val(),this);
		    s << ";\n";
		    IND(s); s << "pd__default = 0;\n";
		  }
		else
		  {
		    IND(s); s << "pd__d = ";
		    produce_default_value(*this,s);
		    s << ";\n";
		    IND(s); s << "pd__default = 1;\n";
		  }
		IND(s); s << "pd_" << f->uqname() << " = _value;\n";
		DEC_INDENT_LEVEL();
		IND(s); s << "}\n";
		break;
	      }

	  }
	i.next();
      }
  }


  IND(s); s << "\n";
  IND(s); s << "size_t NP_alignedSize(size_t initialoffset) const;\n";
  IND(s); s << "void operator>>= (NetBufferedStream&) const;\n";
  IND(s); s << "void operator<<= (NetBufferedStream&);\n";
  IND(s); s << "void operator>>= (MemBufferedStream&) const;\n";
  IND(s); s << "void operator<<= (MemBufferedStream&);\n\n";

  if (idl_global->compile_flags() & IDL_CF_ANY) {
    s << "#if defined(__GNUG__) || defined(__DECCXX)\n";
    IND(s); s << "friend class _0RL_tcParser_unionhelper_" << _idname()
	      << ";\n";
    s << "#else\n";
    IND(s); s << "friend class ::_0RL_tcParser_unionhelper_" << _idname()
	      << ";\n";
    s << "#endif\n";
  }

  DEC_INDENT_LEVEL();
  IND(s); s << "private:\n\n";
  INC_INDENT_LEVEL();

  IND(s); s << o2be_name::narrow_and_produce_unambiguous_name(disc_type(),this)
	    << " pd__d;\n";
  IND(s); s << "CORBA::Boolean pd__default;\n";

  // Generate members which can go into the anonymous union.
  //
  if (has_fix_member) {
    IND(s); s << "union {\n";
    INC_INDENT_LEVEL();
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype;

	    ntype =  o2be_operation::ast2ArgMapping(f->field_type(),
				       o2be_operation::wResult,mapping);
	    switch (ntype)
	      {
		// For o2be_operation::tStructFixed, do not generate data 
		// member inside the anonymous union. This is because even if
		// all the elements are fixed size, it may contain union
		// elements directly or indirectly. This is not allowed because
		// anonymous union does not allow user defined ctor and dtor.
		// We could check whether the struct has a union element.
		// For simplicity, we just play it safe here.
	      case o2be_operation::tFloat:
	      case o2be_operation::tDouble:
		s << "#ifndef USING_PROXY_FLOAT\n";
		IND(s); s << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			  << " pd_" << f->uqname() << ";\n";
                s << "#endif\n";
		break;
	      case o2be_operation::tShort:
	      case o2be_operation::tLong:
	      case o2be_operation::tUShort:
	      case o2be_operation::tULong:
	      case o2be_operation::tBoolean:
	      case o2be_operation::tChar:
	      case o2be_operation::tOctet:
	      case o2be_operation::tEnum:
		IND(s); s << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			  << " pd_" << f->uqname() << ";\n";
		break;
	      case o2be_operation::tArrayFixed:
		// Array of fixed size union is a special case, the data
		// member cannot be put into the anonymous union.
		// Trap it here.
		if( is_array_of_fixed_size_union(o2be_operation::tArrayFixed, f) )
		  break;

		if (f->field_type()->node_type() == AST_Decl::NT_array)
		  {
		    IND(s);
		    o2be_array::narrow_from_decl(f->field_type())->produce_union_member_decl(s,f,this);
		  }
		else
		  {
		    IND(s); s << o2be_typedef::narrow_from_decl(f->field_type())->unambiguous_name(this)
			      << " pd_" << f->uqname() << ";\n";
		  }
		break;
	      default:
		break;
	      }
	  }
	i.next();
      }
    DEC_INDENT_LEVEL();
    IND(s); s << "};\n";
  }
  // Generate members which don't go into the anonymous union.
  //
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype;

	    ntype =  o2be_operation::ast2ArgMapping(f->field_type(),
				       o2be_operation::wResult,mapping);
	    switch (ntype)
	      {
	      case o2be_operation::tString:
		IND(s); s << o2be_string::fieldMemberTypeName()
			  << " pd_" << f->uqname() << ";\n";
		break;
	      case o2be_operation::tObjref:
		if (f->field_type()->node_type() == AST_Decl::NT_interface)
		  {
		    IND(s); s << o2be_interface::narrow_from_decl(f->field_type())->fieldMemberType_fqname(this)
			      << " pd_" << f->uqname() << ";\n";
		  }
		else
		  {
		    IND(s); s << o2be_typedef::narrow_from_decl(f->field_type())->fieldMemberType_fqname(this)
			      << " pd_" << f->uqname() << ";\n";
		  }
		break;
	      case o2be_operation::tTypeCode:
		IND(s); s << o2be_predefined_type::TypeCodeMemberName()
			  << " pd_" << f->uqname() << ";\n";
		break;
              case o2be_operation::tFloat:
              case o2be_operation::tDouble:
		s << "#ifdef USING_PROXY_FLOAT\n";
		IND(s); s << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			  << " pd_" << f->uqname() << ";\n";
		s << "#endif\n";
		break;
	      case o2be_operation::tStructFixed:
		// See comment above on why we do not generate this data member
		// inside the anonymous union.
	      case o2be_operation::tStructVariable:
	      case o2be_operation::tUnionFixed:
	      case o2be_operation::tUnionVariable:
	      case o2be_operation::tAny:
		IND(s); s << o2be_name::narrow_and_produce_unambiguous_name(f->field_type(),this)
			  << " pd_" << f->uqname() << ";\n";
		break;
	      case o2be_operation::tSequence:
		if (f->field_type()->node_type() == AST_Decl::NT_sequence) 
		  {
		    IND(s); s << o2be_sequence::narrow_from_decl(f->field_type())->seq_template_name(this)
			      << " pd_" << f->uqname() << ";\n";
		  }
		else
		  {
		    IND(s); s << o2be_typedef::narrow_from_decl(f->field_type())->unambiguous_name(this)
			      << " pd_" << f->uqname() << ";\n";
		  }
		break;
	      case o2be_operation::tArrayFixed:
		// Array of fixed size union is a special case, the data
		// member cannot be put into the anonymous union.
		// Trap it here.
		if( !is_array_of_fixed_size_union(o2be_operation::tArrayFixed, f) )
		  break;

		// falls through to tArrayVariable.

	      case o2be_operation::tArrayVariable:
		if (f->field_type()->node_type() == AST_Decl::NT_array)
		  {
		    IND(s);
		    o2be_array::narrow_from_decl(f->field_type())->produce_union_member_decl(s,f,this);
		  }
		else
		  {
		    IND(s); s << o2be_typedef::narrow_from_decl(f->field_type())->unambiguous_name(this)
			      << " pd_" << f->uqname() << ";\n";
		  }
		break;
	      default:
		break;
	      }
	  }
	i.next();
      }
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "};\n\n";

  IND(s); s << "typedef _CORBA_ConstrType_"
	    << ((isVariable())?"Variable":"Fix")
	    << "_Var<" << uqname() << "> " 
	      << uqname() << "_var;\n\n";

  if (idl_global->compile_flags() & IDL_CF_ANY) {
    // TypeCode_ptr declaration
    IND(s); s << variable_qualifier()
	      << " const CORBA::TypeCode_ptr " << tcname() << ";\n\n";
  }

  produce_seq_hdr_if_defined(s);
}


void
o2be_union::produce_skel(std::fstream& s)
{
  s << "\n";
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_Decl* decl=AST_UnionBranch::narrow_from_decl(d)->field_type();
	    if (decl->has_ancestor(this))
	      {
		switch (decl->node_type())
		  {
		  case AST_Decl::NT_enum:
		    if (!o2be_enum::narrow_from_decl(decl)
			       ->get_skel_produced_in_field()) {
		      o2be_enum::narrow_from_decl(decl)
			->set_skel_produced_in_field();
		      o2be_enum::narrow_from_decl(decl)->produce_skel(s);
		    }
		    break;
		  case AST_Decl::NT_struct:
		    if (!o2be_structure::narrow_from_decl(decl)
			       ->get_skel_produced_in_field()) {
		      o2be_structure::narrow_from_decl(decl)
			->set_skel_produced_in_field();
		      o2be_structure::narrow_from_decl(decl)->produce_skel(s);
		    }
		    break;
		  case AST_Decl::NT_union:
		    if (!o2be_union::narrow_from_decl(decl)
			       ->get_skel_produced_in_field()) {
		      o2be_union::narrow_from_decl(decl)
			->set_skel_produced_in_field();
		      o2be_union::narrow_from_decl(decl)->produce_skel(s);
		    }
		    break;
		  default:
		    break;
		  }
	      }
	  }
	i.next();
      }
  }
  
  s << "\n";

  IND(s); s << "size_t\n";
  IND(s); s << fqname() << "::NP_alignedSize(size_t initialoffset) const\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::ULong _msgsize = initialoffset;\n";
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     disc_type(),
		     o2be_operation::wIN,mapping);
    o2be_operation::produceSizeCalculation(
		     s,
		     disc_type(),
		     ScopeAsDecl(defined_in()),
		     "",
		     "_msgsize",
		     "pd__d",
		     ntype,
		     mapping);
  }

  if (!(nodefault() && no_missing_disc_value()))
    {
      IND(s); s << "if (pd__default) {\n";
      INC_INDENT_LEVEL();
      {
	UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl* d = i.item();
	    if (d->node_type() == AST_Decl::NT_union_branch)
	      {
		AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
		if (l->label_kind() == AST_UnionLabel::UL_default)
		  {
		    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		    o2be_operation::argMapping mapping;
		    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
										   f->field_type(),
										   o2be_operation::wIN,mapping);
		    if (ntype == o2be_operation::tString) {
		      ntype = o2be_operation::tStringMember;
		      mapping.is_pointer = I_FALSE;
		    }
		    else if (ntype == o2be_operation::tObjref) {
		      ntype = o2be_operation::tObjrefMember;
		      mapping.is_pointer = I_FALSE;
		    }
		    else if (ntype == o2be_operation::tTypeCode) {
		      ntype = o2be_operation::tTypeCodeMember;
		      mapping.is_pointer = I_FALSE;
		    }

		    char *tmpname = new char [strlen("pd_")+strlen(f->uqname())+1];
		    strcpy(tmpname,"pd_");
		    strcat(tmpname,f->uqname());
		    o2be_operation::produceSizeCalculation(
							   s,
							   f->field_type(),
							   ScopeAsDecl(defined_in()),
							   "",
							   "_msgsize",
							   tmpname,
							   ntype,
							   mapping);
		    delete [] tmpname;
		    break;
		  }
	      }
	    i.next();
	  }
      }
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      IND(s); s << "else {\n";
    }
  INC_INDENT_LEVEL();
  IND(s); s << "switch(pd__d) {\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
	    if (l->label_kind() == AST_UnionLabel::UL_label)
	      {
		o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		IND(s); s << "case ";
		produce_disc_value(s,disc_type(),l->label_val(),this);
		s << ":\n";
		INC_INDENT_LEVEL();
		o2be_operation::argMapping mapping;
		o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     f->field_type(),
		     o2be_operation::wIN,mapping);
		if (ntype == o2be_operation::tString) {
		  ntype = o2be_operation::tStringMember;
		  mapping.is_pointer = I_FALSE;
		}
		else if (ntype == o2be_operation::tObjref) {
		  ntype = o2be_operation::tObjrefMember;
		  mapping.is_pointer = I_FALSE;
		}
		else if (ntype == o2be_operation::tTypeCode) {
		  ntype = o2be_operation::tTypeCodeMember;
		  mapping.is_pointer = I_FALSE;
		}

		char *tmpname = new char [strlen("pd_")+strlen(f->uqname())+1];
		strcpy(tmpname,"pd_");
		strcat(tmpname,f->uqname());
		o2be_operation::produceSizeCalculation(
		     s,
		     f->field_type(),
		     ScopeAsDecl(defined_in()),
		     "",
		     "_msgsize",
		     tmpname,
		     ntype,
		     mapping);
		delete [] tmpname;
		IND(s); s << "break;\n";
		DEC_INDENT_LEVEL();
	      }
	  }
	i.next();
      }
  }
  produce_default_break(*this, s);
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  if (!(nodefault() && no_missing_disc_value()))
    {
      IND(s); s << "}\n";
    }
  IND(s); s << "return _msgsize;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "void\n";
  IND(s); s << fqname() << "::operator>>= (NetBufferedStream& _n) const\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     disc_type(),
		     o2be_operation::wIN,mapping);
    o2be_operation::produceMarshalCode(
		     s,
		     disc_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     "pd__d",
		     ntype,
		     mapping);
  }
  if (!(nodefault() && no_missing_disc_value()))
    {
      IND(s); s << "if (pd__default) {\n";
      INC_INDENT_LEVEL();
      {
	UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl* d = i.item();
	    if (d->node_type() == AST_Decl::NT_union_branch)
	      {
		AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
		if (l->label_kind() == AST_UnionLabel::UL_default)
		  {
		    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		    o2be_operation::argMapping mapping;
		    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
										   f->field_type(),
										   o2be_operation::wIN,mapping);
		    if (ntype == o2be_operation::tString) {
		      ntype = o2be_operation::tStringMember;
		      mapping.is_pointer = I_FALSE;
		    }
		    else if (ntype == o2be_operation::tObjref) {
		      ntype = o2be_operation::tObjrefMember;
		      mapping.is_pointer = I_FALSE;
		    }
		    else if (ntype == o2be_operation::tTypeCode) {
		      ntype = o2be_operation::tTypeCodeMember;
		      mapping.is_pointer = I_FALSE;
		    }

		    char *tmpname = new char [strlen("pd_")+strlen(f->uqname())+1];
		    strcpy(tmpname,"pd_");
		    strcat(tmpname,f->uqname());
		    o2be_operation::produceMarshalCode(
						       s,
						       f->field_type(),
						       ScopeAsDecl(defined_in()),
						       "_n",
						       tmpname,
						       ntype,
						       mapping);
		    delete [] tmpname;
		    break;
		  }
	      }
	    i.next();
	  }
      }
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      IND(s); s << "else {\n";
    }
  INC_INDENT_LEVEL();
  IND(s); s << "switch(pd__d) {\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
	    if (l->label_kind() == AST_UnionLabel::UL_label)
	      {
		o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		IND(s); s << "case ";
		produce_disc_value(s,disc_type(),l->label_val(),this);
		s << ":\n";
		INC_INDENT_LEVEL();
		o2be_operation::argMapping mapping;
		o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     f->field_type(),
		     o2be_operation::wIN,mapping);
		if (ntype == o2be_operation::tString) {
		  ntype = o2be_operation::tStringMember;
		  mapping.is_pointer = I_FALSE;
		}
		else if (ntype == o2be_operation::tObjref) {
		  ntype = o2be_operation::tObjrefMember;
		  mapping.is_pointer = I_FALSE;
		}
		else if (ntype == o2be_operation::tTypeCode) {
		  ntype = o2be_operation::tTypeCodeMember;
		  mapping.is_pointer = I_FALSE;
		}

		char *tmpname = new char [strlen("pd_")+strlen(f->uqname())+1];
		strcpy(tmpname,"pd_");
		strcat(tmpname,f->uqname());
		o2be_operation::produceMarshalCode(
		     s,
		     f->field_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     tmpname,
		     ntype,
		     mapping);
		delete [] tmpname;
		IND(s); s << "break;\n";
		DEC_INDENT_LEVEL();
	      }
	  }
	i.next();
      }
  }
  produce_default_break(*this, s);
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  if (!(nodefault() && no_missing_disc_value()))
    {
      IND(s); s << "}\n";
    }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "void\n";
  IND(s); s << fqname() << "::operator<<= (NetBufferedStream& _n)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     disc_type(),
		     o2be_operation::wIN,mapping);
    o2be_operation::produceUnMarshalCode(
		     s,
		     disc_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     "pd__d",
		     ntype,
		     mapping);
  }

  IND(s); s << "switch(pd__d) {\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
	    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
	    if (l->label_kind() == AST_UnionLabel::UL_label)
	      {
		IND(s); s << "case ";
		produce_disc_value(s,disc_type(),l->label_val(),this);
		s << ":\n";
		INC_INDENT_LEVEL();
		IND(s); s << "pd__default = 0;\n";
	      }
	    else
	      {
		IND(s); s << "default:\n";
		INC_INDENT_LEVEL();
		IND(s); s << "pd__default = 1;\n";
	      }

	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     f->field_type(),
		     o2be_operation::wIN,mapping);
	    if (ntype == o2be_operation::tString) {
	      ntype = o2be_operation::tStringMember;
	      mapping.is_pointer = I_FALSE;
	    }
	    else if (ntype == o2be_operation::tObjref) {
	      ntype = o2be_operation::tObjrefMember;
	      mapping.is_pointer = I_FALSE;
	    }
	    else if (ntype == o2be_operation::tTypeCode) {
	      ntype = o2be_operation::tTypeCodeMember;
	      mapping.is_pointer = I_FALSE;
	    }

	    char *tmpname = new char [strlen("pd_")+strlen(f->uqname())+1];
	    strcpy(tmpname,"pd_");
	    strcat(tmpname,f->uqname());
	    o2be_operation::produceUnMarshalCode(
		     s,
		     f->field_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     tmpname,
		     ntype,
		     mapping);
	    delete [] tmpname;
	    IND(s); s << "break;\n";
	    DEC_INDENT_LEVEL();
	  }
	i.next();
      }
  }
  if (nodefault() && !no_missing_disc_value()) {
    IND(s); s << "default: pd__default = 1; break;\n";
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
  
  IND(s); s << "void\n";
  IND(s); s << fqname() << "::operator>>= (MemBufferedStream& _n) const\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     disc_type(),
		     o2be_operation::wIN,mapping);
    o2be_operation::produceMarshalCode(
		     s,
		     disc_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     "pd__d",
		     ntype,
		     mapping);
  }
  if (!(nodefault() && no_missing_disc_value()))
    {
      IND(s); s << "if (pd__default) {\n";
      INC_INDENT_LEVEL();
      {
	UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
	while (!i.is_done())
	  {
	    AST_Decl* d = i.item();
	    if (d->node_type() == AST_Decl::NT_union_branch)
	      {
		AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
		if (l->label_kind() == AST_UnionLabel::UL_default)
		  {
		    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		    o2be_operation::argMapping mapping;
		    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
										   f->field_type(),
										   o2be_operation::wIN,mapping);
		    if (ntype == o2be_operation::tString) {
		      ntype = o2be_operation::tStringMember;
		      mapping.is_pointer = I_FALSE;
		    }
		    else if (ntype == o2be_operation::tObjref) {
		      ntype = o2be_operation::tObjrefMember;
		      mapping.is_pointer = I_FALSE;
		    }
		    else if (ntype == o2be_operation::tTypeCode) {
		      ntype = o2be_operation::tTypeCodeMember;
		      mapping.is_pointer = I_FALSE;
		    }

		    char *tmpname = new char [strlen("pd_")+strlen(f->uqname())+1];
		    strcpy(tmpname,"pd_");
		    strcat(tmpname,f->uqname());
		    o2be_operation::produceMarshalCode(
						       s,
						       f->field_type(),
						       ScopeAsDecl(defined_in()),
						       "_n",
						       tmpname,
						       ntype,
						       mapping);
		    delete [] tmpname;
		    break;
		  }
	      }
	    i.next();
	  }
      }
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      IND(s); s << "else {\n";
    }
  INC_INDENT_LEVEL();
  IND(s); s << "switch(pd__d) {\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
	    if (l->label_kind() == AST_UnionLabel::UL_label)
	      {
		o2be_field *f = o2be_union_branch::narrow_from_decl(d);
		IND(s); s << "case ";
		produce_disc_value(s,disc_type(),l->label_val(),this);
		s << ":\n";
		INC_INDENT_LEVEL();
		o2be_operation::argMapping mapping;
		o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     f->field_type(),
		     o2be_operation::wIN,mapping);
		if (ntype == o2be_operation::tString) {
		  ntype = o2be_operation::tStringMember;
		  mapping.is_pointer = I_FALSE;
		}
		else if (ntype == o2be_operation::tObjref) {
		  ntype = o2be_operation::tObjrefMember;
		  mapping.is_pointer = I_FALSE;
		}
		else if (ntype == o2be_operation::tTypeCode) {
		  ntype = o2be_operation::tTypeCodeMember;
		  mapping.is_pointer = I_FALSE;
		}

		char *tmpname = new char [strlen("pd_")+strlen(f->uqname())+1];
		strcpy(tmpname,"pd_");
		strcat(tmpname,f->uqname());
		o2be_operation::produceMarshalCode(
		     s,
		     f->field_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     tmpname,
		     ntype,
		     mapping);
		delete [] tmpname;
		IND(s); s << "break;\n";
		DEC_INDENT_LEVEL();
	      }
	  }
	i.next();
      }
  }
  produce_default_break(*this, s);
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  if (!(nodefault() && no_missing_disc_value()))
    {
      IND(s); s << "}\n";
    }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "void\n";
  IND(s); s << fqname() << "::operator<<= (MemBufferedStream& _n)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     disc_type(),
		     o2be_operation::wIN,mapping);
    o2be_operation::produceUnMarshalCode(
		     s,
		     disc_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     "pd__d",
		     ntype,
		     mapping);
  }

  IND(s); s << "switch(pd__d) {\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_UnionLabel* l =o2be_union_branch::narrow_from_decl(d)->label();
	    o2be_field *f = o2be_union_branch::narrow_from_decl(d);
	    if (l->label_kind() == AST_UnionLabel::UL_label)
	      {
		IND(s); s << "case ";
		produce_disc_value(s,disc_type(),l->label_val(),this);
		s << ":\n";
		INC_INDENT_LEVEL();
		IND(s); s << "pd__default = 0;\n";
	      }
	    else
	      {
		IND(s); s << "default:\n";
		INC_INDENT_LEVEL();
		IND(s); s << "pd__default = 1;\n";
	      }
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     f->field_type(),
		     o2be_operation::wIN,mapping);
	    if (ntype == o2be_operation::tString) {
	      ntype = o2be_operation::tStringMember;
	      mapping.is_pointer = I_FALSE;
	    }
	    else if (ntype == o2be_operation::tObjref) {
	      ntype = o2be_operation::tObjrefMember;
	      mapping.is_pointer = I_FALSE;
	    }
	    else if (ntype == o2be_operation::tTypeCode) {
	      ntype = o2be_operation::tTypeCodeMember;
	      mapping.is_pointer = I_FALSE;
	    }

	    char *tmpname = new char [strlen("pd_")+strlen(f->uqname())+1];
	    strcpy(tmpname,"pd_");
	    strcat(tmpname,f->uqname());
	    o2be_operation::produceUnMarshalCode(
		     s,
		     f->field_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     tmpname,
		     ntype,
		     mapping);
	    delete [] tmpname;
	    IND(s); s << "break;\n";
	    DEC_INDENT_LEVEL();
	  }
	i.next();
      }
  }
  if (nodefault() && !no_missing_disc_value()) {
    IND(s); s << "default: pd__default = 1; break;\n";
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void
o2be_union::produce_dynskel(std::fstream& s)
{
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_Decl* decl=AST_UnionBranch::narrow_from_decl(d)->field_type();
	    if (decl->has_ancestor(this))
	      {
		switch (decl->node_type())
		  {
		  case AST_Decl::NT_enum:
		    if (!o2be_enum::narrow_from_decl(decl)
			       ->get_dynskel_produced_in_field()) {
		      o2be_enum::narrow_from_decl(decl)
			->set_dynskel_produced_in_field();
		      o2be_enum::narrow_from_decl(decl)->produce_dynskel(s);
		    }
		    break;
		  case AST_Decl::NT_struct:
		    if (!o2be_structure::narrow_from_decl(decl)
			       ->get_dynskel_produced_in_field()) {
		      o2be_structure::narrow_from_decl(decl)
			->set_dynskel_produced_in_field();
		      o2be_structure::narrow_from_decl(decl)
			->produce_dynskel(s);
		    }
		    break;
		  case AST_Decl::NT_union:
		    if (!o2be_union::narrow_from_decl(decl)
			       ->get_dynskel_produced_in_field()) {
		      o2be_union::narrow_from_decl(decl)
			->set_dynskel_produced_in_field();
		      o2be_union::narrow_from_decl(decl)->produce_dynskel(s);
		    }
		    break;
		  default:
		    break;
		  }
	      }
	  }
	i.next();
      }
  }

  // Produce code for types any and TypeCode
  this->produce_typecode_skel(s);

  if (defined_in() != idl_global->root() &&
      defined_in()->scope_node_type() == AST_Decl::NT_module)
    {
      s << "\n#if defined(HAS_Cplusplus_Namespace) && defined(_MSC_VER)\n";
      IND(s); s << "// MSVC++ does not give the constant external"
		" linkage otherwise.\n";
      AST_Decl* inscope = ScopeAsDecl(defined_in());
      char* scopename = o2be_name::narrow_and_produce_uqname(inscope);
      if (strcmp(scopename,o2be_name::narrow_and_produce_fqname(inscope)))
	{
	  scopename = o2be_name::narrow_and_produce__fqname(inscope);
	  IND(s); s << "namespace " << scopename << " = " 
		    << o2be_name::narrow_and_produce_fqname(inscope)
		    << ";\n";
	}
      IND(s); s << "namespace " << scopename << " {\n";
      INC_INDENT_LEVEL();
      IND(s); s << "const CORBA::TypeCode_ptr " << tcname() << " = " 
		<< "_0RL_tc_" << _idname() << ";\n";
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      s << "#else\n";
      IND(s); s << "const CORBA::TypeCode_ptr " << fqtcname() << " = " 
		<< "_0RL_tc_" << _idname() << ";\n";
      s << "#endif\n\n";
    }
  else
    {
      IND(s); s << "const CORBA::TypeCode_ptr " << fqtcname() << " = " 
		<< "_0RL_tc_" << _idname() << ";\n\n";
    }
}


void
o2be_union::produce_binary_operators_in_hdr(std::fstream &s)
{
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_Decl* decl=AST_UnionBranch::narrow_from_decl(d)->field_type();
	    if (decl->has_ancestor(this))
	      {
		switch (decl->node_type())
		  {
		  case AST_Decl::NT_enum:
		    if (!o2be_enum::narrow_from_decl(decl)
			  ->get_binary_operators_hdr_produced_in_field()) 
		      {
			o2be_enum::narrow_from_decl(decl)
			  ->set_binary_operators_hdr_produced_in_field();
			o2be_enum::narrow_from_decl(decl)
			  ->produce_binary_operators_in_hdr(s);
		      }
		    break;
		  case AST_Decl::NT_struct:
		    if (!o2be_structure::narrow_from_decl(decl)
			  ->get_binary_operators_hdr_produced_in_field()) 
		      {
			o2be_structure::narrow_from_decl(decl)
			  ->set_binary_operators_hdr_produced_in_field();
			o2be_structure::narrow_from_decl(decl)
			  ->produce_binary_operators_in_hdr(s);
		      }
		    break;
		  case AST_Decl::NT_union:
		    if (!o2be_union::narrow_from_decl(decl)
			  ->get_binary_operators_hdr_produced_in_field()) 
		      {
			o2be_union::narrow_from_decl(decl)
			  ->set_binary_operators_hdr_produced_in_field();
			o2be_union::narrow_from_decl(decl)
			  ->produce_binary_operators_in_hdr(s);
		      }
		    break;
		  default:
		    break;
		  }
	      }
	  }
	i.next();
      }
  }

  if (idl_global->compile_flags() & IDL_CF_ANY) {
    // any insertion and extraction operators
    IND(s); s << "void operator<<=(CORBA::Any& _a, const " 
	      << fqname() << "& _s);\n";
    IND(s); s << "void operator<<=(CORBA::Any& _a, " 
	      << fqname() <<"* _sp);\n";
    IND(s); s << "CORBA::Boolean operator>>=(const CORBA::Any& _a, " 
	      << fqname() << "*& _sp);\n\n";
  }
}


void
o2be_union::produce_binary_operators_in_dynskel(std::fstream& s)
{
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch)
	  {
	    AST_Decl* decl=AST_UnionBranch::narrow_from_decl(d)->field_type();
	    if (decl->has_ancestor(this))
	      {
		switch (decl->node_type())
		  {
		  case AST_Decl::NT_enum:
		    if (!o2be_enum::narrow_from_decl(decl)
			  ->get_binary_operators_skel_produced_in_field()) 
		      {
			o2be_enum::narrow_from_decl(decl)
			  ->set_binary_operators_skel_produced_in_field();
			o2be_enum::narrow_from_decl(decl)
			  ->produce_binary_operators_in_dynskel(s);
		      }
		    break;
		  case AST_Decl::NT_struct:
		    if (!o2be_structure::narrow_from_decl(decl)
			  ->get_binary_operators_skel_produced_in_field()) 
		      {
			o2be_structure::narrow_from_decl(decl)
			  ->set_binary_operators_skel_produced_in_field();
			o2be_structure::narrow_from_decl(decl)
			  ->produce_binary_operators_in_dynskel(s);
		      }
		    break;
		  case AST_Decl::NT_union:
		    if (!o2be_union::narrow_from_decl(decl)
			  ->get_binary_operators_skel_produced_in_field()) 
		      {
			o2be_union::narrow_from_decl(decl)
			  ->set_binary_operators_skel_produced_in_field();
			o2be_union::narrow_from_decl(decl)
			  ->produce_binary_operators_in_dynskel(s);
		      }
		    break;
		  default:
		    break;
		  }
	      }
	  }
	i.next();
      }
  }

  //////////////////////////////////////////////////////////////////////
  //////////////////////// tcDescriptor generation /////////////////////
  //////////////////////////////////////////////////////////////////////

  // Pre-declare tcParser_buildDesc functions for types defined
  // elsewhere, and generate code for anonymous array members.
  // Ensure we have buildDesc support for all the members.
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while( !i.is_done() ) {
      AST_Decl* d = i.item();

      if( d->node_type() == AST_Decl::NT_union_branch ) {
	AST_Decl* ft = o2be_union_branch::narrow_from_decl(d)->field_type();
	o2be_buildDesc::produce_decls(s, ft);
      }

      i.next();
    }
  }
  // If the discriminator type is defined in another file
  // then we also need to declare the buildDesc function
  // for it.
  o2be_buildDesc::produce_decls(s, disc_type());

  s << '\n';

  // Put the buildDesc helper functions into a helper class. This means
  // that the union class need only have one friend, and avoids a bug
  // in MSVC.
  IND(s); s << "class _0RL_tcParser_unionhelper_" << _idname() << " {\n";
  IND(s); s << "public:\n";
  INC_INDENT_LEVEL();

  // getDiscriminator - build a tcDescriptor for the
  // union discriminator. Also return the value of the
  // discriminator in <_discrim>.
  IND(s); s << "static void getDiscriminator(tcUnionDesc* _desc, "
	    "tcDescriptor& _newdesc, CORBA::PR_unionDiscriminator& _discrim)"
	    " {\n";
  INC_INDENT_LEVEL();
  IND(s); s << fqname() << "* _u = (" << fqname() << "*)_desc->opq_union;\n";
  o2be_buildDesc::call_buildDesc(s, disc_type(), "_newdesc", "_u->pd__d");
  IND(s); s << "_discrim = (CORBA::PR_unionDiscriminator)_u->pd__d;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  // setDiscriminator - set the value of the union discriminator.
  IND(s); s << "static void setDiscriminator(tcUnionDesc* _desc, "
	    "CORBA::PR_unionDiscriminator _discrim, int _is_default) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << fqname() << "* _u = (" << fqname() << "*)_desc->opq_union;\n";
  IND(s); s << "_u->pd__d = ("
	    << o2be_name::narrow_and_produce_fqname(disc_type())
	    << ")_discrim;\n";
  IND(s); s << "_u->pd__default = _is_default;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  // getValueDesc - build a tcDescriptor for the union's
  // selected value.
  IND(s); s << "static CORBA::Boolean getValueDesc(tcUnionDesc* _desc, "
	    "tcDescriptor& _newdesc) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << fqname() << "* _u = (" << fqname() << "*)_desc->opq_union;\n";
  if( !nodefault() ) {
    IND(s); s << "if( _u->pd__default ) {\n";
    INC_INDENT_LEVEL();
    {
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while( !i.is_done() ) {
	AST_Decl* d = i.item();
	if (d->node_type() == AST_Decl::NT_union_branch) {
	  o2be_union_branch* ub = o2be_union_branch::narrow_from_decl(d);
	  AST_UnionLabel* l = ub->label();
	  if( l->label_kind() == AST_UnionLabel::UL_default ) {
	    char* val = new char[1 + 7 + strlen(ub->uqname())];
	    strcpy(val, "_u->pd_");
	    strcat(val, ub->uqname());
	    o2be_buildDesc::call_buildDesc(s, ub->field_type(),
					   "_newdesc", val);
	    delete[] val;
	    break;
	  }
	}
	i.next();
      }
    }
    DEC_INDENT_LEVEL();
    IND(s); s << "} else {\n";
    INC_INDENT_LEVEL();
  }
  IND(s); s << "switch( _u->pd__d ) {\n";
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while( !i.is_done() ) {
      AST_Decl* d = i.item();
      if (d->node_type() == AST_Decl::NT_union_branch)
	{
	  o2be_union_branch* ub = o2be_union_branch::narrow_from_decl(d);
	  AST_UnionLabel* l = ub->label();
	  if (l->label_kind() == AST_UnionLabel::UL_label)
	    {
	      IND(s); s << "case ";
	      produce_disc_value(s, disc_type(), l->label_val(), this, I_TRUE);
	      s << ":\n";
	      INC_INDENT_LEVEL();
	      char* val = new char[1 + 7 + strlen(ub->uqname())];
	      strcpy(val, "_u->pd_");
	      strcat(val, ub->uqname());
	      o2be_buildDesc::call_buildDesc(s, ub->field_type(),
					     "_newdesc", val);
	      delete[] val;
	      IND(s); s << "break;\n";
	      DEC_INDENT_LEVEL();
	    }
	}
      i.next();
    }
  }
  if( !no_missing_disc_value() ) {
    IND(s); s << "default: return 0;\n";
  }
  IND(s); s << "}\n";
  if( !nodefault() ) {
    DEC_INDENT_LEVEL();
    IND(s); s << "}\n";
  }
  IND(s); s << "return 1;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  // End of helper class.
  DEC_INDENT_LEVEL();
  IND(s); s << "};\n\n";

  // tcParser function to build a tcDescriptor for this class.
  IND(s); s << "void _0RL_buildDesc" << canonical_name()
	    << "(tcDescriptor& _desc, "
	    << "const " << fqname() << "& _data)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_desc.p_union.getDiscriminator = _0RL_tcParser_unionhelper_"
	    << _idname() << "::getDiscriminator;\n";
  IND(s); s << "_desc.p_union.setDiscriminator = _0RL_tcParser_unionhelper_"
	    << _idname() << "::setDiscriminator;\n";
  IND(s); s << "_desc.p_union.getValueDesc = _0RL_tcParser_unionhelper_"
	    << _idname() << "::getValueDesc;\n";
  IND(s); s << "_desc.p_union.opq_union = (void*)&_data;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  // Any storage management function - Any has to manage
  // the data extracted from it. This function is needed
  // to release the storage and call the d'tor.
  IND(s); s << "void _0RL_delete_" << _idname() << "(void* _data)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << fqname() << "* _0RL_t = (" << fqname() << "*) _data;\n";
  IND(s); s << "delete _0RL_t;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  //////////////////////////////////////////////////////////////////////
  /////////////////////// Any insertion operators //////////////////////
  //////////////////////////////////////////////////////////////////////

  IND(s); s << "void operator<<=(CORBA::Any& _a, const "
	    << fqname() << "& _s)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "tcDescriptor _0RL_tcdesc;\n";
  o2be_buildDesc::call_buildDesc(s, this, "_0RL_tcdesc", "_s");
  IND(s); s << "_a.PR_packFrom(_0RL_tc_" << _idname()
	    << ", &_0RL_tcdesc);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  //////////////////////////////////////////////////////////////////////
  //////////////////////// Any extraction operator /////////////////////
  //////////////////////////////////////////////////////////////////////

  IND(s); s << "CORBA::Boolean operator>>=(const CORBA::Any& _a, "
	    << fqname() << "*& _sp) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_sp = (" << fqname() << " *) _a.PR_getCachedData();\n";
  IND(s); s << "if (_sp == 0) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "tcDescriptor _0RL_tcdesc;\n";
  IND(s); s << "_sp = new " << fqname() << ";\n";
  o2be_buildDesc::call_buildDesc(s, this, "_0RL_tcdesc", "*_sp");
  IND(s); s << "if( _a.PR_unpackTo(_0RL_tc_" << _idname()
	    << ", &_0RL_tcdesc) ) {\n";
  INC_INDENT_LEVEL();
  // We take the address and cast to get past the
  // const qualifier on <_a>.
  IND(s); s << "((CORBA::Any*)&_a)->PR_setCachedData(_sp, "
	    << "_0RL_delete_" << _idname() << ");\n";
  IND(s); s << "return 1;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "} else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "delete _sp;\n";
  IND(s); s << "_sp = 0;\n";
  IND(s); s << "return 0;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "} else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::TypeCode_var _0RL_tctmp = _a.type();\n";
  IND(s); s << "if (_0RL_tctmp->equal(_0RL_tc_" << _idname()
	    << ")) return 1;\n";
  IND(s); s << "_sp = 0;\n";
  IND(s); s << "return 0;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void
o2be_union::produce_typecode_skel(std::fstream& s)
{
  if( have_produced_typecode_skel() )  return;
  set_have_produced_typecode_skel();

  { // Ensure we have the typecodes of the members ...
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while( !i.is_done() ) {
      AST_Decl* d = i.item();
      i.next();
      if( d->node_type() != AST_Decl::NT_union_branch )
	continue;
      d = AST_UnionBranch::narrow_from_decl(d)->field_type();
      o2be_name::narrow_and_produce_typecode_skel(d, s);
    }
  }
  // ... and the discriminant type.
  o2be_name::narrow_and_produce_typecode_skel(disc_type(), s);

  // Create an array of PR_unionMember to describe the members.
  unsigned int memberCount = 0;
  unsigned int defaultMember = 0;

  IND(s); s << "static CORBA::PR_unionMember _0RL_unionMember_"
	    << _idname() << "[] = {\n";
  INC_INDENT_LEVEL();
  {
    // Produce entries in PR_unionMember for union members
    // (name, TypeCode_ptr, and label value)

    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while( !i.is_done() ) {
      AST_Decl* d = i.item();
      i.next();
      if( d->node_type() != AST_Decl::NT_union_branch )
	continue;

      IND(s); s << "{\"" 
		<< o2be_union_branch::narrow_from_decl(d)->uqname() 
		<< "\", ";

      AST_Decl* decl = AST_UnionBranch::narrow_from_decl(d)->field_type();
      o2be_name::produce_typecode_member(decl,s);
      s << ", ";

      AST_UnionLabel *l = o2be_union_branch::narrow_from_decl(d)->label();
      if (l->label_kind() == AST_UnionLabel::UL_label) {
	AST_ConcreteType *ct = disc_type();
	if (ct->node_type() == AST_Decl::NT_enum) {
	  AST_Decl* v = 
	    AST_Enum::narrow_from_decl(ct)->lookup_by_value(l->label_val());
	  s << o2be_name::narrow_and_produce_fqname(v);
	} else
	  produce_disc_value(s, disc_type(), l->label_val(), this, I_TRUE);
      } else {
	// this label is default
	s << "0";
	defaultMember = memberCount;
      }
      s << "}";
      memberCount++;
      if( i.is_done() )  s << '\n';
      else               s << ",\n";    
    }
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "};\n";
  
  IND(s); s << "static CORBA::TypeCode_ptr _0RL_tc_" << _idname() << " = "
	    << "CORBA::TypeCode::PR_union_tc(\"" << repositoryID()
	    << "\", \"" << uqname() << "\", ";
  o2be_name::produce_typecode_member(disc_type(), s);
  s << ", _0RL_unionMember_" << _idname() << ", " << memberCount;
  if( !nodefault() )  s << ", " << defaultMember;
  s << ");\n\n";
}


void
o2be_union::produce_decls_at_global_scope_in_hdr(std::fstream& s)
{
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
        AST_Decl* d = i.item();
        if (d->node_type() == AST_Decl::NT_union_branch)
          {
            AST_Decl* decl =AST_UnionBranch::narrow_from_decl(d)->field_type();
            if (decl->has_ancestor(this))
              {
                switch (decl->node_type())
                  {
                  case AST_Decl::NT_struct:
		    o2be_structure::narrow_from_decl(decl)
		      ->produce_decls_at_global_scope_in_hdr(s);
		    break;
                  case AST_Decl::NT_union:
		    o2be_union::narrow_from_decl(decl)
		      ->produce_decls_at_global_scope_in_hdr(s);
		    break;
                  default:
                    break;
                  }
              }
          }
        i.next();
      }
  }

  if (idl_global->compile_flags() & IDL_CF_ANY) {
    IND(s); s << "// Declare helper class for union type " << fqname() << "\n";
    IND(s); s << "class _0RL_tcParser_unionhelper_" << _idname() << ";\n\n";
  }
}


void
o2be_union::produce_typedef_hdr(std::fstream& s, o2be_typedef* tdef)
{
  IND(s); s << "typedef " << unambiguous_name(tdef)
	    << " " << tdef->uqname() << ";\n";
  IND(s); s << "typedef " << unambiguous_name(tdef)
	    << "_var " << tdef->uqname() << "_var;\n";
}


idl_bool
o2be_union::no_missing_disc_value()
{
  //?? We should cache the result of this computation. It is
  // called quite a few times for any particular union.

  AST_Decl* decl = disc_type();
  while (decl->node_type() == AST_Decl::NT_typedef)
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();

  switch (decl->node_type())
    {
    case AST_Decl::NT_enum:
      {
	disc_value_t v;
	UTL_ScopeActiveIterator i(o2be_enum::narrow_from_decl(decl),
				  UTL_Scope::IK_decls);
	while (!(i.is_done())) {
	  v.e_val = i.item();
	  if (!lookup_by_disc_value(*this,v)) {
	    return I_FALSE;
	  }
	  i.next();
	}
	break;
      }
    case AST_Decl::NT_pre_defined:
      switch (AST_PredefinedType::narrow_from_decl(decl)->pt())
	{
	case AST_PredefinedType::PT_long:
	  {
	    disc_value_t v;
	    v.i_val = -((int)0x7fffffff);
	    while (v.i_val < ((int)0x7fffffff)) {
	      if (!lookup_by_disc_value(*this,v)) {
		return I_FALSE;
	      }
	      v.i_val++;
	    }
	    if (!lookup_by_disc_value(*this,v))
	      return I_FALSE;
	    break;
	  }
	case AST_PredefinedType::PT_ulong:
	  {
	    disc_value_t v;
	    v.ui_val = 0;
	    while (v.ui_val < ((unsigned int)0xffffffff)) {
	      if (!lookup_by_disc_value(*this,v)) {
		return I_FALSE;
	      }
	      v.ui_val++;
	    }
	    if (!lookup_by_disc_value(*this,v))
	      return I_FALSE;
	    break;
	  }
	case AST_PredefinedType::PT_short:
	  {
	    disc_value_t v;
	    v.s_val = -((short)0x7fff);
	    while (v.s_val < ((short)0x7ffff)) {
	      if (!lookup_by_disc_value(*this,v)) {
		return I_FALSE;
	      }
	      v.s_val++;
	    }
	    if (!lookup_by_disc_value(*this,v))
	      return I_FALSE;
	    break;
	  }
	case AST_PredefinedType::PT_ushort:
	  {
	    disc_value_t v;
	    v.us_val = 0;
	    while (v.us_val < ((unsigned short)0xffff)) {
	      if (!lookup_by_disc_value(*this,v)) {
		return I_FALSE;
	      }
	      v.us_val++;
	    }
	    if (!lookup_by_disc_value(*this,v))
	      return I_FALSE;
	    break;
	  }
	case AST_PredefinedType::PT_char:
	  {
	    disc_value_t v;
	    v.c_val = '\0';
	    while (v.c_val < '\177') {
	      if (!lookup_by_disc_value(*this,v)) {
		return I_FALSE;
	      }
	      v.c_val++;
	    }
	    if (!lookup_by_disc_value(*this,v))
	      return I_FALSE;
	    break;
	  }
	case AST_PredefinedType::PT_boolean:
	  {
	    disc_value_t v;
	    v.b_val = I_TRUE;
	    if (!lookup_by_disc_value(*this,v))
	      return I_FALSE;
	    else
	      {
		v.b_val = I_FALSE;
		if (!lookup_by_disc_value(*this,v))
		  return I_FALSE;
	      }
	    break;
	  }
	default:
	  //?? This ought to be caught earlier - but it isn't!!!
	  throw o2be_internal_error(__FILE__,__LINE__,
				    "Unexpected union discriminant value");
	  break;
	}
      break;
    default:
      throw o2be_internal_error(__FILE__,__LINE__,
				"Unexpected union discriminant value");
      break;
    }
  return I_TRUE;
}


static
void
produce_default_value(o2be_union& u,std::fstream& s)
{
  AST_Decl* decl = u.disc_type();
  while (decl->node_type() == AST_Decl::NT_typedef)
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();

  switch (decl->node_type())
    {
    case AST_Decl::NT_enum:
      {
	disc_value_t v;
	UTL_ScopeActiveIterator i(o2be_enum::narrow_from_decl(decl),
				  UTL_Scope::IK_decls);
	while (!(i.is_done())) {
	  v.e_val = i.item();
	  if (!lookup_by_disc_value(u,v)) {
	    s << o2be_name::narrow_and_produce_unambiguous_name(v.e_val,&u);
	    break;
	  }
	  i.next();
	}
	break;
      }
    case AST_Decl::NT_pre_defined:
      switch (AST_PredefinedType::narrow_from_decl(decl)->pt())
	{
	case AST_PredefinedType::PT_long:
	  {
	    disc_value_t v;
	    v.i_val = -((int)0x7fffffff);
	    while (lookup_by_disc_value(u,v)) {
	      v.i_val++;
	    }
	    s << v.i_val;
	    break;
	  }
	case AST_PredefinedType::PT_ulong:
	  {
	    disc_value_t v;
	    v.ui_val = 0;
	    while (lookup_by_disc_value(u,v)) {
	      v.ui_val++;
	    }
	    s << v.ui_val;
	    break;
	  }
	case AST_PredefinedType::PT_short:
	  {
	    disc_value_t v;
	    v.s_val = -((short)0x7fff);
	    while (lookup_by_disc_value(u,v)) {
	      v.s_val++;
	    }
	    s << v.s_val;
	    break;
	  }
	case AST_PredefinedType::PT_ushort:
	  {
	    disc_value_t v;
	    v.us_val = 0;
	    while (lookup_by_disc_value(u,v)) {
	      v.us_val++;
	    }
	    s << v.us_val;
	    break;
	  }
	case AST_PredefinedType::PT_char:
	  {
	    disc_value_t v;
	    v.c_val = '\0';
	    while (lookup_by_disc_value(u,v)) {
	      v.c_val++;
	    }
	    if (v.c_val >= ' ' && v.c_val <= '~')
	      s << "'" << v.c_val << "'";
	    else {
	      s << "'\\"
		<< (int) ((v.c_val & 0100) >> 6)
		<< (int) ((v.c_val & 070) >> 3)
		<< (int) (v.c_val & 007)
		<< "'";
	    }
	    break;
	  }
	case AST_PredefinedType::PT_boolean:
	  {
	    disc_value_t v;
	    v.b_val = I_TRUE;
	    if (lookup_by_disc_value(u,v))
	      s << "0";
	    else
	      s << "1";
	    break;
	  }
	default:
	  throw o2be_internal_error(__FILE__,__LINE__,
				    "Unexpected union discriminant value");
	  break;
	}
      break;
    default:
      throw o2be_internal_error(__FILE__,__LINE__,
				"Unexpected union discriminant value");
      break;
    }
  return;
}


static
void
produce_disc_value(std::fstream& s, AST_ConcreteType* t,
		   AST_Expression* exp, AST_Decl* used_in,
		   idl_bool use_fqname)
{
  if (t->node_type() != AST_Decl::NT_enum)
    {
      AST_Expression::AST_ExprValue *v = exp->ev();
      switch (v->et) 
	{
	case AST_Expression::EV_short:
	  s << v->u.sval;
	  break;
	case AST_Expression::EV_ushort:
	  s << v->u.usval;
	  break;
	case AST_Expression::EV_long:
	  s << v->u.lval;
	  break;
	case AST_Expression::EV_ulong:
	  s << v->u.ulval;
	  break;
	case AST_Expression::EV_bool:
	  s << ((v->u.bval == 0) ? "0" : "1");
	  break;
	case AST_Expression::EV_char:
	  {
	    char c = v->u.cval;
	    if (c >= ' ' && c <= '~')
	      s << "'" << c << "'";
	    else {
	      s << "'\\"
		<< (int) ((c & 0100) >> 6)
		<< (int) ((c & 070) >> 3)
		<< (int) (c & 007)
		<< "'";
	    }
	  }
	  break;
	default:
	  throw o2be_internal_error(__FILE__,__LINE__,
				    "Unexpected union discriminant value");

	}
    }
  else
    {
      AST_Decl* v = AST_Enum::narrow_from_decl(t)->lookup_by_value(exp);
      s << o2be_name::narrow_and_produce_unambiguous_name(v, used_in,
							  use_fqname);
    }
}

static idl_bool
match_disc_value(o2be_union_branch& b,AST_Decl* decl,disc_value_t v)
{
  AST_UnionLabel* l = b.label();
  
  if (l->label_kind() == AST_UnionLabel::UL_default)
    return I_FALSE;

  while (decl->node_type() == AST_Decl::NT_typedef)
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();

  switch (decl->node_type())
    {
    case AST_Decl::NT_enum:
      {
	AST_Decl* bv = AST_Enum::narrow_from_decl(decl)->lookup_by_value(l->label_val());
	if (bv == v.e_val)
	  return I_TRUE;
	break;
      }
    case AST_Decl::NT_pre_defined:
      {
	AST_Expression::AST_ExprValue *bv = l->label_val()->ev();
	switch (AST_PredefinedType::narrow_from_decl(decl)->pt())
	  {
	  case AST_PredefinedType::PT_long:
	    if (bv->u.lval == v.i_val)
	      return I_TRUE;
	    break;
	  case AST_PredefinedType::PT_ulong:
	    if (bv->u.ulval == v.ui_val)
	      return I_TRUE;
	    break;
	  case AST_PredefinedType::PT_short:
	    if (bv->u.sval == v.s_val)
	      return I_TRUE;
	    break;
	  case AST_PredefinedType::PT_ushort:
	    if (bv->u.usval == v.us_val)
	      return I_TRUE;
	    break;
	  case AST_PredefinedType::PT_char:
	    if (bv->u.cval == v.c_val)
	      return I_TRUE;
	    break;
	  case AST_PredefinedType::PT_boolean:
	    if (bv->u.bval == (unsigned long)v.b_val)
	      return I_TRUE;
	    break;
	  default:
	    throw o2be_internal_error(__FILE__,__LINE__,
				      "Unexpected union discriminant value");
	    break;
	  }
	break;
      }
    default:
      throw o2be_internal_error(__FILE__,__LINE__,
				"Unexpected union discriminant value");
      break;
    }
  return I_FALSE;
}


static o2be_union_branch*
lookup_by_disc_value(o2be_union& u,disc_value_t v)
{
  UTL_ScopeActiveIterator i(&u,UTL_Scope::IK_decls);
  while (!i.is_done())
    {
      AST_Decl* d = i.item();
      if (d->node_type() == AST_Decl::NT_union_branch)
	{
	  o2be_union_branch *b = o2be_union_branch::narrow_from_decl(d);
	  if (match_disc_value(*(b),u.disc_type(),v))
	    return b;
	}
      i.next();
    }
  return 0;
}


static void
produce_default_break(o2be_union& u, std::fstream& s)
{

// I actually question whether this was needed at all.  In each call the line
// "default: break;" is generated at the very end of the switch statement.
// Semantically this is useless code, but maybe there was a reason it was
// there.
// In particular, if both true and false are specified for a boolean
// discriminator, this causes a warning on MSVC when CORBA::Boolean is a "real"
// C++ bool type.

// bcv 23-FEB-1998 12:59:02.59

  AST_Decl* decl = u.disc_type();
  while (decl->node_type() == AST_Decl::NT_typedef)
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();

  if (decl->node_type()==AST_Decl::NT_pre_defined &&
      AST_PredefinedType::narrow_from_decl(decl)->pt()==
	AST_PredefinedType::PT_boolean &&
      u.nodefault() &&
      u.no_missing_disc_value()) {
    s << "#ifndef HAS_Cplusplus_Bool\n";
    IND(s); s << "default: break;\n";
    s << "#endif\n";
  } else {
    IND(s); s << "default: break;\n";
  }
}

const char*
o2be_union::out_adptarg_name(AST_Decl* used_in) const
{
  if (o2be_global::qflag()) {
    return pd_out_adptarg_name;
  }
  else {
    const char* ubname = unambiguous_name(used_in);
    if (strcmp(fqname(),ubname) == 0) {
      return pd_out_adptarg_name;
    }
    else {
      char* result = new char[strlen(ADPT_CLASS_TEMPLATE)+strlen("<,>")+
		       strlen(ubname)+
		       strlen(ubname)+strlen("_var")+1];
      strcpy(result,ADPT_CLASS_TEMPLATE);
      strcat(result,"<");
      strcat(result,ubname);
      strcat(result,",");
      strcat(result,ubname);
      strcat(result,"_var>");  
      return result;
    }
  }
}


IMPL_NARROW_METHODS1(o2be_union, AST_Union)
IMPL_NARROW_FROM_DECL(o2be_union)
IMPL_NARROW_FROM_SCOPE(o2be_union)
