// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_struct.cc           Created on: 12/08/1996
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
//   OMNI BE for the class AST_Struct
//

/*
  $Log: o2be_struct.cc,v $
  Revision 1.1  1999/02/14 17:45:29  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.10  1999/01/07 09:36:08  djr
  Changes to support new TypeCode/Any implementation, which is now
  placed in a new file ...DynSK.cc (by default).

  Revision 1.9  1998/08/19 15:54:12  sll
  New member functions void produce_binary_operators_in_hdr and the like
  are responsible for generating binary operators <<= etc in the global
  namespace.

  Revision 1.8  1998/08/13 22:46:14  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available.

  Revision 1.7  1998/04/07 18:52:31  sll
  Use std::fstream instead of fstream.
  Stub code modified to accommodate the use of namespace to represent module.

// Revision 1.6  1998/01/27  16:49:47  ewc
//  Added support for type Any and TypeCode
//
  Revision 1.5  1997/12/09 19:55:08  sll
  *** empty log message ***

// Revision 1.4  1997/05/06  14:08:26  sll
// Public release.
//
  */

/*
  Example:

  // IDL

  struct fixedlen {
     short a;
     long  b;
  };

  // C++
  struct fixedlen {
     CORBA::Short a;
     CORBA::Long  b;
  };

  class fixedlen_var {
  public:
     fixedlen_var();
     fixedlen_var(fixedlen *);
     fixedlen_var(const fixedlen_var &);
     ~fixedlen_var();
     fixedlen_var &operator=(fixedlen *);
     fixedlen_var &operator=(const fixedlen_var &);
     fixedlen *operator-> const ();

     // conversion operators to support parameter passing
     operator fixedlen &();
     operator fixedlen *&();
     operator const fixedlen *() const;
  };


  // IDL
  interface A {
     ...	
  };
  struct varlen {
     short  a;
     long   b;
     string c;
     A      d;
  };

  // C++
  struct varlen {
     CORBA::Short a;
     CORBA::Long  b;
     CORBA::String_Member c;
     A_Member             d;
  };

  class varlen_var {
  public:
     varlen_var();
     varlen_var(varlen *);
     varlen_var(const varlen_var &);
     ~varlen_var();
     varlen_var &operator=(varlen *);
     varlen_var &operator=(const varlen_var &);
     varlen *operator-> const ();

     // conversion operators to support parameter passing
     operator varlen &();
     operator varlen *&();
     operator const varlen *() const;
  };

  */

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

#define ADPT_CLASS_TEMPLATE  "_CORBA_ConstrType_Variable_OUT_arg"

o2be_structure::o2be_structure(UTL_ScopedName *n, UTL_StrList *p)
	    : AST_Decl(AST_Decl::NT_struct, n, p),
	      UTL_Scope(AST_Decl::NT_struct),
	      o2be_name(AST_Decl::NT_struct, n, p),
	      o2be_sequence_chain(AST_Decl::NT_struct, n, p)
{
  pd_isvar = I_FALSE;
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

AST_Field *
o2be_structure::add_field(AST_Field *f)
{
  // Check that the CFE operation succeeds. If it returns 0,
  // stop any further work.
  if (AST_Structure::add_field(f) == 0)
    return 0;

  // Now check if the field is of variable size.
  if (isVariable())
    return f;

  AST_Decl *decl  = f->field_type();
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
  return f;
}


void
o2be_structure::produce_hdr(std::fstream &s)
{
  IND(s); s << "struct " << uqname() << " {\n";
  INC_INDENT_LEVEL();
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();
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

  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();

	    AST_Decl *tdecl = decl;
	    while (tdecl->node_type() == AST_Decl::NT_typedef)
	      tdecl = o2be_typedef::narrow_from_decl(tdecl)->base_type();

	    IND(s);
	    switch (tdecl->node_type())
	      {
	      case AST_Decl::NT_string:
		{
		  if (decl->node_type() == AST_Decl::NT_string)
		    s << o2be_string::fieldMemberTypeName();
		  else
		    s << o2be_typedef::narrow_from_decl(decl)->fieldMemberType_fqname(this);
		  s <<" "<< o2be_field::narrow_from_decl(d)->uqname() << ";\n";
		  break;
		}
	      case AST_Decl::NT_interface:
		{
		  if (decl->node_type() == AST_Decl::NT_interface)
		    s << o2be_interface::narrow_from_decl(decl)->fieldMemberType_fqname(this);
		  else
		    s << o2be_typedef::narrow_from_decl(decl)->fieldMemberType_fqname(this);
		  s <<" "<< o2be_field::narrow_from_decl(d)->uqname() << ";\n";
		  break;
		}
	      case AST_Decl::NT_array:
		{
		  if (decl->node_type() == AST_Decl::NT_array)
		    o2be_array::narrow_from_decl(decl)->produce_struct_member_decl(s,d,this);
		  else {
		    s << o2be_typedef::narrow_from_decl(decl)->unambiguous_name(this);
		    s <<" "<< o2be_field::narrow_from_decl(d)->uqname() << ";\n";
		  }
		  break;
		}
	      case AST_Decl::NT_pre_defined:
		{
		  if (decl->node_type() == AST_Decl::NT_pre_defined)
		    s << o2be_predefined_type::narrow_from_decl(decl)->fieldMemberTypeName();
		  else
		    s << o2be_typedef::narrow_from_decl(decl)->fieldMemberType_fqname(this);
		  s <<" "<< o2be_field::narrow_from_decl(d)->uqname() << ";\n";
		  break;
		}		  
	      case AST_Decl::NT_sequence:
		{
		  if (decl->node_type() == AST_Decl::NT_sequence) {
		    s << o2be_sequence::narrow_from_decl(decl)->seq_template_name(this)
		      << " "
		      << o2be_field::narrow_from_decl(d)->uqname()
		      << ";\n";
		  }
		  else {
		    s << o2be_typedef::narrow_from_decl(decl)->unambiguous_name(this);
		    s <<" "<< o2be_field::narrow_from_decl(d)->uqname() << ";\n";
		  }
		  break;
		}
	      default:
		s << o2be_name::narrow_and_produce_unambiguous_name(decl,this)
		  << " " << o2be_field::narrow_from_decl(d)->uqname() << ";\n";
	      }
	  }
	i.next();
      }
  }

  IND(s); s << "\n";
  IND(s); s << "size_t NP_alignedSize(size_t initialoffset) const;\n";
  IND(s); s << "void operator>>= (NetBufferedStream &) const;\n";
  IND(s); s << "void operator<<= (NetBufferedStream &);\n";
  IND(s); s << "void operator>>= (MemBufferedStream &) const;\n";
  IND(s); s << "void operator<<= (MemBufferedStream &);\n";
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
o2be_structure::produce_skel(std::fstream &s)
{
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();
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

  IND(s); s << "size_t\n";
  IND(s); s << fqname() << "::NP_alignedSize(size_t _initialoffset) const\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::ULong _msgsize = _initialoffset;\n";
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
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
	    o2be_operation::produceSizeCalculation(
                     s,
		     AST_Field::narrow_from_decl(d)->field_type(),
		     ScopeAsDecl(defined_in()),
		     "",
		     "_msgsize",
		     o2be_field::narrow_from_decl(d)->uqname(),
		     ntype,
		     mapping);
	  }
	i.next();
      }
  }
  IND(s); s << "return _msgsize;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "void\n";
  IND(s); s << fqname() << "::operator>>= (NetBufferedStream &_n) const\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
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
	    o2be_operation::produceMarshalCode(
                     s,
		     AST_Field::narrow_from_decl(d)->field_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     o2be_field::narrow_from_decl(d)->uqname(),
		     ntype,
		     mapping);
	  }
	i.next();
      }
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "void\n";
  IND(s); s << fqname() << "::operator<<= (NetBufferedStream &_n)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
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
	    o2be_operation::produceUnMarshalCode(
                     s,
		     AST_Field::narrow_from_decl(d)->field_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     o2be_field::narrow_from_decl(d)->uqname(),
		     ntype,
		     mapping);
	  }
	i.next();
      }
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "void\n";
  IND(s); s << fqname() << "::operator>>= (MemBufferedStream &_n) const\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
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
	    o2be_operation::produceMarshalCode(
                     s,
		     AST_Field::narrow_from_decl(d)->field_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     o2be_field::narrow_from_decl(d)->uqname(),
		     ntype,
		     mapping);
	  }
	i.next();
      }
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "void\n";
  IND(s); s << fqname() << "::operator<<= (MemBufferedStream &_n)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
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
	    o2be_operation::produceUnMarshalCode(
                     s,
		     AST_Field::narrow_from_decl(d)->field_type(),
		     ScopeAsDecl(defined_in()),
		     "_n",
		     o2be_field::narrow_from_decl(d)->uqname(),
		     ntype,
		     mapping,
		     I_TRUE);
	  }
	i.next();
      }
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void
o2be_structure::produce_dynskel(std::fstream &s)
{
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();
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
      IND(s); s << "// MSVC++ does not give the constant external linkage"
		" otherwise.\n";
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
o2be_structure::produce_binary_operators_in_hdr(std::fstream &s)
{
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();
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
    IND(s); s << "extern void operator<<=(CORBA::Any& _a, const "
	      << fqname() << "& _s);\n";
    IND(s); s << "extern void operator<<=(CORBA::Any& _a, "
	      << fqname() <<"* _sp);\n";
    IND(s); s << "extern CORBA::Boolean operator>>=(const CORBA::Any& _a, "
	      << fqname() << "*& _sp);\n\n";
  }
}


void
o2be_structure::produce_binary_operators_in_dynskel(std::fstream &s)
{
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();
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

  // Any storage management function
  // Stupidly, any has to handle the storage for data unmarshalled from it,
  // so it needs some way to delete the data... duh
  IND(s); s << "void _0RL_delete_" << _idname() << "(void* _data) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << fqname() << "* _0RL_t = (" << fqname() << "*) _data;\n";
  IND(s); s << "delete _0RL_t;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  //////////////////////////////////////////////////////////////////////
  //////////////////////// tcDescriptor generation /////////////////////
  //////////////////////////////////////////////////////////////////////

  // Ensure we have buildDesc support for all the members.
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while( !i.is_done() ) {
      AST_Decl *d = i.item();

      if( d->node_type() == AST_Decl::NT_field ) {
	AST_Decl* ft = o2be_field::narrow_from_decl(d)->field_type();
	o2be_buildDesc::produce_decls(s, ft);
      }

      i.next();
    }
  }
  s << "\n";

  // getMemberDesc - generate the tcDescriptor of a member of the structure.
  IND(s); s << "static CORBA::Boolean\n";
  IND(s); s << "_0RL_tcParser_getMemberDesc_" << _idname()
	    << "(tcStructDesc *_desc, CORBA::ULong _index, tcDescriptor "
	    "&_newdesc)";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "switch (_index) {\n";
  {
    unsigned long currentIndex = 0;

    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	if (d->node_type() == AST_Decl::NT_field)
	  {
	    IND(s); s << "case " << currentIndex << ":\n";
	    INC_INDENT_LEVEL();
	    o2be_field* field = o2be_field::narrow_from_decl(d);
	    char* val = new char[1 + strlen(fqname()) +
				strlen(field->uqname()) + 25];
	    strcpy(val, "((");
	    strcat(val, fqname());
	    strcat(val, "*)_desc->opq_struct)->");
	    strcat(val, field->uqname());
	    o2be_buildDesc::call_buildDesc(s, field->field_type(),
					   "_newdesc", val);
	    delete[] val;
	    IND(s); s << "return 1;\n";
	    DEC_INDENT_LEVEL();

	    currentIndex++;
	  }
	i.next();
      }
  }
  IND(s); s << "default:\n";
  INC_INDENT_LEVEL();
  IND(s); s << "return 0;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "};\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  // getMemberCount - return the number of members the structure has.
  IND(s); s << "static CORBA::ULong\n";
  IND(s); s << "_0RL_tcParser_getMemberCount_" << _idname()
	    << "(tcStructDesc *_desc)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "return " << this->nmembers() << ";\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  // tcParser function to build a tcDescriptor for this class.
  IND(s); s << "void _0RL_buildDesc" << canonical_name()
	    << "(tcDescriptor &_desc, const " << fqname()
	    << "& _data)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_desc.p_struct.getMemberDesc = _0RL_tcParser_getMemberDesc_"
	    << _idname() << ";\n";
  IND(s); s << "_desc.p_struct.getMemberCount ="
	    " _0RL_tcParser_getMemberCount_" << _idname() << ";\n";
  IND(s); s << "_desc.p_struct.opq_struct = (void *)&_data;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  //////////////////////////////////////////////////////////////////////
  /////////////////////// Any insertion operator ///////////////////////
  //////////////////////////////////////////////////////////////////////

  IND(s); s << "void operator<<=(CORBA::Any& _a, const " 
	    << fqname() << "& _s) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "tcDescriptor _0RL_tcdesc;\n";
  o2be_buildDesc::call_buildDesc(s, this, "_0RL_tcdesc", "_s");
  IND(s); s << "_a.PR_packFrom(_0RL_tc_" << _idname()
	    << ", &_0RL_tcdesc);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "void operator<<=(CORBA::Any& _a, " << fqname() 
	    << "* _sp) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "tcDescriptor _0RL_tcdesc;\n";
  o2be_buildDesc::call_buildDesc(s, this, "_0RL_tcdesc", "*_sp");
  IND(s); s << "_a.PR_packFrom(_0RL_tc_" << _idname()
	    << ", &_0RL_tcdesc);\n";
  IND(s); s << "delete _sp;\n";
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
  IND(s); s << "if (_a.PR_unpackTo(_0RL_tc_" << _idname()
	    << ", &_0RL_tcdesc)) {\n";
  INC_INDENT_LEVEL();
  // We take the address and cast to get past the
  // const qualifier on <_a>.
  IND(s); s << "((CORBA::Any *)&_a)->PR_setCachedData(_sp, "
	    << "_0RL_delete_" << _idname() << ");\n";
  IND(s); s << "return 1;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "} else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "delete _sp; _sp = 0;\n";
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
o2be_structure::produce_typedef_hdr(std::fstream &s, o2be_typedef *tdef)
{
  IND(s); s << "typedef " << unambiguous_name(tdef)
	    << " " << tdef->uqname() << ";\n";
  IND(s); s << "typedef " << unambiguous_name(tdef)
	    << "_var " << tdef->uqname() << "_var;\n";
}

void
o2be_structure::produce_typecode_skel(std::fstream &s)
{
  if( have_produced_typecode_skel() )  return;
  set_have_produced_typecode_skel();

  if (idl_global->compile_flags() & IDL_CF_ANY) {

    { // Ensure we have the typecodes of the members
      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while( !i.is_done() ) {
	AST_Decl* d = i.item();
	i.next();
	if( d->node_type() != AST_Decl::NT_field )
	  continue;
	d = AST_Field::narrow_from_decl(d)->field_type();
	o2be_name::narrow_and_produce_typecode_skel(d, s);
      }
    }

    // Produce entries in PR_structMember for struct members
    IND(s); s << "static CORBA::PR_structMember _0RL_structmember_"
	      << _idname() << "[] = {\n";

    INC_INDENT_LEVEL();
    { // Produce entries in PR_structMember for struct members

      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while( !i.is_done() ) {
	AST_Decl* d = i.item();
	i.next();
	if( d->node_type() != AST_Decl::NT_field )
	  continue;

	IND(s); s << "{\"" << o2be_field::narrow_from_decl(d)->uqname()
		  << "\", ";
	AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();
	o2be_name::produce_typecode_member(decl, s);
	s << "}";
	if( i.is_done() )  s << '\n';
	else               s << ",\n";
      }
    }
    DEC_INDENT_LEVEL();
    IND(s); s << "};\n";

    IND(s); s << "static CORBA::TypeCode_ptr _0RL_tc_" << _idname() << " = "
	      << "CORBA::TypeCode::PR_struct_tc("
	      << "\"" << repositoryID() << "\", \"" << uqname() << "\", "
	      << "_0RL_structmember_" << _idname() << ", "
	      << this->nmembers() << ");\n\n";
  }
}


void
o2be_structure::produce_decls_at_global_scope_in_hdr(std::fstream& s)
{
  {
    // declare any constructor types defined in this scope
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
        AST_Decl *d = i.item();
        if (d->node_type() == AST_Decl::NT_field)
          {
            AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();
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
}


const char*
o2be_structure::out_adptarg_name(AST_Decl* used_in) const
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


IMPL_NARROW_METHODS1(o2be_structure, AST_Structure)
IMPL_NARROW_FROM_DECL(o2be_structure)
IMPL_NARROW_FROM_SCOPE(o2be_structure)
