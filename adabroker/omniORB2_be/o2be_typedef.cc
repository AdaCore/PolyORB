// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_typedef.cc          Created on: 07/10/1996
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
  $Log: o2be_typedef.cc,v $
  Revision 1.1  1999/02/14 17:45:29  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.11  1999/01/07 09:35:57  djr
  Changes to support new TypeCode/Any implementation, which is now
  placed in a new file ...DynSK.cc (by default).

  Revision 1.10  1998/08/19 15:54:30  sll
  New member functions void produce_binary_operators_in_hdr and the like
  are responsible for generating binary operators <<= etc in the global
  namespace.

  Revision 1.9  1998/08/13 22:46:47  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available.

  Revision 1.8  1998/04/07 18:53:00  sll
  Stub code modified to accommodate the use of namespace to represent module.
  Use std::fstream instead of fstream.

// Revision 1.7  1998/01/27  16:50:17  ewc
//  Added support for type Any and TypeCode
//
  Revision 1.6  1997/12/23 19:29:37  sll
  Now generate the implementation of array helper functions defined in
  the global scope in the skeleton file. This applies to typedef of array
  as well.

  Revision 1.5  1997/12/09 19:55:01  sll
  *** empty log message ***

  Revision 1.4  1997/08/27 17:55:07  sll
  If the true type of an interface is Object, use CORBA::Object_member as
  the field member type name.

// Revision 1.3  1997/05/06  14:09:04  sll
// Public release.
//
  */


#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

#define OBJREF_MEMBER_TEMPLATE_NAME "_CORBA_ObjRef_Member"
#define STRING_MEMBER_NAME          "CORBA::String_member"
#define TYPECODE_MEMBER_NAME        "CORBA::TypeCode_member"
#define SEQUENCE_VAR_TEMPLATE	    "_CORBA_ConstrType_Variable_Var"

o2be_typedef::o2be_typedef(AST_Type *bt, UTL_ScopedName *n, UTL_StrList *p)
	  : AST_Typedef(bt, n, p),
	    AST_Decl(AST_Decl::NT_typedef, n, p),
	    o2be_name(AST_Decl::NT_typedef, n, p),
	    o2be_sequence_chain(AST_Decl::NT_typedef, n, p)
{
  pd_have_produced_typecode_skel = I_FALSE;

  AST_Decl *decl = base_type();

  while (decl->node_type() == AST_Decl::NT_typedef) {
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();
  }

  switch (decl->node_type())
    {
    case AST_Decl::NT_interface:
      if (strcmp(o2be_name::narrow_and_produce_uqname(decl),"Object") == 0) {
	pd_fm_uqname = (char *)o2be_interface::narrow_from_decl(decl)->
	                  fieldMemberType_uqname();
      }
      else {
	pd_fm_uqname = new char[strlen(OBJREF_MEMBER_TEMPLATE_NAME)+
			       strlen(uqname()) +
			       strlen(uqname()) + strlen("_Helper") + 4];
	strcpy(pd_fm_uqname,OBJREF_MEMBER_TEMPLATE_NAME);
	strcat(pd_fm_uqname,"<");
	strcat(pd_fm_uqname,uqname());
	strcat(pd_fm_uqname,",");
	strcat(pd_fm_uqname,uqname());
	strcat(pd_fm_uqname,"_Helper");
	strcat(pd_fm_uqname,">");
      }
      break;
    case AST_Decl::NT_string:
      pd_fm_uqname = new char[strlen(STRING_MEMBER_NAME)+1];
      strcpy(pd_fm_uqname,STRING_MEMBER_NAME);
      break;
    default:
      pd_fm_uqname = uqname();
      break;
    }
}


void
o2be_typedef::produce_hdr(std::fstream &s)
{
  AST_Decl *decl = base_type();
  const char *tname = o2be_name::narrow_and_produce_fqname(decl);

  while (decl->node_type() == AST_Decl::NT_typedef) {
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();
  }

  if (idl_global->compile_flags() & IDL_CF_ANY) {
    // TypeCode_ptr declaration
    IND(s); s << variable_qualifier()
	      << " const CORBA::TypeCode_ptr " << tcname() << ";\n";
  }

  switch (decl->node_type())
    {
    case AST_Decl::NT_pre_defined:
      o2be_predefined_type::narrow_from_decl(decl)
	->produce_typedef_hdr(s, this);
      break;
    case AST_Decl::NT_enum:
      o2be_enum::narrow_from_decl(decl)->produce_typedef_hdr(s, this);
      break;
    case AST_Decl::NT_interface:
      o2be_interface::narrow_from_decl(decl)->produce_typedef_hdr(s, this);
      break;
    case AST_Decl::NT_union:
      o2be_union::narrow_from_decl(decl)->produce_typedef_hdr(s, this);
      break;
    case AST_Decl::NT_struct:
      o2be_structure::narrow_from_decl(decl)->produce_typedef_hdr(s, this);
      break;
    case AST_Decl::NT_string:
      o2be_string::produce_typedef_hdr(s, this);
      break;
    case AST_Decl::NT_sequence:
      o2be_sequence::narrow_from_decl(decl)->produce_typedef_hdr(s, this);
      break;
    case AST_Decl::NT_array:
      if (base_type()->node_type() == AST_Decl::NT_array)
	o2be_array::narrow_from_decl(decl)->produce_hdr(s, this);
      else
	o2be_array::produce_typedef_hdr(s, this,
				o2be_typedef::narrow_from_decl(base_type()));
      break;
    default:
      throw o2be_internal_error(__FILE__,__LINE__,"Unexpected argument type");
    }
}


void
o2be_typedef::produce_skel(std::fstream &s)
{
  AST_Decl *decl = base_type();

  while (decl->node_type() == AST_Decl::NT_typedef) {
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();
  }

  switch (decl->node_type())
    {
    case AST_Decl::NT_array:
      if (base_type()->node_type() == AST_Decl::NT_array)
	o2be_array::narrow_from_decl(decl)->produce_skel(s,this);
      else
	o2be_array::produce_typedef_skel(s,this,
			    o2be_typedef::narrow_from_decl(base_type()));
      break;
    default:
      break;
    };
}


void
o2be_typedef::produce_dynskel(std::fstream &s)
{
  AST_Decl *decl = base_type();

  while (decl->node_type() == AST_Decl::NT_typedef) {
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();
  }

  // Produce code for types any and TypeCode
  this->produce_typecode_skel(s);

  if( defined_in() != idl_global->root() &&
      defined_in()->scope_node_type() == AST_Decl::NT_module) {

    s << "\n#if defined(HAS_Cplusplus_Namespace) && defined(_MSC_VER)\n";
    IND(s); s << "// MSVC++ does not give the constant external"
	      " linkage othewise.\n";
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
  else {
    IND(s); s << "const CORBA::TypeCode_ptr " << fqtcname() << " = " 
	      << "_0RL_tc_" << _idname() << ";\n\n";
  }
}


void
o2be_typedef::produce_binary_operators_in_hdr(std::fstream &s)
{
  if (idl_global->compile_flags() & IDL_CF_ANY) {
    AST_Decl *decl = base_type();

    switch (decl->node_type())
      {
      case AST_Decl::NT_sequence:
	o2be_sequence::narrow_from_decl(decl)
	  ->produce_typedef_binary_operators_in_hdr(s, this);
	break;
      case AST_Decl::NT_array:
	o2be_array::narrow_from_decl(decl)
	  ->produce_binary_operators_in_hdr(s, this);
	break;
      default:
	break;
      }
  }
}


void
o2be_typedef::produce_binary_operators_in_dynskel(std::fstream &s)
{
  AST_Decl *decl = base_type();

  switch (decl->node_type())
    {
    case AST_Decl::NT_sequence:
      o2be_sequence::narrow_from_decl(decl)
	->produce_typedef_binary_operators_in_dynskel(s, this);
      break;
    case AST_Decl::NT_array:
      o2be_array::narrow_from_decl(decl)
	->produce_binary_operators_in_dynskel(s, this);
      break;
    default:
      break;
    }
}


void
o2be_typedef::produce_typecode_skel(std::fstream &s)
{
  if( have_produced_typecode_skel() )  return;
  set_have_produced_typecode_skel();

  AST_Decl* base = base_type();

  // Generate typecode for the base_type().
  o2be_name::narrow_and_produce_typecode_skel(base, s);

  IND(s); s << "static CORBA::TypeCode_ptr _0RL_tc_" << _idname() << " = "
	    << "CORBA::TypeCode::PR_alias_tc("
	    << "\""<< repositoryID() << "\", \"" << uqname() 
	    << "\", ";
  o2be_name::produce_typecode_member(base, s);
  s << ");\n\n";
}

const char*
o2be_typedef::fieldMemberType_fqname(AST_Decl* used_in)
{
  const char* ubname;
  char* result;

  if (o2be_global::qflag()) {
    ubname = fqname();
  }
  else {
    ubname = unambiguous_name(used_in);
  }

  AST_Decl *decl = base_type();
  while (decl->node_type() == AST_Decl::NT_typedef) {
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();
  }
  switch (decl->node_type())
    {
    case AST_Decl::NT_interface:
      if (strcmp(o2be_name::narrow_and_produce_uqname(decl),"Object") == 0) {
	result = (char *)o2be_interface::narrow_from_decl(decl)->
	                             fieldMemberType_fqname(used_in);
      }
      else {
	result = new char[strlen(OBJREF_MEMBER_TEMPLATE_NAME)+
			       strlen(ubname) +
			       strlen(ubname) + strlen("_Helper") + 4];
	strcpy(result,OBJREF_MEMBER_TEMPLATE_NAME);
	strcat(result,"<");
	strcat(result,ubname);
	strcat(result,",");
	strcat(result,ubname);
	strcat(result,"_Helper");
	strcat(result,">");
      }
      break;
    case AST_Decl::NT_string:
      result = new char[strlen(STRING_MEMBER_NAME)+1];
      strcpy(result,STRING_MEMBER_NAME);
      break;
    case AST_Decl::NT_pre_defined:
      if(o2be_predefined_type::narrow_from_decl(decl)->pt() == 
	 AST_PredefinedType::PT_TypeCode)
	{
	  result = new char[strlen(TYPECODE_MEMBER_NAME)+1];
	  strcpy(result,TYPECODE_MEMBER_NAME);
	  break;
	}      
    default:
      result = (char*) ubname;
      break;
    }
  return result;
}

IMPL_NARROW_METHODS1(o2be_typedef, AST_Typedef)
IMPL_NARROW_FROM_DECL(o2be_typedef)
