// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_enum.cc             Created on: 12/08/1996
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
//   OMNI BE for the class AST_Enum
//

/*
  $Log: o2be_enum.cc,v $
  Revision 1.1  1999/02/14 17:45:23  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.10  1999/01/07 09:47:52  djr
  Changes to support new TypeCode/Any implementation, which is now
  placed in a new file ...DynSK.cc (by default).
   Renamed local variables beginning with __.

  Revision 1.9  1998/08/19 15:51:37  sll
  New member functions void produce_binary_operators_in_hdr and the like
  are responsible for generating binary operators <<= etc in the global
  namespace.

  Revision 1.8  1998/08/13 22:42:06  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available.

  Revision 1.7  1998/04/07 18:45:26  sll
  Use std::fstream instead of fstream.
  Stub code contains workaround code for MSVC++ to initialise typecode const
  properly.
  Stub code modified to accomodate namespace support.

// Revision 1.6  1998/01/27  16:35:19  ewc
//  Added support for type any and TypeCode
//
  Revision 1.5  1997/12/09 19:55:06  sll
  *** empty log message ***

// Revision 1.4  1997/05/06  13:53:47  sll
// Public release.
//
  */

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

o2be_enum::o2be_enum(UTL_ScopedName *n, UTL_StrList *p)
       : AST_Enum(n, p),
	 AST_Decl(AST_Decl::NT_enum, n, p),
	 UTL_Scope(AST_Decl::NT_enum),
	 o2be_name(AST_Decl::NT_enum,n,p),
	 o2be_sequence_chain(AST_Decl::NT_enum,n,p)
{
  pd_hdr_produced_in_field = I_FALSE;
  pd_skel_produced_in_field = I_FALSE;
  pd_binary_operators_hdr_produced_in_field = I_FALSE;
  pd_binary_operators_skel_produced_in_field = I_FALSE;
  pd_have_produced_typecode_skel = I_FALSE;
}


void
o2be_enum::produce_hdr(std::fstream &s)
{
  IND(s); s << "enum " << uqname() << " { ";
  {
    UTL_ScopeActiveIterator i(this, IK_decls);
    while (!(i.is_done())) {
      s << o2be_name::narrow_and_produce_uqname(i.item());
      i.next();
      if (!(i.is_done()))
	s << ", ";
    }
  }
  s << " };\n\n";

  if (idl_global->compile_flags() & IDL_CF_ANY) {
    // TypeCode_ptr declaration
    IND(s); s << variable_qualifier()
	      << " const CORBA::TypeCode_ptr " << tcname() << ";\n\n";
  }

  produce_seq_hdr_if_defined(s);
}


void
o2be_enum::produce_skel(std::fstream &s)
{
}


void
o2be_enum::produce_dynskel(std::fstream &s)
{
  // Produce code for types any and TypeCode
  this->produce_typecode_skel(s);

  if (defined_in() != idl_global->root() &&
      defined_in()->scope_node_type() == AST_Decl::NT_module)
    {
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
		<< "_0RL_tc_" << _idname() << ";\n\n";
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      s << "#else\n";
      IND(s); s << "const CORBA::TypeCode_ptr " << fqtcname() << " = "
		<< "_0RL_tc_" << _idname() << ";\n\n";
      s << "#endif\n";
    }
  else
    {
      IND(s); s << "const CORBA::TypeCode_ptr " << fqtcname() << " = "
		<< "_0RL_tc_" << _idname() << ";\n\n";
    }
}


void
o2be_enum::produce_binary_operators_in_hdr(std::fstream &s)
{
  IND(s); s << "inline void operator >>=(" << fqname()
	    << " _e, NetBufferedStream& s) {\n";

  INC_INDENT_LEVEL();
  IND(s); s << "::operator>>=((CORBA::ULong)_e, s);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "inline void operator <<= (" << fqname()
	    << "& _e, NetBufferedStream& s) {\n";

  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::ULong _0RL_e;\n";
  IND(s); s << "::operator<<=(_0RL_e,s);\n";
  IND(s); s << "switch (_0RL_e) {\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this, IK_decls);
    while (!(i.is_done())) {
      IND(s) s << "case " << o2be_name::narrow_and_produce_fqname(i.item()) << ":\n";
      i.next();
    }
  }
  INC_INDENT_LEVEL();
  IND(s); s << "_e = (" << fqname() << ") _0RL_e;\n";
  IND(s); s << "break;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "default:\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_CORBA_marshal_error();\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "inline void operator>>= (" << fqname()
	    << " _e, MemBufferedStream& s) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "::operator>>=((CORBA::ULong)_e,s);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "inline void operator<<= (" << fqname()
	    << "& _e,MemBufferedStream& s) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::ULong _0RL_e;\n";
  IND(s); s << "::operator<<=(_0RL_e,s);\n";
  IND(s); s << "switch (_0RL_e) {\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this, IK_decls);
    while (!(i.is_done())) {
      IND(s) s << "case " << o2be_name::narrow_and_produce_fqname(i.item()) << ":\n";
      i.next();
    }
  }
  INC_INDENT_LEVEL();
  IND(s); s << "_e = (" << fqname() << ") _0RL_e;\n";
  IND(s); s << "break;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "default:\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_CORBA_marshal_error();\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  if (idl_global->compile_flags() & IDL_CF_ANY) {
    // any insertion and extraction operator
    IND(s); s << "void operator<<=(CORBA::Any& _a, " 
	      << fqname() << " _s);\n";
    IND(s); s << "CORBA::Boolean operator>>=(const CORBA::Any& _a, " 
	      << fqname() << "& _s);\n\n";
  }

}


void
o2be_enum::produce_binary_operators_in_dynskel(std::fstream &s)
{
  //////////////////////////////////////////////////////////////////////
  //////////////////////// tcDescriptor generation /////////////////////
  //////////////////////////////////////////////////////////////////////

  IND(s); s << "void _0RL_buildDesc" << canonical_name()
	    << "(tcDescriptor& _desc, const " << fqname() << "& _data)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_desc.p_enum = (CORBA::ULong*)&_data;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  //////////////////////////////////////////////////////////////////////
  /////////////////////// Any insertion operator ///////////////////////
  //////////////////////////////////////////////////////////////////////

  IND(s); s << "void operator<<=(CORBA::Any& _a, " << fqname() << " _s)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "tcDescriptor _0RL_tcd;\n";
  o2be_buildDesc::call_buildDesc(s, this, "_0RL_tcd", "_s");
  IND(s); s << "_a.PR_packFrom(_0RL_tc_" << _idname() << ", &_0RL_tcd);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  //////////////////////////////////////////////////////////////////////
  //////////////////////// Any extraction operator /////////////////////
  //////////////////////////////////////////////////////////////////////

  IND(s); s << "CORBA::Boolean operator>>=(const CORBA::Any& _a, "
	    << fqname() << "& _s)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "tcDescriptor _0RL_tcd;\n";
  o2be_buildDesc::call_buildDesc(s, this, "_0RL_tcd", "_s");
  IND(s); s << "return _a.PR_unpackTo(_0RL_tc_" << _idname()
	    << ", &_0RL_tcd);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void
o2be_enum::produce_typedef_hdr(std::fstream &s, o2be_typedef *tdef)
{
  IND(s); s << "typedef " << unambiguous_name(tdef) 
	    << " " << tdef->uqname() << ";\n";
}


void
o2be_enum::produce_typecode_skel(std::fstream &s)
{
  if( have_produced_typecode_skel() )  return;
  set_have_produced_typecode_skel();

  IND(s); s << "static const char* _0RL_enumMember_" << _idname()
	    << "[] = { ";
  unsigned int valueCount = 0;
  {
    UTL_ScopeActiveIterator i(this, IK_decls);
    while (!(i.is_done())) {
      s << "\"" << o2be_name::narrow_and_produce_uqname(i.item()) << "\"";
      valueCount++;
      i.next();
      if( !i.is_done() )  s << ", ";
    }
  }
  s << "};\n";

  IND(s); s << "static CORBA::TypeCode_ptr _0RL_tc_" << _idname()
	    << " = CORBA::TypeCode::PR_enum_tc(\""
	    << repositoryID() << "\", \"" << uqname() << "\", "
	    << "_0RL_enumMember_" << _idname() << ", " << valueCount
	    << ");\n";
}


// Narrowing
IMPL_NARROW_METHODS1(o2be_enum, AST_Enum)
IMPL_NARROW_FROM_DECL(o2be_enum)
IMPL_NARROW_FROM_SCOPE(o2be_enum)


o2be_enum_val::o2be_enum_val(unsigned long v, 
			     UTL_ScopedName *n, 
			     UTL_StrList *p)
  : AST_Decl(AST_Decl::NT_enum_val, n, p),
    AST_Constant(AST_Expression::EV_ulong,
		 AST_Decl::NT_enum_val,
		 new AST_Expression(v),
		 n,
		 p),
    AST_EnumVal(v,n,p),
    o2be_name(AST_Decl::NT_enum_val,n,p)
{
  o2be_enum* enum_decl = o2be_enum::narrow_from_decl(ScopeAsDecl(defined_in()));
  set_scopename(enum_decl->scopename());
  set__scopename(enum_decl->_scopename());
  char* buf = new char [strlen(scopename())+strlen(uqname())+1];
  strcpy(buf,scopename());
  strcat(buf,uqname());
  set_fqname(buf);
  buf = new char [strlen(_scopename())+strlen(uqname())+1];
  strcpy(buf,_scopename());
  strcat(buf,uqname());
  set__fqname(buf);

  set_tcname("");
  set_fqtcname("");
  set__fqtcname("");
}



IMPL_NARROW_METHODS1(o2be_enum_val, AST_EnumVal)
IMPL_NARROW_FROM_DECL(o2be_enum_val)

