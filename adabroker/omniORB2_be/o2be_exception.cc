//                          Package   : omniidl2
// o2be_exception.cc        Created on: 9/8/96
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

/*
  $Log: o2be_exception.cc,v $
  Revision 1.1  1999/02/14 17:45:23  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.12  1999/01/07 09:47:16  djr
  Changes to support new TypeCode/Any implementation, which is now
  placed in a new file ...DynSK.cc (by default).

  Revision 1.11  1998/08/19 15:52:20  sll
  New member functions void produce_binary_operators_in_hdr and the like
  are responsible for generating binary operators <<= etc in the global
  namespace.

  Revision 1.10  1998/08/13 22:37:07  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available.

  Revision 1.9  1998/04/07 18:47:11  sll
  Use std::fstream instead of fstream.
  Stub code modified to accommodate the use of namespace to represent module.

// Revision 1.8  1998/01/27  16:35:52  ewc
//  Added support for type any and TypeCode
//
  Revision 1.7  1997/12/23 19:27:31  sll
  Now generate the correct stub for exception with anonymous array member.

  Revision 1.6  1997/12/09 19:55:20  sll
  *** empty log message ***

  Revision 1.5  1997/08/13 09:23:38  sll
  o2be_exception::repoIdConstLen() now returns the correct length of the
  repository ID. Previously, it wrongly returns the length of the header macro
  name.

// Revision 1.4  1997/05/06  13:54:46  sll
// Public release.
//
  */

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

#define IRREPOID_POSTFIX          "_IntfRepoID"


o2be_exception::o2be_exception(UTL_ScopedName *n, UTL_StrList *p)
  : AST_Decl(AST_Decl::NT_except, n, p),
    AST_Structure(AST_Decl::NT_except, n, p),
    UTL_Scope(AST_Decl::NT_except),
    o2be_name(AST_Decl::NT_except,n,p)
{
  pd_repoid = new char[strlen(_fqname())+strlen(IRREPOID_POSTFIX)+1];
  strcpy(pd_repoid,_fqname());
  strcat(pd_repoid,IRREPOID_POSTFIX);
  pd_repoidsize = strlen(repositoryID())+1;
  pd_have_produced_typecode_skel = I_FALSE;
}


void
o2be_exception::produce_hdr(std::fstream &s)
{
  s << "#define " << repoIdConstName() <<" \""<< repositoryID() << "\"\n\n";
  IND(s); s << "class " << uqname() << " : public CORBA::UserException {\n";
  IND(s); s << "public:\n\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
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
	  case AST_Decl::NT_pre_defined:
	    {
	      if (AST_PredefinedType::narrow_from_decl(tdecl)->pt()
		  == AST_PredefinedType::PT_TypeCode)
		{
		  s << o2be_predefined_type::TypeCodeMemberName();
		}
	      else
		{
		  s << o2be_name::narrow_and_produce_unambiguous_name(decl,this);
		}		    
	      s << " " << o2be_field::narrow_from_decl(d)->uqname() << ";\n";
	      break;
	    }
	  case AST_Decl::NT_array:
	    {
	      if (decl->node_type() == AST_Decl::NT_array) {
		// Check if this is an anonymous array type, if so
		// generate the supporting typedef.
		if (decl->has_ancestor(this)) {
		  char* fname = o2be_field::narrow_from_decl(d)->uqname();
		  char * tmpname = new char [strlen(fname) + 2];
		  strcpy(tmpname,"_");
		  strcat(tmpname,fname);
		  o2be_array::narrow_from_decl(decl)->produce_typedef_in_union(s,tmpname,this);
		}
		o2be_array::narrow_from_decl(decl)->produce_struct_member_decl(s,d,this);
	      }
	      else {
		s << o2be_typedef::narrow_from_decl(decl)->unambiguous_name(this);
		s <<" "<< o2be_field::narrow_from_decl(d)->uqname() << ";\n";
	      }
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

	i.next();
      }
  }

  IND(s); s << "\n";
  IND(s); s << uqname() << "() {};\n";
  IND(s); s << uqname() << "(const " << uqname() << " &);\n";
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    idl_bool first = I_TRUE;
    while (!i.is_done())
      {
	if (first) {
	  IND(s); s << uqname() << "(";
	  first = I_FALSE;
	}
	AST_Decl *d = i.item();
	AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();
	o2be_operation::argMapping mapping;
	o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     decl,						   
		     o2be_operation::wIN,mapping);

	s << ((mapping.is_const) ? "const ":"");

	switch (ntype) {
	case o2be_operation::tObjref:
	  s << o2be_interface::narrow_from_decl(decl)->unambiguous_objref_name(this);
	  break;
	case o2be_operation::tString:
	  s << "char* ";
	  break;
	case o2be_operation::tTypeCode:
	  s << "CORBA::TypeCode_ptr ";
	  break;
	case o2be_operation::tArrayFixed:
	case o2be_operation::tArrayVariable:
	  // Check if this is an anonymous array type, if so
	  // use the supporting typedef for the array
	  if (decl->node_type() == AST_Decl::NT_array &&
	      decl->has_ancestor(this))
	    {
	      s << "_0RL_" << o2be_field::narrow_from_decl(d)->uqname();
	    }
	  else
	    {
	      s << o2be_name::narrow_and_produce_unambiguous_name(decl,this);
	    }
	  break;
	case o2be_operation::tSequence:
	  if (decl->node_type() == AST_Decl::NT_sequence) {
	    s << o2be_sequence::narrow_from_decl(decl)->seq_template_name(this);
	  }
	  else {
	    s << o2be_name::narrow_and_produce_unambiguous_name(decl,this);
	  }
	  break;
	default:
	  s << o2be_name::narrow_and_produce_unambiguous_name(decl,this)
	    << ((mapping.is_arrayslice) ? "_slice":"")
	    << " "
	    << ((mapping.is_pointer)    ? "*":"")
	    << ((mapping.is_reference)  ? "&":"");
	  break;
	}
	s << " _" << o2be_field::narrow_from_decl(d)->uqname();
	i.next();
	s << ((!i.is_done()) ? ", " : "");
      }
    if (!first) {
      s << ");\n";
    };
  }
  IND(s); s << uqname() << " & operator=(const " << uqname() << " &);\n";
  IND(s); s << "virtual ~" << uqname() << "() {};\n";
  IND(s); s << "size_t NP_alignedSize(size_t initialoffset) const;\n";
  IND(s); s << "void operator>>= (NetBufferedStream &) const;\n";
  IND(s); s << "void operator<<= (NetBufferedStream &);\n";
  IND(s); s << "void operator>>= (MemBufferedStream &) const;\n";
  IND(s); s << "void operator<<= (MemBufferedStream &);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "};\n\n";

  if (idl_global->compile_flags() & IDL_CF_ANY) {
    // TypeCode_ptr declaration
    IND(s); s << variable_qualifier()
	      << " const CORBA::TypeCode_ptr " << tcname() << ";\n\n";
  }
}


void
o2be_exception::produce_skel(std::fstream &s)
{
  IND(s); s << fqname() << "::" << uqname()
	    << "(const " << fqname() << " &_s)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	o2be_field *d = o2be_field::narrow_from_decl(i.item());
	o2be_operation::argMapping mapping;
	o2be_operation::argType ntype;
	ntype =  o2be_operation::ast2ArgMapping(d->field_type(),
						o2be_operation::wIN,mapping);
	switch (ntype) 
	  {
	  case o2be_operation::tArrayFixed:
	  case o2be_operation::tArrayVariable:
	    {
	      IND(s); s << "{\n";
	      INC_INDENT_LEVEL();
	      AST_Decl* decl = d->field_type();
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
	      IND(s); s << d->uqname();
	      ndim = 0;
	      while (ndim < o2be_array::narrow_from_decl(decl)->getNumOfDims())
		{
		  s << "[_i" << ndim << "]";
		  ndim++;
		}
	      s << " = _s." << d->uqname();
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
	      DEC_INDENT_LEVEL();
	      IND(s); s << "}\n";
	      break;
	    }
	  
	  default:
	    IND(s); s << d->uqname() << " = _s." << d->uqname() << ";\n";
	    break;
	  }
	i.next();
      }
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    idl_bool first = I_TRUE;
    while (!i.is_done())
      {
	if (first) {
	  IND(s); s << fqname() << "::" << uqname() << "(";
	  first = I_FALSE;
	}
	AST_Decl *d = i.item();
	AST_Decl *decl = AST_Field::narrow_from_decl(d)->field_type();
	o2be_operation::argMapping mapping;
	o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     decl,						   
		     o2be_operation::wIN,mapping);

	s << ((mapping.is_const) ? "const ":"");
	switch (ntype) {
	case o2be_operation::tObjref:
	  s << o2be_interface::narrow_from_decl(decl)
	    ->unambiguous_objref_name(this);
	  break;
	case o2be_operation::tString:
	  s << "char* ";
	  break;
	case o2be_operation::tTypeCode:
	  s << "CORBA::TypeCode_ptr ";
	  break;
	case o2be_operation::tArrayFixed:
	case o2be_operation::tArrayVariable:
	  // Check if this is an anonymous array type, if so
	  // use the supporting typedef for the array
	  if (decl->node_type() == AST_Decl::NT_array &&
	      decl->has_ancestor(this))
	    {
	      s << "_0RL_" << o2be_field::narrow_from_decl(d)->uqname();
	    }
	  else
	    {
	      s << o2be_name::narrow_and_produce_unambiguous_name(decl,this);
	    }
	  break;
	case o2be_operation::tSequence:
	  if (decl->node_type() == AST_Decl::NT_sequence) {
	    s << o2be_sequence::narrow_from_decl(decl)->seq_template_name(this);
	  }
	  else {
	    s << o2be_name::narrow_and_produce_unambiguous_name(decl,this);
	  }
	  break;
	default:
	  s << o2be_name::narrow_and_produce_unambiguous_name(decl,this)
	    << ((mapping.is_arrayslice) ? "_slice":"")
	    << " "
	    << ((mapping.is_pointer)    ? "*":"")
	    << ((mapping.is_reference)  ? "&":"");
	  break;
	}
	s << " _" << o2be_field::narrow_from_decl(d)->uqname();
	i.next();
	s << ((!i.is_done()) ? ", " : "");
      }
    if (!first) {
      s << ")\n";
      IND(s); s << "{\n";
      INC_INDENT_LEVEL();
      {
	UTL_ScopeActiveIterator ii(this,UTL_Scope::IK_decls);
	while (!ii.is_done())
	  {
	    o2be_field *dd = o2be_field::narrow_from_decl(ii.item());
	    o2be_operation::argMapping mapping;
	    o2be_operation::argType ntype;
	    ntype =  o2be_operation::ast2ArgMapping(dd->field_type(),
						    o2be_operation::wIN,mapping);
	    switch (ntype) 
	      {
	      case o2be_operation::tArrayFixed:
	      case o2be_operation::tArrayVariable:
		{
		  IND(s); s << "{\n";
		  INC_INDENT_LEVEL();
		  AST_Decl* decl = dd->field_type();
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
		  IND(s); s << dd->uqname();
		  ndim = 0;
		  while (ndim < o2be_array::narrow_from_decl(decl)->getNumOfDims())
		    {
		      s << "[_i" << ndim << "]";
		      ndim++;
		    }
		  s << " = _" << dd->uqname();
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
		  DEC_INDENT_LEVEL();
		  IND(s); s << "}\n";
		  break;
		}
	  
	      default:
		IND(s); s << dd->uqname() << " = _" << dd->uqname() << ";\n";
		break;
	      }
	    ii.next();
	  }
      }
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n\n";
    }
  }

  IND(s); s << fqname() << " & " << fqname() 
	    << "::operator=(const " << fqname() << " &_s)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	o2be_field *d = o2be_field::narrow_from_decl(i.item());
	o2be_operation::argMapping mapping;
	o2be_operation::argType ntype;
	ntype =  o2be_operation::ast2ArgMapping(d->field_type(),
						o2be_operation::wIN,mapping);
	switch (ntype) 
	  {
	  case o2be_operation::tArrayFixed:
	  case o2be_operation::tArrayVariable:
	    {
	      IND(s); s << "{\n";
	      INC_INDENT_LEVEL();
	      AST_Decl* decl = d->field_type();
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
	      IND(s); s << d->uqname();
	      ndim = 0;
	      while (ndim < o2be_array::narrow_from_decl(decl)->getNumOfDims())
		{
		  s << "[_i" << ndim << "]";
		  ndim++;
		}
	      s << " = _s." << d->uqname();
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
	      DEC_INDENT_LEVEL();
	      IND(s); s << "}\n";
	      break;
	    }
	  
	  default:
	    IND(s); s << d->uqname() << " = _s." << d->uqname() << ";\n";
	    break;
	  }
	i.next();
      }
  }
  IND(s); s << "return *this;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "size_t\n";
  IND(s); s << fqname() << "::NP_alignedSize(size_t _initialoffset) const\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "size_t _msgsize = _initialoffset;\n";
  {
    UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
    while (!i.is_done())
      {
	AST_Decl *d = i.item();
	o2be_operation::argMapping mapping;
	o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
		     o2be_operation::wIN,mapping);

	if (ntype == o2be_operation::tTypeCode) {
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
	o2be_operation::argMapping mapping;
	o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
		     o2be_operation::wIN,mapping);

	if (ntype == o2be_operation::tTypeCode) {
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
	o2be_operation::argMapping mapping;
	o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
		     o2be_operation::wIN,mapping);

	if (ntype == o2be_operation::tTypeCode) {
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
	o2be_operation::argMapping mapping;
	o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
		     o2be_operation::wIN,mapping);

	if (ntype == o2be_operation::tTypeCode) {
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
	o2be_operation::argMapping mapping;
	o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(
		     AST_Field::narrow_from_decl(d)->field_type(),
		     o2be_operation::wIN,mapping);

	if (ntype == o2be_operation::tTypeCode) {
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
	i.next();
      }
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void
o2be_exception::produce_dynskel(std::fstream &s)
{
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
o2be_exception::produce_binary_operators_in_hdr(std::fstream &s)
{
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
o2be_exception::produce_binary_operators_in_dynskel(std::fstream &s)
{
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

  // any insertion/extraction helper functions
  IND(s); s << "CORBA::Boolean _0RL_tcParser_getMemberDesc_" << _idname()
	    << "(tcStructDesc *_desc, CORBA::ULong _index, "
	    "tcDescriptor &_newdesc)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "switch( _index ) {\n";
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

  IND(s); s << "CORBA::ULong\n";
  IND(s); s << "_0RL_tcParser_getMemberCount_" << _idname()
	    << "(tcStructDesc *_desc)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "return " << this->nmembers() << ";\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  // tcParser function to build a tcDescriptor for this class
  IND(s); s << "void _0RL_buildDesc" << canonical_name()
	    << "(tcDescriptor &_desc, const " << fqname()
	    << "& _data)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_desc.p_struct.getMemberDesc = "
	    "_0RL_tcParser_getMemberDesc_" << _idname() << ";\n";
  IND(s); s << "_desc.p_struct.getMemberCount = "
	    "_0RL_tcParser_getMemberCount_" << _idname() << ";\n";
  IND(s); s << "_desc.p_struct.opq_struct = (void *)&_data;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  IND(s); s << "void _0RL_delete_" << _idname() << "(void* _data) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << fqname() << "*" << " _0RL_t = (" << fqname() << "*) _data;\n";
  IND(s); s << "delete _0RL_t;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  //////////////////////////////////////////////////////////////////////
  /////////////////////// Any insertion operators //////////////////////
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
  IND(s); s << "((CORBA::Any *)&_a)->PR_setCachedData(_sp, "
	    << "_0RL_delete_" << _idname() << ");\n";
  IND(s); s << "return 1;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "} else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "delete _sp;_sp = 0;\n";
  IND(s); s << "return 0;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "} else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::TypeCode_var _0RL_tctmp = _a.type();\n";
  IND(s); s << "if (_0RL_tctmp->equal(_0RL_tc_" << _idname()
	    << ")) return 1;\n";
  IND(s); s << "delete _sp; _sp = 0;\n";
  IND(s); s << "return 0;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}

void
o2be_exception::produce_typecode_skel(std::fstream &s)
{
  if( have_produced_typecode_skel() )  return;
  set_have_produced_typecode_skel();

  unsigned long memberCount = this->nmembers();

  if (memberCount > 0) {

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

    IND(s); s << "static CORBA::PR_structMember _0RL_structmember_"
	      << _idname() << "[] = {\n";

    INC_INDENT_LEVEL();
    { // Produce entries in PR_structMember for struct members

      UTL_ScopeActiveIterator i(this,UTL_Scope::IK_decls);
      while( !i.is_done() ) {
	AST_Decl *d = i.item();
	i.next();
	if( d->node_type() != AST_Decl::NT_field )
	  continue;

	IND(s); s << "{\"" << o2be_field::narrow_from_decl(d)->uqname()
		  << "\", ";
	AST_Decl* decl = AST_Field::narrow_from_decl(d)->field_type();
	o2be_name::produce_typecode_member(decl,s);
	s << "}";
	if( i.is_done() )  s << '\n';
	else               s << ",\n";
      }
    }
    DEC_INDENT_LEVEL();
    IND(s); s << "};\n";
  }

  IND(s); s << "static CORBA::TypeCode_ptr _0RL_tc_" << _idname() << " = " 
	    << "CORBA::TypeCode::PR_exception_tc("
	    << "\"" << repositoryID() << "\", \"" << uqname() << "\", ";
  if (memberCount > 0) s << "_0RL_structmember_" << _idname() << ", ";
  else s << "(CORBA::PR_structMember*) 0, ";
  s << memberCount << ");\n\n";
}


IMPL_NARROW_METHODS1(o2be_exception, AST_Exception)
IMPL_NARROW_FROM_DECL(o2be_exception)
IMPL_NARROW_FROM_SCOPE(o2be_exception)
