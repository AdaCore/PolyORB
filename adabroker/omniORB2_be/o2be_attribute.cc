// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_attribute.cc        Created on: 13/09/1996
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
  $Log: o2be_attribute.cc,v $
  Revision 1.1  1999/02/14 17:45:22  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.19  1999/01/07 09:51:11  djr
  New implementation of proxy calls which reduces the size of
  the code generated.

  Revision 1.18  1998/08/13 22:35:48  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available

  Revision 1.17  1998/04/07 18:40:29  sll
  Use std::fstream instead of fstream.
  Stub now use omniORB::log to log error messages.

  Revision 1.16  1998/03/25 14:39:12  sll
  *** empty log message ***

  Revision 1.15  1998/03/25 14:19:50  sll
  Temporary work-around for egcs compiler.

  Revision 1.14  1998/01/27 16:34:29  ewc
   Added support for type any and TypeCode

  Revision 1.13  1997/12/10 11:35:10  sll
  Updated life cycle service stub.

  Revision 1.12  1997/12/09 19:54:19  sll
  *** empty log message ***

// Revision 1.11  1997/09/20  16:35:25  dpg1
// New code generation for LifeCycle support.
//
  Revision 1.10  1997/08/22 12:43:23  sll
  Oh well, gcc does not like variable names starting with __, changed
  the prefix to _0RL_.

  Revision 1.9  1997/08/21 21:20:58  sll
  Names of internal variables inside the stub code now all start with the
  prefix __ to avoid potential clash with identifiers defined in IDL.

// Revision 1.8  1997/05/06  13:50:29  sll
// Public release.
//
  */

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

o2be_attribute::o2be_attribute(idl_bool ro, AST_Type *ft,
			       UTL_ScopedName *n, UTL_StrList *p)
  : AST_Attribute(ro,ft,n,p),
    AST_Field(AST_Decl::NT_attr,ft,n,p),
    AST_Decl(AST_Decl::NT_attr,n,p),
    o2be_name(AST_Decl::NT_attr,n,p)
{
  pd_mangled_read_signature = 0;
  pd_mangled_write_signature = 0;
}


void
o2be_attribute::produce_decl_rd(std::fstream& s,
				idl_bool use_fully_qualified_names)
{
  o2be_operation::argMapping mapping;
  o2be_operation::argType ntype =
    o2be_operation::ast2ArgMapping(field_type(), o2be_operation::wResult,
				   mapping);
  AST_Decl* decl = field_type();

  switch( ntype ) {
  case o2be_operation::tObjref:
    while( decl->node_type() == AST_Decl::NT_typedef )
      decl = o2be_typedef::narrow_from_decl(decl)->base_type();
    s << o2be_interface::narrow_from_decl(decl)
      ->unambiguous_objref_name(this, use_fully_qualified_names);
    break;
  case o2be_operation::tString:
    s << "char*";
    break;
  case o2be_operation::tTypeCode:
    s << "CORBA::TypeCode_ptr";
    break;
  default:
    s << o2be_name::narrow_and_produce_unambiguous_name(decl, this,
						use_fully_qualified_names);
    break;
  }

  if( mapping.is_arrayslice )  s << "_slice";
  if( mapping.is_pointer    )  s << "*";
  if( mapping.is_reference  )  s << "&";
}


void
o2be_attribute::produce_decl_wr(std::fstream& s,
				idl_bool use_fully_qualified_names,
				idl_bool for_call_desc)
{
  o2be_operation::argMapping mapping;
  o2be_operation::argType ntype =
    o2be_operation::ast2ArgMapping(field_type(), o2be_operation::wIN,mapping);
  AST_Decl* decl = field_type();

  if( mapping.is_const )  s << "const ";

  switch( ntype ) {
  case o2be_operation::tObjref:
    while( decl->node_type() == AST_Decl::NT_typedef )
      decl = o2be_typedef::narrow_from_decl(decl)->base_type();
    s << o2be_interface::narrow_from_decl(decl)
      ->unambiguous_objref_name(this, use_fully_qualified_names);
    break;
  case o2be_operation::tString:
    s << "char*";
    break;
  case o2be_operation::tTypeCode:
    s << "CORBA::TypeCode_ptr";
    break;
  case o2be_operation::tArrayFixed:
  case o2be_operation::tArrayVariable:
    // We use pointer to slice for passing arrays to call descriptors ...
    if( for_call_desc ) {
      mapping.is_arrayslice = I_TRUE;
      mapping.is_pointer = I_TRUE;
    }
    s << o2be_name::narrow_and_produce_unambiguous_name(decl, this,
						use_fully_qualified_names);
    break;
  default:
    s << o2be_name::narrow_and_produce_unambiguous_name(decl, this,
						use_fully_qualified_names);
    break;
  }
  if( mapping.is_arrayslice )  s << "_slice";
  if( mapping.is_pointer    )  s << "*";
  if( mapping.is_reference  )  s << "&";
}


void
o2be_attribute::produce_read_proxy_call_desc(std::fstream& s,
					     const char* class_name)
{
  IND(s); s << "// Proxy call descriptor class. Mangled signature:\n";
  IND(s); s << "//  " << mangled_read_signature() << '\n';
  IND(s); s << "class " << class_name << '\n';
  IND(s); s << "  : public " << "OmniProxyCallDesc" << '\n';
  IND(s); s << "{\n";
  IND(s); s << "public:\n";
  INC_INDENT_LEVEL();

  // Constructor.
  IND(s); s << "inline " << class_name
	    << "(const char* _op, size_t _op_len) :\n";
  IND(s); s << "  OmniProxyCallDesc(_op, _op_len)  {}\n\n";

  // Declaration of methods to implement the call.
  IND(s); s << "virtual void unmarshalReturnedValues(GIOP_C&);\n";

  // Result accessor.
  IND(s); s << "inline ";
  produce_decl_rd(s, I_TRUE);
  s << " result() { return pd_result; }\n";

  s << "\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "private:\n";
  INC_INDENT_LEVEL();

  // Private data members - return value.
  IND(s); produce_decl_rd(s, I_TRUE);
  s << " pd_result;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "};\n\n";


  IND(s); s << "void " << class_name
	      << "::unmarshalReturnedValues(GIOP_C& giop_client)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();

  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype =
      o2be_operation::ast2ArgMapping(field_type(), o2be_operation::wResult,
				     mapping);

    // Allocate memory for the return value.
    if (mapping.is_arrayslice) {
      IND(s); s << "pd_result = ";
      AST_Decl* truetype = field_type();
      while( truetype->node_type() == AST_Decl::NT_typedef )
	truetype = o2be_typedef::narrow_from_decl(truetype)->base_type();
      s << o2be_array::narrow_from_decl(truetype)->fqname()
	<< "_alloc();\n";
    }
    else if (mapping.is_pointer) {
      IND(s); s << "pd_result = new ";
      o2be_operation::declareVarType(s, field_type(), o2be_global::root());
      s << ";\n";
    }

    // Unmarshal it.
    o2be_operation::produceUnMarshalCode(s, field_type(), o2be_global::root(),
					 "giop_client", "pd_result", ntype,
					 mapping);
  }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void
o2be_attribute::produce_write_proxy_call_desc(std::fstream& s,
					      const char* class_name)
{
  IND(s); s << "// Proxy call descriptor class. Mangled signature:\n";
  IND(s); s << "//  " << mangled_write_signature() << '\n';
  IND(s); s << "class " << class_name << '\n';
  IND(s); s << "  : public " << "OmniProxyCallDesc" << '\n';
  IND(s); s << "{\n";
  IND(s); s << "public:\n";
  INC_INDENT_LEVEL();

  // Constructor.
  IND(s); s << "inline " << class_name
	    << "(const char* _op, size_t _op_len, ";
  produce_decl_wr(s, I_TRUE, I_TRUE);
  s << " arg) :\n";
  IND(s); s << "  OmniProxyCallDesc(_op, _op_len),\n";
  IND(s); s << "  _value(arg)  {}\n\n";

  // Declaration of methods to implement the call.
  IND(s); s << "virtual CORBA::ULong alignedSize(CORBA::ULong size_in);\n";
  IND(s); s << "virtual void marshalArguments(GIOP_C&);\n";

  s << "\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "private:\n";
  INC_INDENT_LEVEL();

  // Private data members - argument.
  IND(s); produce_decl_wr(s, I_TRUE, I_TRUE);
  s << " _value;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "};\n\n";


  IND(s); s << "CORBA::ULong " << class_name
	    << "::alignedSize(CORBA::ULong msgsize)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  o2be_operation::argMapping mapping;
  o2be_operation::argType ntype =
    o2be_operation::ast2ArgMapping(field_type(), o2be_operation::wIN, mapping);

  o2be_operation::produceSizeCalculation(s, field_type(),
					 o2be_global::root(),
					 "giop_client", "msgsize",
					 "_value", ntype, mapping);
  IND(s); s << "return msgsize;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";


  IND(s); s << "void " << class_name
	    << "::marshalArguments(GIOP_C& giop_client)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  o2be_operation::produceMarshalCode(s, field_type(),
				     o2be_global::root(), "giop_client",
				     "_value", ntype, mapping);
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void
o2be_attribute::produce_proxy_rd_skel(std::fstream& s,
				      o2be_interface& defined_in)
{
  o2be_call_desc::produce_descriptor(s, *this);
  const char* call_desc_class = o2be_call_desc::read_descriptor_name(*this);

  IND(s); produce_decl_rd(s, I_TRUE);
  s << ' ' << defined_in.proxy_fqname() << "::" << uqname() << "()\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << call_desc_class << " _call_desc(\"_get_"
	    << local_name()->get_string() << "\", "
	    << (strlen(local_name()->get_string()) + strlen("_get_") + 1)
	    << ");\n\n";
  IND(s); s << "OmniProxyCallWrapper::invoke(this, _call_desc);\n";
  IND(s); s << "return _call_desc.result();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void 
o2be_attribute::produce_proxy_wr_skel(std::fstream& s,
				      o2be_interface& defined_in)
{
  o2be_call_desc::produce_descriptor(s, *this);
  const char* call_desc_class = o2be_call_desc::write_descriptor_name(*this);

  IND(s); s << "void " << defined_in.proxy_fqname() << "::"
	    << uqname() << '(';
  produce_decl_wr(s, I_TRUE, I_TRUE);
  s << " _value)\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << call_desc_class << " _call_desc(\"_set_"
	    << local_name()->get_string() << "\", "
	    << (strlen(local_name()->get_string()) + strlen("_set_") + 1)
	    << ", _value);\n\n";
  IND(s); s << "OmniProxyCallWrapper::invoke(this, _call_desc);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void
o2be_attribute::produce_server_rd_skel(std::fstream& s,o2be_interface &defined_in)
{
  IND(s); s << "if (!_0RL_response_expected) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "_0RL_s.RequestReceived();\n";

  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
    if (ntype == o2be_operation::tObjref || 
	ntype == o2be_operation::tString ||
	ntype == o2be_operation::tTypeCode ||
	(mapping.is_arrayslice) ||
	(mapping.is_pointer)) 
      {
	// declare a <type>_var variable to manage the pointer type
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this,1,mapping.is_arrayslice);
      }
    else 
      {
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this);
      }
    s << " _0RL_result = " << uqname() << "();\n";
  }

  // calculate reply message size
  IND(s); s << "size_t _0RL_msgsize = (size_t) GIOP_S::ReplyHeaderSize();\n";
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
    if ((ntype == o2be_operation::tObjref ||
	 ntype == o2be_operation::tString ||
	 ntype == o2be_operation::tTypeCode ||
	 mapping.is_pointer) && !mapping.is_arrayslice)
      {
	// These are declared as <type>_var variable 
	if (ntype == o2be_operation::tString) {
	  o2be_operation::produceSizeCalculation(s,field_type(),
						 (AST_Decl*)&defined_in,
						 "_0RL_s","_0RL_msgsize",
						 "_0RL_result",ntype,mapping);
	}
	else {
	  // use operator->() to get to the pointer
	  o2be_operation::produceSizeCalculation(s,field_type(),
						 (AST_Decl*)&defined_in,
						 "_0RL_s","_0RL_msgsize",
						 "(_0RL_result.operator->())",
						 ntype,mapping);
	}
      }
    else
      {
	o2be_operation::produceSizeCalculation(s,field_type(),
					       (AST_Decl*)&defined_in,
					       "_0RL_s","_0RL_msgsize",
					       "_0RL_result",ntype,mapping);
      }
  }

  IND(s); s << "_0RL_s.InitialiseReply(GIOP::NO_EXCEPTION,(CORBA::ULong)_0RL_msgsize);\n";

  // marshall results
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
    if ((ntype == o2be_operation::tObjref || 
	 ntype == o2be_operation::tString ||
	 ntype == o2be_operation::tTypeCode ||
	 mapping.is_pointer)
	&& !mapping.is_arrayslice)
      {
	// These are declared as <type>_var variable 
	if (ntype == o2be_operation::tString) {
	  o2be_operation::produceMarshalCode(s,field_type(),
					     (AST_Decl*)&defined_in,
					     "_0RL_s",
					     "_0RL_result",ntype,mapping);
	}
	else {
	  // use operator->() to get to the pointer
	  o2be_operation::produceMarshalCode(s,field_type(),
					     (AST_Decl*)&defined_in,
					     "_0RL_s",
					     "(_0RL_result.operator->())",
					     ntype,mapping);
	}
      }
    else
      {
	o2be_operation::produceMarshalCode(s,field_type(),
					   (AST_Decl*)&defined_in,
					   "_0RL_s",
					   "_0RL_result",ntype,mapping);
      }
  }

  IND(s); s << "_0RL_s.ReplyCompleted();\n";
  IND(s); s << "return 1;\n";
  return;
}


void
o2be_attribute::produce_server_wr_skel(std::fstream& s,o2be_interface &defined_in)
{
  IND(s); s << "if (!_0RL_response_expected) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  {
    // unmarshall arguments
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wIN,mapping);
    if (ntype == o2be_operation::tObjref || 
	ntype == o2be_operation::tString ||
	ntype == o2be_operation::tTypeCode) 
      {
      // declare a <type>_var variable to manage the pointer type
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this,1);
      }
    else
      {
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this);
      }
    s << " " << "_value;\n";
    o2be_operation::produceUnMarshalCode(s,field_type(),
					 (AST_Decl*)&defined_in,
					 "_0RL_s","_value",
					 ntype,mapping);
  }

  IND(s); s << "_0RL_s.RequestReceived();\n";

  IND(s); s << uqname() << "(_value);\n";

  IND(s); s << "size_t _0RL_msgsize = (size_t) GIOP_S::ReplyHeaderSize();\n";

  IND(s); s << "_0RL_s.InitialiseReply(GIOP::NO_EXCEPTION,(CORBA::ULong)_0RL_msgsize);\n";

  IND(s); s << "_0RL_s.ReplyCompleted();\n";
  IND(s); s << "return 1;\n";
  return;
}


void
o2be_attribute::produce_nil_rd_skel(std::fstream& s)
{
  IND(s); produce_decl_rd(s);
  s << ' ' << uqname() << "() {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);\n";
  s << "#ifdef NEED_DUMMY_RETURN\n";
  IND(s); s << "// never reach here! Dummy return to keep some compilers happy.\n";
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
    if (ntype == o2be_operation::tObjref || 
	ntype == o2be_operation::tString ||
	ntype == o2be_operation::tTypeCode ||
	(mapping.is_arrayslice) ||
	(mapping.is_pointer))
      {
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this,
				       0,mapping.is_arrayslice);
	s << 
       ((ntype != o2be_operation::tObjref && ntype != o2be_operation::tString
	 && ntype != o2be_operation::tTypeCode) ? " *" : "") 
	  << " _0RL_result" << " = "
	  << ((ntype == o2be_operation::tTypeCode) ? "CORBA::TypeCode::_nil()" : "0")
	  << ";\n";
      }
    else
      {
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this);
	s << " _0RL_result";
	switch (ntype)
	  {
	  case o2be_operation::tShort:
	  case o2be_operation::tLong:
	  case o2be_operation::tUShort:
	  case o2be_operation::tULong:
	  case o2be_operation::tFloat:
	  case o2be_operation::tDouble:
	  case o2be_operation::tBoolean:
	  case o2be_operation::tChar:
	  case o2be_operation::tOctet:
	    s << " = 0;\n";
	    break;
	  case o2be_operation::tEnum:
	    {
	      s << " = ";
	      AST_Decl *decl = field_type();
	      while (decl->node_type() == AST_Decl::NT_typedef)
		decl = o2be_typedef::narrow_from_decl(decl)->base_type();
	      UTL_ScopeActiveIterator i(o2be_enum::narrow_from_decl(decl),
					UTL_Scope::IK_decls);
	      AST_Decl *eval = i.item();
	      s << o2be_name::narrow_and_produce_unambiguous_name(eval,this) 
		<< ";\n";
	    }
	    break;
	  case o2be_operation::tStructFixed:
	    s << ";\n";
	    s << "memset((void *)&_0RL_result,0,sizeof(_0RL_result));\n";
	    break;
	  default:
	    s << ";\n";
	    break;
	  }
      }
  }
  IND(s); s << "return _0RL_result;\n";
  s << "#endif\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
}


void
o2be_attribute::produce_nil_wr_skel(std::fstream& s)
{
  IND(s); s << "void " << uqname() << '(';
  produce_decl_wr(s);
  s << " _value) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw CORBA::BAD_OPERATION(0,CORBA::COMPLETED_NO);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
}


void
o2be_attribute::produce_lcproxy_rd_skel(std::fstream& s,
					o2be_interface& defined_in)
{
  idl_bool hasVariableLenOutArgs = I_FALSE;

  IND(s); produce_decl_rd(s, I_TRUE);
  s << ' ' << defined_in.lcproxy_fqname() << "::" << uqname() << "()\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::ULong _0RL_retries = 0;\n";
  s << "#ifndef EGCS_WORKAROUND\n";
  s << "_0RL_again:\n";
  s << "#else\n";
  s << "while(1) {\n";
  s << "#endif\n";
  IND(s); s << "assertObjectExistent();\n";
  IND(s); s << "omniRopeAndKey _0RL_r;\n";
  IND(s); s << "CORBA::Boolean _0RL_fwd = getRopeAndKey(_0RL_r);\n";
  IND(s); s << "CORBA::Boolean _0RL_reuse = 0;\n";

  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
    if (ntype == o2be_operation::tObjref || 
	ntype == o2be_operation::tString ||
	ntype == o2be_operation::tTypeCode ||
	(mapping.is_arrayslice) ||
	(mapping.is_pointer))
      {
	hasVariableLenOutArgs = I_TRUE;
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this,0,mapping.is_arrayslice);
	s << 
	 ((ntype != o2be_operation::tObjref && ntype != o2be_operation::tString
	   && ntype != o2be_operation::tTypeCode)?" *":"") 
	  << " _0RL_result = "
	  << ((ntype == o2be_operation::tTypeCode) ? "CORBA::TypeCode::_nil()" : "0")
	  << ";\n";
      }
    else
      {
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this);
	s << " _0RL_result;\n";
      }
  }

  IND(s); s << "try {\n";
  INC_INDENT_LEVEL();

  IND(s); s << "GIOP_C _0RL_c(_0RL_r.rope());\n";
  IND(s); s << "_0RL_reuse = _0RL_c.isReUsingExistingConnection();\n";

  // calculate request message size
  IND(s); s << "CORBA::ULong _0RL_msgsize = GIOP_C::RequestHeaderSize(_0RL_r.keysize(),"
	    << strlen("_get_") + strlen(local_name()->get_string()) + 1 
	    << ");\n";

  IND(s); s << "_0RL_c.InitialiseRequest(_0RL_r.key(),_0RL_r.keysize(),(char *)\""
	    << "_get_" << local_name()->get_string() << "\"," << strlen("_get_") + strlen(local_name()->get_string()) + 1 << ",_0RL_msgsize,0);\n";

  IND(s); s << "switch (_0RL_c.ReceiveReply())\n";  // invoke method
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();

  IND(s); s << "case GIOP::NO_EXCEPTION:\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  // unmarshall results
  if (hasVariableLenOutArgs)
    {
      o2be_operation::argMapping mapping;
      o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
      if (mapping.is_arrayslice) {
	IND(s); s << "_0RL_result = ";
	AST_Decl *truetype = field_type();
	while (truetype->node_type() == AST_Decl::NT_typedef) {
	  truetype = o2be_typedef::narrow_from_decl(truetype)->base_type();
	}
	s << o2be_array::narrow_from_decl(truetype)->fqname();
	s  << "_alloc();\n";
      }
      else if (mapping.is_pointer) {
	IND(s); s << "_0RL_result = new ";
	o2be_operation::declareVarType(s,field_type(),this);
	s << ";\n";
      }
    }

  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
    o2be_operation::produceUnMarshalCode(s,field_type(),
					 (AST_Decl*)&defined_in,
					 "_0RL_c","_0RL_result",ntype,mapping);
  }

  IND(s); s << "_0RL_c.RequestCompleted();\n";

  if (hasVariableLenOutArgs)
    {
      IND(s); s << "return _0RL_result;\n";

    }
  else
    {
      IND(s); s << "return _0RL_result;\n";
    }
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "case GIOP::USER_EXCEPTION:\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_c.RequestCompleted(1);\n";
  IND(s); s << "throw CORBA::UNKNOWN(0,CORBA::COMPLETED_MAYBE);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "case GIOP::SYSTEM_EXCEPTION:\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_c.RequestCompleted(1);\n";
  IND(s); s << "throw omniORB::fatalException(__FILE__,__LINE__,\"GIOP::SYSTEM_EXCEPTION should not be returned by GIOP_C::ReceiveReply()\");\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "case GIOP::LOCATION_FORWARD:\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::Object_var obj = CORBA::Object::unmarshalObjRef(_0RL_c);\n";
  IND(s); s << "_0RL_c.RequestCompleted();\n";
  IND(s); s << "if (CORBA::is_nil(obj)) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "if (omniORB::traceLevel > 10) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "omniORB::log << \"Received GIOP::LOCATION_FORWARD message that contains a nil object reference.\\n\";\n";
  IND(s); s << "omniORB::log.flush();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "throw CORBA::COMM_FAILURE(0,CORBA::COMPLETED_NO);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "_0RL_c.~GIOP_C();\n";
  IND(s); s << defined_in.wrapproxy_fqname() << " *_0RL_w = _get_wrap_"
	    << defined_in._fqname() << "();\n";
  IND(s); s << "_0RL_w->_forward_to(obj);\n";
  IND(s); s << "if (omniORB::traceLevel > 10) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "omniORB::log << \"GIOP::LOCATION_FORWARD: retry request.\\n\";\n";
  IND(s); s << "omniORB::log.flush();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "return _0RL_w->" << uqname() << "();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "catch (const CORBA::COMM_FAILURE& ex) {\n";
  INC_INDENT_LEVEL();
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
	
    if (mapping.is_arrayslice)
      {
	IND(s); s << "if (_0RL_result) delete [] _0RL_result;\n";
      }
    else if (mapping.is_reference && mapping.is_pointer)
      {
	IND(s); s << "if (_0RL_result) delete _0RL_result;\n";
      }
    else if (ntype == o2be_operation::tObjref)
      {
	IND(s); s << "if (_0RL_result) CORBA::release(_0RL_result);\n";
      }
    else if (ntype == o2be_operation::tString)
      {
	IND(s); s << "if (_0RL_result) CORBA::string_free(_0RL_result);\n";
      }
    else if (ntype == o2be_operation::tTypeCode)
      {
	IND(s); s << "if (!CORBA::is_nil(_0RL_result)) CORBA::release(_0RL_result);\n";
      }
  }

  IND(s); s << "if (_0RL_reuse) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::TRANSIENT _0RL_ex2(ex.minor(),ex.completed());\n";
  IND(s); s << "if (!_omni_callTransientExceptionHandler(this,_0RL_retries++,_0RL_ex2))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw _0RL_ex2;\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else {\n";
  INC_INDENT_LEVEL();

  IND(s); s << defined_in.wrapproxy_uqname()
	    << " *_0RL_w = _get_wrap_" << defined_in._fqname() << "();\n";
  IND(s); s << "if (_0RL_w->_forwarded()) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_w->_reset_proxy();\n";
  IND(s); s << "CORBA::TRANSIENT _0RL_ex2(ex.minor(),ex.completed());\n";
  IND(s); s << "if (_omni_callTransientExceptionHandler(this,_0RL_retries++,_0RL_ex2)) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "return _0RL_w->" << uqname() << "();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw _0RL_ex2;\n";
  DEC_INDENT_LEVEL();

  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "if (!_omni_callCommFailureExceptionHandler(this,_0RL_retries++,ex))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw;\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  DEC_INDENT_LEVEL();


  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "catch (const CORBA::TRANSIENT& ex) {\n";
  INC_INDENT_LEVEL();
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
	
    if (mapping.is_arrayslice)
      {
	IND(s); s << "if (_0RL_result) delete [] _0RL_result;\n";
      }
    else if (mapping.is_reference && mapping.is_pointer)
      {
	IND(s); s << "if (_0RL_result) delete _0RL_result;\n";
      }
    else if (ntype == o2be_operation::tObjref)
      {
	IND(s); s << "if (_0RL_result) CORBA::release(_0RL_result);\n";
      }
    else if (ntype == o2be_operation::tString)
      {
	IND(s); s << "if (_0RL_result) CORBA::string_free(_0RL_result);\n";
      }
    else if (ntype == o2be_operation::tTypeCode)
      {
	IND(s); s << "if (!CORBA::is_nil(_0RL_result)) CORBA::release(_0RL_result);\n";
      }
  }
  IND(s); s << "if (!_omni_callTransientExceptionHandler(this,_0RL_retries++,ex))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw;\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "catch (const CORBA::OBJECT_NOT_EXIST& ex) {\n";
  INC_INDENT_LEVEL();
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
	
    if (mapping.is_arrayslice)
      {
	IND(s); s << "if (_0RL_result) delete [] _0RL_result;\n";
      }
    else if (mapping.is_reference && mapping.is_pointer)
      {
	IND(s); s << "if (_0RL_result) delete _0RL_result;\n";
      }
    else if (ntype == o2be_operation::tObjref)
      {
	IND(s); s << "if (_0RL_result) CORBA::release(_0RL_result);\n";
      }
    else if (ntype == o2be_operation::tString)
      {
	IND(s); s << "if (_0RL_result) CORBA::string_free(_0RL_result);\n";
      }
    else if (ntype == o2be_operation::tTypeCode)
      {
	IND(s); s << "if (!CORBA::is_nil(_0RL_result)) CORBA::release(_0RL_result);\n";
      }
  }
  IND(s); s << defined_in.wrapproxy_uqname()
	    << " *_0RL_w = _get_wrap_" << defined_in._fqname() << "();\n";
  IND(s); s << "if (_0RL_w->_forwarded()) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_w->_reset_proxy();\n";

  IND(s); s << "CORBA::TRANSIENT _0RL_ex2(ex.minor(),ex.completed());\n";
  IND(s); s << "if (_omni_callTransientExceptionHandler(this,_0RL_retries++,_0RL_ex2)) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "return _0RL_w->" << uqname() << "();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw _0RL_ex2;\n";
  DEC_INDENT_LEVEL();

  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "if (!_omni_callSystemExceptionHandler(this,_0RL_retries++,ex))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw;\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "catch (const CORBA::SystemException& ex) {\n";
  INC_INDENT_LEVEL();
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
	
    if (mapping.is_arrayslice)
      {
	IND(s); s << "if (_0RL_result) delete [] _0RL_result;\n";
      }
    else if (mapping.is_reference && mapping.is_pointer)
      {
	IND(s); s << "if (_0RL_result) delete _0RL_result;\n";
      }
    else if (ntype == o2be_operation::tObjref)
      {
	IND(s); s << "if (_0RL_result) CORBA::release(_0RL_result);\n";
      }
    else if (ntype == o2be_operation::tString)
      {
	IND(s); s << "if (_0RL_result) CORBA::string_free(_0RL_result);\n";
      }
    else if (ntype == o2be_operation::tTypeCode)
      {
	IND(s); s << "if (!CORBA::is_nil(_0RL_result)) CORBA::release(_0RL_result);\n";
      }
  }
  IND(s); s << "if (!_omni_callSystemExceptionHandler(this,_0RL_retries++,ex))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw;\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  if (hasVariableLenOutArgs) {
    IND(s); s << "catch (...) {\n";
    INC_INDENT_LEVEL();
    {
      o2be_operation::argMapping mapping;
      o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
	
      if (mapping.is_arrayslice)
	{
	  IND(s); s << "if (_0RL_result) delete [] _0RL_result;\n";
	}
      else if (mapping.is_reference && mapping.is_pointer)
	{
	  IND(s); s << "if (_0RL_result) delete _0RL_result;\n";
	}
      else if (ntype == o2be_operation::tObjref)
	{
	  IND(s); s << "if (_0RL_result) CORBA::release(_0RL_result);\n";
	}
      else if (ntype == o2be_operation::tString)
	{
	  IND(s); s << "if (_0RL_result) CORBA::string_free(_0RL_result);\n";
	}
      else if (ntype == o2be_operation::tTypeCode)
	{
	  IND(s); s << "if (!CORBA::is_nil(_0RL_result)) CORBA::release(_0RL_result);\n";
	}
    }
    IND(s); s << "throw;\n";
    DEC_INDENT_LEVEL();
    IND(s); s << "}\n";
  }

  s << "#ifndef EGCS_WORKAROUND\n";
  IND(s); s << "goto _0RL_again;\n";
  s << "#else\n";
  s << "}\n";
  s << "#endif\n";

  s << "#ifdef NEED_DUMMY_RETURN\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "// never reach here! Dummy return to keep some compilers happy.\n";
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
    if (ntype == o2be_operation::tObjref || 
	ntype == o2be_operation::tString ||
	ntype == o2be_operation::tTypeCode ||
	(mapping.is_arrayslice) ||
	(mapping.is_pointer))
      {
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this,0,mapping.is_arrayslice);
	s << 
	((ntype != o2be_operation::tObjref && ntype != o2be_operation::tString
	  && ntype != o2be_operation::tTypeCode)?" *":"") 
	  << " _0RL_result" << " = "
	  << ((ntype == o2be_operation::tTypeCode) ? "CORBA::TypeCode::_nil()" : "0")
	  << ";\n";
      }
    else
      {
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this);
	s << " _0RL_result";
	switch (ntype)
	  {
	  case o2be_operation::tShort:
	  case o2be_operation::tLong:
	  case o2be_operation::tUShort:
	  case o2be_operation::tULong:
	  case o2be_operation::tFloat:
	  case o2be_operation::tDouble:
	  case o2be_operation::tBoolean:
	  case o2be_operation::tChar:
	  case o2be_operation::tOctet:
	    s << " = 0;\n";
	    break;
	  case o2be_operation::tEnum:
	    {
	      s << " = ";
	      AST_Decl *decl = field_type();
	      while (decl->node_type() == AST_Decl::NT_typedef)
		decl = o2be_typedef::narrow_from_decl(decl)->base_type();
	      UTL_ScopeActiveIterator i(o2be_enum::narrow_from_decl(decl),
					UTL_Scope::IK_decls);
	      AST_Decl *eval = i.item();
	      s << o2be_name::narrow_and_produce_fqname(eval) << ";\n";
	    }
	    break;
	  case o2be_operation::tStructFixed:
	    s << ";\n";
	    s << "memset((void *)&_0RL_result,0,sizeof(_0RL_result));\n";
	    break;
	  default:
	    s << ";\n";
	    break;
	  }
      }
  }
  IND(s); s << "return _0RL_result;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  s << "#endif\n";

  DEC_INDENT_LEVEL();
  IND(s);s << "}\n";
  return;
}


void 
o2be_attribute::produce_lcproxy_wr_skel(std::fstream& s,o2be_interface &defined_in)
{
  idl_bool hasVariableLenOutArgs = I_FALSE;

  IND(s); s << "void " << defined_in.lcproxy_fqname() << "::"
	    << uqname() << '(';
  produce_decl_wr(s, I_TRUE);
  s << " _value) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::ULong _0RL_retries = 0;\n";
  s << "#ifndef EGCS_WORKAROUND\n";
  s << "_0RL_again:\n";
  s << "#else\n";
  s << "while(1) {\n";
  s << "#endif\n";
  IND(s); s << "assertObjectExistent();\n";
  IND(s); s << "omniRopeAndKey _0RL_r;\n";
  IND(s); s << "CORBA::Boolean _0RL_fwd = getRopeAndKey(_0RL_r);\n";
  IND(s); s << "CORBA::Boolean _0RL_reuse = 0;\n";
  IND(s); s << "try {\n";
  INC_INDENT_LEVEL();

  IND(s); s << "GIOP_C _0RL_c(_0RL_r.rope());\n";
  IND(s); s << "_0RL_reuse = _0RL_c.isReUsingExistingConnection();\n";

  // calculate request message size
  IND(s); s << "CORBA::ULong _0RL_msgsize = GIOP_C::RequestHeaderSize(_0RL_r.keysize(),"
	    << strlen("_set_") + strlen(local_name()->get_string()) + 1 
	    << ");\n";

  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wIN,mapping);
    o2be_operation::produceSizeCalculation(s,field_type(),
					   (AST_Decl*)&defined_in,
					   "_0RL_c","_0RL_msgsize",
					   "_value",ntype,mapping);
  }

  IND(s); s << "_0RL_c.InitialiseRequest(_0RL_r.key(),_0RL_r.keysize(),(char *)\""
	    << "_set_" << local_name()->get_string() << "\"," << strlen("_set_") + strlen(local_name()->get_string()) + 1 << ",_0RL_msgsize,0);\n";

  // marshall arguments;
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wIN,mapping);
    o2be_operation::produceMarshalCode(s,field_type(),(AST_Decl*)&defined_in,
				       "_0RL_c",
				       "_value",ntype,mapping);
  }

  IND(s); s << "switch (_0RL_c.ReceiveReply())\n";  // invoke method
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();

  IND(s); s << "case GIOP::NO_EXCEPTION:\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_c.RequestCompleted();\n";
  IND(s); s << "return;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "case GIOP::USER_EXCEPTION:\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_c.RequestCompleted(1);\n";
  IND(s); s << "throw CORBA::UNKNOWN(0,CORBA::COMPLETED_MAYBE);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "case GIOP::SYSTEM_EXCEPTION:\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_c.RequestCompleted(1);\n";
  IND(s); s << "throw omniORB::fatalException(__FILE__,__LINE__,\"GIOP::SYSTEM_EXCEPTION should not be returned by GIOP_C::ReceiveReply()\");\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "case GIOP::LOCATION_FORWARD:\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::Object_var obj = CORBA::Object::unmarshalObjRef(_0RL_c);\n";
  IND(s); s << "_0RL_c.RequestCompleted();\n";
  IND(s); s << "if (CORBA::is_nil(obj)) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "if (omniORB::traceLevel > 10) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "omniORB::log << \"Received GIOP::LOCATION_FORWARD message that contains a nil object reference.\\n\";\n";
  IND(s); s << "omniORB::log.flush();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "throw CORBA::COMM_FAILURE(0,CORBA::COMPLETED_NO);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "_0RL_c.~GIOP_C();\n";
  IND(s); s << defined_in.wrapproxy_fqname() << " *_0RL_w = _get_wrap_"
	    << defined_in._fqname() << "();\n";
  IND(s); s << "_0RL_w->_forward_to(obj);\n";
  IND(s); s << "if (omniORB::traceLevel > 10) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "omniORB::log << \"GIOP::LOCATION_FORWARD: retry request.\\n\";\n";
  IND(s); s << "omniORB::log.flush();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "_0RL_w->" << uqname() << "(_value);\n";
  IND(s); s << "return;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "catch (const CORBA::COMM_FAILURE& ex) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "if (_0RL_reuse) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::TRANSIENT _0RL_ex2(ex.minor(),ex.completed());\n";
  IND(s); s << "if (!_omni_callTransientExceptionHandler(this,_0RL_retries++,_0RL_ex2))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw _0RL_ex2;\n";
  DEC_INDENT_LEVEL();

  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else {\n";
  INC_INDENT_LEVEL();

  IND(s); s << defined_in.wrapproxy_uqname()
	    << " *_0RL_w = _get_wrap_" << defined_in._fqname() << "();\n";
  IND(s); s << "if (_0RL_w->_forwarded()) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_w->_reset_proxy();\n";
  IND(s); s << "CORBA::TRANSIENT _0RL_ex2(ex.minor(),ex.completed());\n";
  IND(s); s << "if (_omni_callTransientExceptionHandler(this,_0RL_retries++,_0RL_ex2)) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_w->" << uqname() << "(_value);\n";
  IND(s); s << "return;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw _0RL_ex2;\n";
  DEC_INDENT_LEVEL();

  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "if (!_omni_callCommFailureExceptionHandler(this,_0RL_retries++,ex))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw;\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "catch (const CORBA::TRANSIENT& ex) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "if (!_omni_callTransientExceptionHandler(this,_0RL_retries++,ex))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw;\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "catch (const CORBA::OBJECT_NOT_EXIST& ex) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << defined_in.wrapproxy_uqname()
	    << " *_0RL_w = _get_wrap_" << defined_in._fqname() << "();\n";
  IND(s); s << "if (_0RL_w->_forwarded()) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_w->_reset_proxy();\n";
  IND(s); s << "CORBA::TRANSIENT _0RL_ex2(ex.minor(),ex.completed());\n";
  IND(s); s << "if (_omni_callTransientExceptionHandler(this,_0RL_retries++,_0RL_ex2)) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_w->" << uqname() << "(_value);\n";
  IND(s); s << "return;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw _0RL_ex2;\n";
  DEC_INDENT_LEVEL();

  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "if (!_omni_callSystemExceptionHandler(this,_0RL_retries++,ex))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw;\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  IND(s); s << "catch (const CORBA::SystemException& ex) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "if (!_omni_callSystemExceptionHandler(this,_0RL_retries++,ex))\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw;\n";
  DEC_INDENT_LEVEL();
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";

  s << "#ifndef EGCS_WORKAROUND\n";
  IND(s); s << "goto _0RL_again;\n";
  s << "#else\n";
  s << "}\n";
  s << "#endif\n";

  DEC_INDENT_LEVEL();
  IND(s);s << "}\n";
  return;
}


void
o2be_attribute::produce_dead_rd_skel(std::fstream& s)
{
  IND(s); produce_decl_rd(s, I_TRUE);
  s << ' ' << uqname() << "()\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);\n";
  s << "#ifdef NEED_DUMMY_RETURN\n";
  IND(s); s << "// never reach here! Dummy return to keep some compilers happy.\n";
  {
    o2be_operation::argMapping mapping;
    o2be_operation::argType ntype = o2be_operation::ast2ArgMapping(field_type(),o2be_operation::wResult,mapping);
    if (ntype == o2be_operation::tObjref || 
	ntype == o2be_operation::tString ||
	ntype == o2be_operation::tTypeCode ||
	(mapping.is_arrayslice) ||
	(mapping.is_pointer))
      {
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this,
				       0,mapping.is_arrayslice);

	s << 
	 ((ntype != o2be_operation::tObjref && ntype != o2be_operation::tString
	   && ntype != o2be_operation::tTypeCode)?" *":"") 
	  << " _0RL_result" << " = "
	  << ((ntype == o2be_operation::tTypeCode) ? "CORBA::TypeCode::_nil()" : "0")
	  << ";\n";
      }
    else
      {
	IND(s);
	o2be_operation::declareVarType(s,field_type(),this);
	s << " _0RL_result";
	switch (ntype)
	  {
	  case o2be_operation::tShort:
	  case o2be_operation::tLong:
	  case o2be_operation::tUShort:
	  case o2be_operation::tULong:
	  case o2be_operation::tFloat:
	  case o2be_operation::tDouble:
	  case o2be_operation::tBoolean:
	  case o2be_operation::tChar:
	  case o2be_operation::tOctet:
	    s << " = 0;\n";
	    break;
	  case o2be_operation::tEnum:
	    {
	      s << " = ";
	      AST_Decl *decl = field_type();
	      while (decl->node_type() == AST_Decl::NT_typedef)
		decl = o2be_typedef::narrow_from_decl(decl)->base_type();
	      UTL_ScopeActiveIterator i(o2be_enum::narrow_from_decl(decl),
					UTL_Scope::IK_decls);
	      AST_Decl *eval = i.item();
	      s << o2be_name::narrow_and_produce_unambiguous_name(eval,this) 
		<< ";\n";
	    }
	    break;
	  case o2be_operation::tStructFixed:
	    s << ";\n";
	    s << "memset((void *)&_0RL_result,0,sizeof(_0RL_result));\n";
	    break;
	  default:
	    s << ";\n";
	    break;
	  }
      }
  }
  IND(s); s << "return _0RL_result;\n";
  s << "#endif\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
}


void
o2be_attribute::produce_dead_wr_skel(std::fstream& s)
{
  IND(s); s << "void " << uqname() << '(';
  produce_decl_wr(s, I_TRUE);
  s << " _value) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "throw CORBA::OBJECT_NOT_EXIST(0,CORBA::COMPLETED_NO);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
}


void
o2be_attribute::produce_home_rd_skel(std::fstream& s,
				     o2be_interface &defined_in)
{
  IND(s); produce_decl_rd(s, I_TRUE);
  s << ' ' << uqname() << "() {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "return _actual_" << defined_in._fqname() << "->"
	    << uqname() << "();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
}


void
o2be_attribute::produce_home_wr_skel(std::fstream& s, o2be_interface &defined_in)
{
  IND(s); s << "void " << uqname() << '(';
  produce_decl_wr(s, I_TRUE);
  s << " _value) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_actual_" << defined_in._fqname() << "->"
	    << uqname() << "(_value);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
}


void
o2be_attribute::produce_wrapproxy_rd_skel(std::fstream& s,
					  o2be_interface &defined_in)
{
  IND(s); produce_decl_rd(s, I_TRUE);
  s << ' ' << uqname() << "() {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "return _actual_" << defined_in._fqname() << "->"
	    << uqname() << "();\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
}


void
o2be_attribute::produce_wrapproxy_wr_skel(std::fstream& s,
					  o2be_interface &defined_in)
{
  IND(s); s << "void " << uqname() << '(';
  produce_decl_wr(s, I_TRUE);
  s << " _value) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_actual_" << defined_in._fqname() << "->"
	    << uqname() << "(_value);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
}


const char*
o2be_attribute::mangled_read_signature()
{
  if( !pd_mangled_read_signature )
    pd_mangled_read_signature =
      o2be_name_mangler::produce_attribute_read_signature(*this);

  return pd_mangled_read_signature;
}


const char*
o2be_attribute::mangled_write_signature()
{
  if( !pd_mangled_write_signature )
    pd_mangled_write_signature =
      o2be_name_mangler::produce_attribute_write_signature(*this);

  return pd_mangled_write_signature;
}


IMPL_NARROW_METHODS1(o2be_attribute, AST_Attribute)
IMPL_NARROW_FROM_DECL(o2be_attribute)
IMPL_NARROW_FROM_SCOPE(o2be_attribute)
