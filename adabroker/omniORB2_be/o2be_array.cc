// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_array.cc            Created on: 07/10/1996
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
  $Log: o2be_array.cc,v $
  Revision 1.1  1999/02/14 17:45:21  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.14  1999/01/07 09:51:56  djr
  Changes to support new TypeCode/Any implementation, which is now
  placed in a new file ...DynSK.cc (by default). Other minor
  changes.

  Revision 1.13  1998/08/19 15:50:08  sll
  New member functions void produce_binary_operators_in_hdr and the like
  are responsible for generating binary operators <<= etc in the global
  namespace.

  Revision 1.12  1998/08/13 22:35:13  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available

  Revision 1.11  1998/04/07 18:39:39  sll
  Use std::fstream instead of fstream.

// Revision 1.10  1998/03/09  17:14:39  ewc
// Use new _Forany function to get to underlying array slice. Avoid
// explicit operator call that caused problems for aC++ on HPUX
//
// Revision 1.9  1998/01/27  16:33:34  ewc
//  Added support for type any and TypeCode
//
  Revision 1.8  1997/12/23 19:27:51  sll
  Bug fixes.

  Revision 1.7  1997/12/18 17:28:43  sll
  *** empty log message ***

  Revision 1.6  1997/12/09 19:55:17  sll
  *** empty log message ***

// Revision 1.5  1997/05/06  13:49:08  sll
// Public release.
//
  */


#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

#define ADPT_CLASS_TEMPLATE "_CORBA_Array_OUT_arg"

o2be_array::o2be_array(UTL_ScopedName* n,
		       unsigned long ndims,
		       UTL_ExprList* dims)
  : AST_Array(n, ndims, dims),
    AST_Decl(AST_Decl::NT_array, n, NULL),
    o2be_name(AST_Decl::NT_array,n,NULL)
{
  pd_have_produced_tcParser_buildDesc_code = I_FALSE;
}

idl_bool
o2be_array::isVariable()
{
  idl_bool isvar;

  AST_Decl* decl = getElementType();
  switch (decl->node_type())
    {
    case AST_Decl::NT_interface:
    case AST_Decl::NT_string:
    case AST_Decl::NT_sequence:
      isvar = I_TRUE;
      break;
    case AST_Decl::NT_union:
      isvar = o2be_union::narrow_from_decl(decl)->isVariable();
      break;
    case AST_Decl::NT_struct:
      isvar = o2be_structure::narrow_from_decl(decl)->isVariable();
      break;
    case AST_Decl::NT_pre_defined:
      if (o2be_predefined_type::narrow_from_decl(decl)->pt() ==
	  AST_PredefinedType::PT_any || 
	  o2be_predefined_type::narrow_from_decl(decl)->pt() == 
	  AST_PredefinedType::PT_TypeCode)
	isvar = I_TRUE;
      else
	isvar = I_FALSE;
      break;
    default:
      isvar = I_FALSE;
    }
  return isvar;
}

size_t
o2be_array::getSliceDim()
{
  AST_Expression** d = dims();
  AST_Expression::AST_ExprValue* v = d[0]->ev();
  switch (v->et) {
  case AST_Expression::EV_short:
    return (size_t)v->u.sval;
  case AST_Expression::EV_ushort:
    return (size_t)v->u.usval;
  case AST_Expression::EV_long:
    return (size_t)v->u.lval;
  case AST_Expression::EV_ulong:
    return (size_t)v->u.ulval;
  default:
    throw o2be_internal_error(__FILE__, __LINE__,
			      "unexpected type for array dimension");
  }
  return 0;
}

AST_Decl*
o2be_array::getElementType()
{
  AST_Decl* decl = base_type();
  
  while (1) {
    while (decl->node_type() == AST_Decl::NT_typedef)
      decl = o2be_typedef::narrow_from_decl(decl)->base_type();
    if (decl->node_type() == AST_Decl::NT_array) {
      return o2be_array::narrow_from_decl(decl)->getElementType();
    }
    else {
      return decl;
    }
  }
}

size_t
o2be_array::getNumOfElements()
{
  size_t dim = 1;
  AST_Expression** d = dims();
  unsigned long i;
  for( i=0; i < n_dims(); i++ ) {
    AST_Expression::AST_ExprValue* v = d[i]->ev();
    switch( v->et ) {
    case AST_Expression::EV_short:
      dim = dim * v->u.sval;
      break;
    case AST_Expression::EV_ushort:
      dim = dim * v->u.usval;
      break;
    case AST_Expression::EV_long:
      dim = dim * v->u.lval;
      break;
    case AST_Expression::EV_ulong:
      dim = dim * v->u.ulval;
      break;
    default:
      throw o2be_internal_error(__FILE__, __LINE__,
				"unexpected type for array dimension");
      break;
    }
  }
  AST_Decl* decl = base_type();
  while (1) {
    while (decl->node_type() == AST_Decl::NT_typedef)
      decl = o2be_typedef::narrow_from_decl(decl)->base_type();
    if (decl->node_type() == AST_Decl::NT_array) {
      dim = dim * o2be_array::narrow_from_decl(decl)->getNumOfElements();
      return dim;
    }
    else {
      return dim;
    }
  }
}

size_t
o2be_array::getNumOfDims()
{
  size_t d = 0;
  AST_Decl* decl = this;

  do {
    o2be_array* array = o2be_array::narrow_from_decl(decl);
    d += array->n_dims();

    decl = array->base_type();
    while( decl->node_type() == AST_Decl::NT_typedef )
      decl = o2be_typedef::narrow_from_decl(decl)->base_type();

  } while( decl->node_type() == AST_Decl::NT_array );

  return d;
}

o2be_array::
dim_iterator::dim_iterator(o2be_array* v)
{
  pd_ndim = v->getNumOfDims();
  pd_next = 0;
  pd_dims = new AST_Expression* [pd_ndim];

  int i;
  int ndim;
  AST_Expression **d;

  d = v->dims();
  ndim = v->n_dims();
  for (i=0; i < ndim; i++) {
    pd_dims[pd_next++] = d[i];
  }

  while (pd_next < pd_ndim) {
    AST_Decl* decl = v->base_type();
    while (decl->node_type() == AST_Decl::NT_typedef)
      decl = o2be_typedef::narrow_from_decl(decl)->base_type();
    d = o2be_array::narrow_from_decl(decl)->dims();
    ndim = o2be_array::narrow_from_decl(decl)->n_dims();
    for (i=0; i < ndim; i++) {
      pd_dims[pd_next++] = d[i];
    }
  };
    
  pd_next = 0;
  return;
}

size_t
o2be_array::
dim_iterator::operator() ()
{
  AST_Expression::AST_ExprValue* v;

  if (pd_next < pd_ndim)
    v = pd_dims[pd_next++]->ev();
  else
    return 0;

  switch (v->et) 
    {
    case AST_Expression::EV_short:
      return (size_t)v->u.sval;
    case AST_Expression::EV_ushort:
      return (size_t)v->u.usval;
    case AST_Expression::EV_long:
      return (size_t)v->u.lval;
    case AST_Expression::EV_ulong:
      return (size_t)v->u.ulval;
    default:
      throw o2be_internal_error(__FILE__,__LINE__,"unexpected type for array dimension");
    }
  return 0;
}

const char*
o2be_array::member_name(AST_Decl* decl, AST_Decl* in)
{
  switch( decl->node_type() ) {
  case AST_Decl::NT_string:
    return o2be_string::fieldMemberTypeName();
  case AST_Decl::NT_interface:
    return o2be_interface::narrow_from_decl(decl)->fieldMemberType_fqname(in);
  case AST_Decl::NT_typedef:
    return o2be_typedef::narrow_from_decl(decl)->fieldMemberType_fqname(in);
  case AST_Decl::NT_sequence:
    return o2be_sequence::narrow_from_decl(decl)->seq_template_name(in);
  case AST_Decl::NT_pre_defined:
    return o2be_predefined_type::narrow_from_decl(decl)->fieldMemberTypeName();
  default:
    return o2be_name::narrow_and_produce_unambiguous_name(decl, in);
  }
}

void
o2be_array::produce_hdr (std::fstream &s, o2be_typedef* tdef)
{
  const char* elm_fqname = element_name(tdef);

  s << "\n";
  IND(s); s << "typedef " << elm_fqname << " "
	    << tdef->uqname();
  {
    int nd = n_dims();
    AST_Expression **d = dims();
    int i;
    for (i=0; i < nd; i++)
      {
	s << "[";
	AST_Expression::AST_ExprValue* v = d[i]->ev();
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
	  default:
	    throw o2be_internal_error(__FILE__,__LINE__,"unexpected type in array expression");
	  }
	s << "]";
      }
  }	
  s << ";\n";

  IND(s); s << "typedef " << elm_fqname << " "
	    << tdef->uqname() << "_slice";
  {
    int nd = n_dims();
    AST_Expression **d = dims();
    int i;
    for (i=1; i < nd; i++)
      {
	s << "[";
	AST_Expression::AST_ExprValue* v = d[i]->ev();
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
	  default:
	    throw o2be_internal_error(__FILE__,__LINE__,"unexpected type in array expression");
	  }
	s << "]";
      }
  }	
  s << ";\n\n";

  if (defined_in() == idl_global->root())
    {
      // memory management functions are declared as externs in the global
      // scope. Their implementation cannot be generated inline.
      IND(s); s << "extern "
		<< tdef->uqname() << "_slice* "
		<< tdef->uqname() << "_alloc();\n";
      IND(s); s << "extern "
		<< tdef->uqname() << "_slice* "<< tdef->uqname() << "_dup(const "
		<< tdef->uqname() << "_slice* _s);\n";
      IND(s); s << "extern "
		<< "void " << tdef->uqname() << "_free("
		<< tdef->uqname() << "_slice* _s);\n";
    }
  else
    {
      IND(s); s << "static inline "
		<< tdef->uqname() << "_slice* "
		<< tdef->uqname() << "_alloc() {\n";
      INC_INDENT_LEVEL();
      IND(s); s << "return new " << tdef->uqname() << "_slice["
		<< getSliceDim()
		<< "];\n";
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n\n";
      IND(s); s << "static inline "
		<< tdef->uqname() << "_slice* "<< tdef->uqname() << "_dup(const "
		<< tdef->uqname() << "_slice* _s) {\n";
      INC_INDENT_LEVEL();
      IND(s); s << "if (!_s) return 0;\n";
      IND(s); s << tdef->uqname() << "_slice* _data = "
		<< tdef->uqname() << "_alloc();\n";
      IND(s); s << "if (_data) {\n";
      INC_INDENT_LEVEL();
      {
	unsigned int ndim = 0;
	unsigned int dimval;
	o2be_array::dim_iterator next(this);
	while (ndim < getNumOfDims())
	  {
	    dimval = next();
	    IND(s); s << "for (unsigned int _i" << ndim << " =0;"
		      << "_i" << ndim << " < " << dimval << ";"
		      << "_i" << ndim << "++) {\n";
	    INC_INDENT_LEVEL();
	    ndim++;
	  }
	
	IND(s); s << "_data";
	ndim = 0;
	while (ndim < getNumOfDims())
	  {
	    s << "[_i" << ndim << "]";
	    ndim++;
	  }
	s << " = _s";
	ndim = 0;
	while (ndim < getNumOfDims())
	  {
	    s << "[_i" << ndim << "]";
	    ndim++;
	  }
	s << ";\n";
	ndim = 0;
	while (ndim < getNumOfDims())
	  {
	    DEC_INDENT_LEVEL();
	    IND(s); s << "}\n";
	    ndim++;
	  }
      }
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      IND(s); s << "return _data;\n";
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n\n";
      IND(s); s << "static inline "
		<< "void " << tdef->uqname() << "_free("
		<< tdef->uqname() << "_slice* _s) {\n";
      INC_INDENT_LEVEL();
      IND(s); s << "delete [] _s;\n";
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n\n";
    }

  IND(s); s << "class " << tdef->uqname() << "_copyHelper {\n";
  IND(s); s << "public:\n";
  INC_INDENT_LEVEL();
  IND(s); s << "static inline " << tdef->uqname() << "_slice* alloc() { return "
	    << tdef->uqname() << "_alloc(); }\n";
  IND(s); s << "static inline " << tdef->uqname() << "_slice* dup(const "
	    << tdef->uqname() << "_slice* p) { return "
	    << tdef->uqname() << "_dup(p); }\n";
  IND(s); s << "static inline void free("
	    << tdef->uqname() << "_slice* p) { "
	    << tdef->uqname() << "_free(p); }\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "};\n\n";

  IND(s); s << "typedef _CORBA_Array_Var<" 
	    << tdef->uqname() << "_copyHelper,"
	    << tdef->uqname() << "_slice> "
	    << tdef->uqname() << "_var;\n";
  IND(s); s << "typedef _CORBA_Array_Forany<"
	    << tdef->uqname() << "_copyHelper,"
	    << tdef->uqname() << "_slice> "
	    << tdef->uqname() << "_forany;\n\n";
}


#define PIM_INDEX    0
#define PIM_NAME     1

static void produce_indicies(o2be_array* array, std::fstream& s,
			     size_t from_dim, size_t to_dim, int mode)
{
  size_t dim;
  o2be_array::dim_iterator next(array);
  for( dim = 1; dim < from_dim; dim++ )  next();

  switch( mode ){
  case PIM_INDEX:
    for( ; dim <= to_dim; dim++ )
      s << '[' << next() << ']';
    break;
  case PIM_NAME:
    for( ; dim <= to_dim; dim++ )
      s << "_a" << next();
    break;
  }
}


void
o2be_array::produce_skel (std::fstream &s, o2be_typedef* tdef)
{
  if (defined_in() == idl_global->root())
    {
      // memory management functions are declared as externs in the global
      // scope. Generate their implemenation here.
      IND(s); s << tdef->uqname() << "_slice* "
		<< tdef->uqname() << "_alloc() {\n";
      INC_INDENT_LEVEL();
      IND(s); s << "return new " << tdef->uqname() << "_slice["
		<< getSliceDim()
		<< "];\n";
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n\n";

      IND(s); s << tdef->uqname() << "_slice* "<< tdef->uqname()
		<< "_dup(const " << tdef->uqname() << "_slice* _s)\n";
      IND(s); s << "{\n";
      INC_INDENT_LEVEL();
      IND(s); s << "if (!_s) return 0;\n";
      IND(s); s << tdef->uqname() << "_slice* _data = "
		<< tdef->uqname() << "_alloc();\n";
      IND(s); s << "if (_data) {\n";
      INC_INDENT_LEVEL();
      {
	unsigned int ndim = 0;
	unsigned int dimval;
	o2be_array::dim_iterator next(this);
	while (ndim < getNumOfDims())
	  {
	    dimval = next();
	    IND(s); s << "for (unsigned int _i" << ndim << " =0;"
		      << "_i" << ndim << " < " << dimval << ";"
		      << "_i" << ndim << "++) {\n";
	    INC_INDENT_LEVEL();
	    ndim++;
	  }
	
	IND(s); s << "_data";
	ndim = 0;
	while (ndim < getNumOfDims())
	  {
	    s << "[_i" << ndim << "]";
	    ndim++;
	  }
	s << " = _s";
	ndim = 0;
	while (ndim < getNumOfDims())
	  {
	    s << "[_i" << ndim << "]";
	    ndim++;
	  }
	s << ";\n";
	ndim = 0;
	while (ndim < getNumOfDims())
	  {
	    DEC_INDENT_LEVEL();
	    IND(s); s << "}\n";
	    ndim++;
	  }
      }
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      IND(s); s << "return _data;\n";
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n\n";
      IND(s); s << "void " << tdef->uqname() << "_free("
		<< tdef->uqname() << "_slice* _s) {\n";
      INC_INDENT_LEVEL();
      IND(s); s << "delete [] _s;\n";
      DEC_INDENT_LEVEL();
      IND(s); s << "}\n\n";
    }
}


void
o2be_array::produce_dynskel(std::fstream &s, o2be_typedef* tdef)
{
}


void
o2be_array::produce_binary_operators_in_hdr(std::fstream &s,
					    o2be_typedef* tdef)
{
  if (idl_global->compile_flags() & IDL_CF_ANY) {
    IND(s); s << "void operator<<=(CORBA::Any& _a, const "
	      << tdef->fqname()
	      << "_forany& _s);\n";
    IND(s); s << "CORBA::Boolean operator>>=(const CORBA::Any& _a, "
	      << tdef->fqname() << "_forany& _s);\n\n";
  }
}


void
o2be_array::produce_binary_operators_in_dynskel(std::fstream &s,
						o2be_typedef* tdef)
{
  // Definitions of any insertion and extraction operators, and deletion
  // function.

  IND(s); s << "void _0RL_delete_" << tdef->_idname() << "(void* _data) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << tdef->fqname() << "_slice* _0RL_t = (" << tdef->fqname() 
	    << "_slice*) _data;\n";
  IND(s); s << tdef->fqname() << "_free(_0RL_t);\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  //////////////////////////////////////////////////////////////////////
  //////////////////////// tcDescriptor generation /////////////////////
  //////////////////////////////////////////////////////////////////////

  produce_buildDesc_support(s);

  //////////////////////////////////////////////////////////////////////
  /////////////////////// Any insertion operator ///////////////////////
  //////////////////////////////////////////////////////////////////////

  IND(s); s << "void operator<<=(CORBA::Any& _a, const " << tdef->fqname()
	    << "_forany& _s) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << tdef->fqname() << "_slice* _0RL_s = _s.NP_getSlice();\n";
  IND(s); s << "tcDescriptor _0RL_tcdesc;\n";
  o2be_buildDesc::call_buildDesc(s, tdef, "_0RL_tcdesc", "_0RL_s");
  IND(s); s << "_a.PR_packFrom(" << tdef->fqtcname() << ", &_0RL_tcdesc);\n";
  IND(s); s << "if( _s.NP_nocopy() ) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "delete[] _0RL_s;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";

  //////////////////////////////////////////////////////////////////////
  //////////////////////// Any extraction operator /////////////////////
  //////////////////////////////////////////////////////////////////////

  IND(s); s << "CORBA::Boolean operator>>=(const CORBA::Any& _a, "
	    << tdef->fqname() << "_forany& _s) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << tdef->fqname() << "_slice* _0RL_s = (" << tdef->fqname()
	    << "_slice*) _a.PR_getCachedData();\n";
  IND(s); s << "if( !_0RL_s ) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_0RL_s = " << tdef->fqname() << "_alloc();\n";
  IND(s); s << "tcDescriptor _0RL_tcdesc;\n";
  o2be_buildDesc::call_buildDesc(s, tdef, "_0RL_tcdesc", "_0RL_s");
  IND(s); s << "if( !_a.PR_unpackTo(" << tdef->fqtcname() << ", "
	    "&_0RL_tcdesc) ) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "delete[] _0RL_s;\n";
  IND(s); s << "_s = 0;\n";
  IND(s); s << "return 0;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  // We take the address and cast to get past the
  // const qualifier on <_a>.
  IND(s); s << "((CORBA::Any*)&_a)->PR_setCachedData(_0RL_s, "
	    << "_0RL_delete_" << tdef->_idname() << ");\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "} else {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "CORBA::TypeCode_var _0RL_tc = _a.type();\n";
  IND(s); s << "if( !_0RL_tc->equal(" << tdef->fqtcname() << ") ) {\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_s = 0;\n";
  IND(s); s << "return 0;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  IND(s); s << "_s = _0RL_s;\n";
  IND(s); s << "return 1;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n\n";
}


void
o2be_array::produce_typecode_skel(std::fstream &s)
{
  // Since array types are effectively anonymous, we only
  // need to ensure that the base_type's typecode is generated.
  o2be_name::narrow_and_produce_typecode_skel(base_type(), s);
}

void 
o2be_array::produce_typecode_member(std::fstream &s)
{
  AST_Expression **d = dims();
  unsigned long count;
  for( count = 0; count < n_dims(); count++ )
    {
      s << "CORBA::TypeCode::PR_array_tc(";
      AST_Expression::AST_ExprValue* v = d[count]->ev();
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
	default:
	  throw o2be_internal_error(__FILE__,__LINE__,
				    "unexpected type in array expression");
	}
      s << ", ";
    }    

  AST_Decl* decl = base_type();
  o2be_name::produce_typecode_member(decl, s);

  for( count = 0; count < n_dims(); count++ )  s << ")";
}


void
o2be_array::produce_typedef_hdr(std::fstream &s, o2be_typedef* tdef1,
				o2be_typedef* tdef2)
{
  IND(s); s << "typedef " << tdef2->unambiguous_name(tdef1) 
	    << " " << tdef1->uqname() << ";\n";
  IND(s); s << "typedef " << tdef2->unambiguous_name(tdef1) 
	    << "_slice " << tdef1->uqname() 
	    << "_slice;\n";
  IND(s); s << "typedef " << tdef2->unambiguous_name(tdef1) 
	    << "_copyHelper " << tdef1->uqname() << "_copyHelper;\n";
  IND(s); s << "typedef " << tdef2->unambiguous_name(tdef1) 
	    << "_var " << tdef1->uqname() << "_var;\n";
  IND(s); s << "typedef " << tdef2->unambiguous_name(tdef1) 
	    << "_forany " << tdef1->uqname() << "_forany;\n";
  IND(s); s << ( !(tdef1->defined_in()==idl_global->root()) ?
		 "static " : "extern " )
	    << tdef1->uqname() << "_slice* "<< tdef1->uqname() << "_alloc() ";
  if (!(tdef1->defined_in()==idl_global->root())) {
    s << "{ return " << tdef2->unambiguous_name(tdef1) << "_alloc(); }\n";
  }
  else {
    s << ";\n";
  }
  IND(s); s << ( !(tdef1->defined_in()==idl_global->root()) ?
		 "static " : "extern " )
	    << tdef1->uqname() << "_slice* "<< tdef1->uqname() << "_dup(const "
	    << tdef1->uqname() << "_slice* p) ";
  if (!(tdef1->defined_in()==idl_global->root())) {
    s << "{ return " << tdef2->unambiguous_name(tdef1) 
      << "_dup(p); }\n";
  }
  else {
    s << ";\n";
  }
  IND(s); s << ( !(tdef1->defined_in()==idl_global->root()) ?
		 "static " : "extern " )
	    << "void " << tdef1->uqname() << "_free( "
	    << tdef1->uqname() << "_slice* p) ";
  if (!(tdef1->defined_in()==idl_global->root())) {
    s << "{ " << tdef2->unambiguous_name(tdef1) 
      << "_free(p); }\n\n";
  }
  else {
    s << ";\n\n";
  }
}


void
o2be_array::produce_typedef_skel(std::fstream &s, o2be_typedef* tdef1,
				 o2be_typedef* tdef2)
{
  if (tdef1->defined_in() == idl_global->root()) {
    IND(s); s << "extern " << tdef1->fqname() << "_slice* " 
	      << tdef1->fqname() << "_alloc() {\n";
    INC_INDENT_LEVEL();
    IND(s); s << "return " << tdef2->fqname() << "_alloc();\n";
    DEC_INDENT_LEVEL();
    IND(s); s << "}\n\n";

    IND(s); s << "extern " << tdef1->fqname() << "_slice* " 
	      << tdef1->fqname() << "_dup(const " << tdef1->fqname()
	      << "_slice* p) {\n";
    INC_INDENT_LEVEL();
    IND(s); s << "return " << tdef2->fqname() << "_dup(p);\n";
    DEC_INDENT_LEVEL();
    IND(s); s << "}\n\n";

    IND(s); s << "extern void " << tdef1->fqname() << "_free( " 
	      << tdef1->fqname() << "_slice* p) {\n";
    INC_INDENT_LEVEL();
    IND(s); s << tdef2->fqname() << "_free(p);\n";
    DEC_INDENT_LEVEL();
    IND(s); s << "}\n\n";
  }
}


// Used only by produce_union_member_decl & produce_struct_member_decl
void
o2be_array::_produce_member_decl (std::fstream &s, char* varname,
				  AST_Decl* used_in)
{
  const char* elm_fqname = element_name(used_in);

  IND(s); s << elm_fqname << " " << varname;
  {
    int nd = n_dims();
    AST_Expression **d = dims();
    int i;
    for (i=0; i < nd; i++)
      {
	s << "[";
	AST_Expression::AST_ExprValue* v = d[i]->ev();
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
	  default:
	    throw o2be_internal_error(__FILE__,__LINE__,"unexpected type in array expression");
	  }
	s << "]";
      }
  }	
  s << ";\n";
}

void
o2be_array::produce_struct_member_decl (std::fstream &s, AST_Decl* fieldtype,
					AST_Decl* used_in)
{
  _produce_member_decl(s,o2be_name::narrow_and_produce_uqname(fieldtype),
		       used_in);
}

void
o2be_array::produce_union_member_decl (std::fstream &s, AST_Decl* fieldtype,
				       AST_Decl* used_in)
{
  char* varname = new char[strlen("pd_") +
		  strlen(o2be_name::narrow_and_produce_uqname(fieldtype))+1];
  strcpy(varname,"pd_");
  strcat(varname,o2be_name::narrow_and_produce_uqname(fieldtype));
  _produce_member_decl(s,varname,used_in);
  delete [] varname;
}

void
o2be_array::produce_typedef_in_union(std::fstream &s, const char* tname,
				     AST_Decl* used_in)
{
  const char* elm_fqname = element_name(used_in);

  IND(s); s << "typedef " << elm_fqname << " _0RL" << tname;
  {
    int nd = n_dims();
    AST_Expression **d = dims();
    int i;
    for (i=0; i < nd; i++)
      {
	s << "[";
	AST_Expression::AST_ExprValue* v = d[i]->ev();
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
	  default:
	    throw o2be_internal_error(__FILE__,__LINE__,"unexpected type in array expression");
	  }
	s << "]";
      }
  }	
  s << ";\n";


  IND(s); s << "typedef " << elm_fqname << " "
	    << tname << "_slice";
  {
    int nd = n_dims();
    AST_Expression **d = dims();
    int i;
    for (i=1; i < nd; i++)
      {
	s << "[";
	AST_Expression::AST_ExprValue* v = d[i]->ev();
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
	  default:
	    throw o2be_internal_error(__FILE__,__LINE__,"unexpected type in array expression");
	  }
	s << "]";
      }
  }	
  s << ";\n\n";
}


void
o2be_array::produce_buildDesc_support(std::fstream& s)
{
  // Array types are not distinguishable from one another if
  // their base type and dimensions are the same. Thus each of
  // the following tcDescriptor helper functions must only be
  // generated once for an equivalence class of array types.
  // This includes the array type's slice and sub-slices if
  // the array is multi-dimensional, and further also the
  // element type's slices and sub-slices if the element type
  // is itself an array.
  //  An equivalent array type may also be generated in another
  // idl source file - so the buildDesc support needs to be
  // generated wherever it is needed, and declared <static>
  // to prevent clashes at link time.
  //  To protect again multiple definitions of equivalent
  // functions in this source file we form a canonical name
  // for the functions which depends only on the basic element
  // type and sizes of dimensions. Each helper function
  // is protected by a #ifndef, #define, #endif construct to
  // ensure that there is no clash if an equivalent helper
  // has been defined already.

  // Ensure we only generate the code for this array declaration
  // once in this source file.
  if( pd_have_produced_tcParser_buildDesc_code )
    return;

  // Get the element type. Recurses through sub-arrays to get
  // the basic element.
  AST_Decl* element_type = getElementType();

  // Get the element type name. This gets the fully qualified name of the
  // type that this array is made from.
  const char* element_fqname = element_name(o2be_global::root());

  // The actual element type of the array. We recurse through
  // subarray types to get at the basic element type. This
  // is used to produce the canonical names.
  const char* basic_element__idname =
    o2be_name::narrow_and_produce__idname(element_type);

  // Similar - but produces the type of the basic element.
  const char* basic_element_fqname =
    member_name(element_type, o2be_global::root());

  const char* canon_name = canonical_name();

  // If the base_type() is not defined in this file, we
  // need to declare the buildDesc helper for it.
  o2be_buildDesc::produce_decls(s, base_type());

  s << '\n';

  // First we need a helper function to get a tcDescriptor for
  // an element of each dimension of the array.

  for( size_t dim = n_dims(); dim > 0; dim-- ) {
    // Generate the guard.
    s << "#ifndef __0RL_tcParser_getElementDesc";
    produce_indicies(this, s, dim, getNumOfDims(), PIM_NAME);
    s << o2be_name::narrow_and_produce_canonical_name(element_type)
      << "__\n";
    s << "#define __0RL_tcParser_getElementDesc";
    produce_indicies(this, s, dim, getNumOfDims(), PIM_NAME);
    s << o2be_name::narrow_and_produce_canonical_name(element_type)
      << "__\n";

    // static - to prevent clashes between similar functions
    // declared in multiple source files.
    IND(s); s << "static CORBA::Boolean\n";
    IND(s); s << "_0RL_tcParser_getElementDesc";
    produce_indicies(this, s, dim, getNumOfDims(), PIM_NAME);
    s << o2be_name::narrow_and_produce_canonical_name(element_type)
      << "(tcArrayDesc* _adesc, CORBA::ULong _index, "
      "tcDescriptor &_desc)\n";
    IND(s); s << "{\n";
    INC_INDENT_LEVEL();

    // Create a temporary reference to the array element required.
    IND(s); s << element_fqname << " (&_0RL_tmp)";
    produce_indicies(this, s, dim + 1, n_dims(), PIM_INDEX);
    s << " = (*((" << element_fqname << "(*)";
    produce_indicies(this, s, dim, n_dims(), PIM_INDEX);
    s << ")_adesc->opq_array))[_index];\n";

    // If this is the innermost array-slice (including slices of element
    // type of it is an array) then use the appropriate buildDesc fn.
    // If not then we use the getElementDesc function for the next slice
    // in.
    if( dim == getNumOfDims() ) {
      o2be_buildDesc::call_buildDesc(s, element_type, "_desc", "_0RL_tmp");
    } else {
      IND(s); s << "_desc.p_array.getElementDesc = "
		"_0RL_tcParser_getElementDesc";
      produce_indicies(this, s, dim + 1, getNumOfDims(), PIM_NAME);
      s << o2be_name::narrow_and_produce_canonical_name(element_type)
	<< ";\n";
      IND(s); s << "_desc.p_array.opq_array = &_0RL_tmp;\n";
    }

    IND(s); s << "return 1;\n";
    DEC_INDENT_LEVEL();
    IND(s); s << "}\n";
    s << "#endif\n\n";
  }

  // Now the actual buildDesc function for this type. This
  // also needs to be guarded with a canonical name, as an
  // equivalent function may have already been defined.

  // Generate the guard.
  s << "#ifndef __0RL_tcParser_buildDesc" << canon_name << "__\n";
  s << "#define __0RL_tcParser_buildDesc" << canon_name << "__\n";

  // We take a pointer to slice rather than reference to the actual
  // array type, as it is better supported by compilers. MSVC and
  // g++ particularly have problems with references to multi-dimensional
  // arrays.

  IND(s); s << "static void\n";
  IND(s); s << "_0RL_buildDesc" << canon_name << "(tcDescriptor& _desc, ";
  s << "const " << basic_element_fqname << "(*_data)";
  produce_indicies(this, s, 2, getNumOfDims(), PIM_INDEX);
  s << ")\n";
  IND(s); s << "{\n";
  INC_INDENT_LEVEL();
  IND(s); s << "_desc.p_array.getElementDesc = "
	    "_0RL_tcParser_getElementDesc";
  produce_indicies(this, s, 1, getNumOfDims(), PIM_NAME);
  s << o2be_name::narrow_and_produce_canonical_name(element_type) << ";\n";
  IND(s); s << "_desc.p_array.opq_array = (void*) _data;\n";
  DEC_INDENT_LEVEL();
  IND(s); s << "}\n";
  s << "#endif\n\n";

  pd_have_produced_tcParser_buildDesc_code = I_TRUE;
}


void
o2be_array::call_buildDesc(std::fstream& s, const char* newdesc,
			   const char* instance_name)
{
  // To get round a bug in MSVC we have to cast separately to the
  // non-const slice pointer, and then to the const slice pointer.

  IND(s); s << "_0RL_buildDesc" << canonical_name() << '(' << newdesc << ", "
	    << "(const " << member_name(getElementType(), o2be_global::root())
	    << "(*)";
  produce_indicies(this, s, 2, getNumOfDims(), PIM_INDEX);
  s << ")(" << member_name(getElementType(), o2be_global::root())
	    << "(*)";
  produce_indicies(this, s, 2, getNumOfDims(), PIM_INDEX);
  s << ")(" << instance_name << "));\n";
}


const char*
o2be_array::out_adptarg_name(o2be_typedef* tdef,AST_Decl* used_in) const
{
  const char* ubname;

  if (o2be_global::qflag()) {
    ubname = tdef->fqname();
  }
  else {
    ubname = tdef->unambiguous_name(used_in);
  }

  char* p = new char[strlen(ADPT_CLASS_TEMPLATE)+strlen("<, >")+
		     strlen(ubname) + strlen("_slice") +
		     strlen(ubname) + strlen("_var")+1];
  strcpy(p,ADPT_CLASS_TEMPLATE);
  strcat(p,"<");
  strcat(p,ubname);
  strcat(p,"_slice");
  strcat(p,",");
  strcat(p,ubname);
  strcat(p,"_var >");
  return p;  
}


IMPL_NARROW_METHODS1(o2be_array, AST_Array)
IMPL_NARROW_FROM_DECL(o2be_array)
