// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_name_mangle.cc      Created on: 12/1998
//			    Author    : David Riddoch (djr)
//
//    Copyright (C) 1996-1999 Olivetti & Oracle Research Laboratory
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

//
// This is the grammar used for name mangling in omniidl2.
//
//  {...} - arbitrary repitition
//  [...] - optional
//  "..." - literal
//  |     - alternation
//
// The terminal symbols are:
//   basic     - a type which can be distinguished by a c++ compiler
//   dimension - an integer giving the dimension of the array
//   bound     - an integer giving the bound on a sequence (0 for unbounded)
//
//
// scope ::= basic "_m"
//
// idname ::= {basic "_m"} basic           ; unique name for identifier
//
// array ::= "_a" dimension
//
// sequence ::= "_s" bound
//
// cannon_type_name ::= {array | sequence} "_c" cannon_type_name
//                                         ; canonical name for a type
//
// return_type ::= cannon_type_name
//
// argument ::= "_i" cannon_type_name      ; in
//            | "_n" cannon_type_name      ; inout
//            | "_o" cannon_type_name      ; out
//
// throws ::= "_e" cannon_type_name
//
// op_signature ::= ["_w"] return_type {argument} {throws}
//



#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif


#define STRING_BUF_INC  32


class StringBuf {
public:
  inline StringBuf(size_t size = STRING_BUF_INC) {
    pd_current = pd_start = new char[size];
    pd_end = pd_start + size;
    *pd_current = '\0';
  }

  inline operator char* () { return pd_start; }

  inline void operator += (const char* s) {
    size_t len = strlen(s);
    if( len > free_space() )  reserve(len);
    strcpy(pd_current, s);
    pd_current += len;
  }

  inline void operator += (char c) {
    if( free_space() < 1 )  reserve(1);
    *pd_current++ = c;
    *pd_current = '\0';
  }

  void reserve(size_t n);

private:
  inline size_t free_space() { return pd_end - pd_current - 1; }

  char* pd_start;
  char* pd_current;
  char* pd_end;
};


void
StringBuf::reserve(size_t n)
{
  size_t extra = STRING_BUF_INC;
  while( extra + free_space() < n )
    extra += STRING_BUF_INC;
  size_t newsize = (pd_end - pd_start) + extra;
  char* newptr = new char[newsize];
  strcpy(newptr, pd_start);
  pd_current = newptr + (pd_current - pd_start);
  pd_end = newptr + newsize;
  pd_start = newptr;
}


#define SCOPE_SEPARATOR             "_m"
#define ARRAY_SEPARATOR             "_a"
#define SEQ_SEPARATOR               "_s"
#define CANNON_NAME_SEPARATOR       "_c"
#define ONEWAY_SEPARATOR            "_w"
#define IN_SEPARATOR                "_i"
#define OUT_SEPARATOR               "_o"
#define INOUT_SEPARATOR             "_n"
#define EXCEPTION_SEPARATOR         "_e"


char*
o2be_name_mangler::produce_idname(UTL_ScopedName* n)
{
  StringBuf result;

  UTL_ScopedNameActiveIterator iter(n);
  Identifier* last = n->last_component();

  while( !iter.is_done() ) {
    Identifier* id = iter.item();
    char* q = id->get_string();

    if( strlen(q) != 0 ) {
      while( *q )
	if( *q == '_' )  { result += "__"; q++; }
	else             result += *q++;

      if( id != last )   result += SCOPE_SEPARATOR;
    }
    iter.next();
    id = iter.item();
  }

  return result;
}


char*
o2be_name_mangler::produce_canonical_name(AST_Decl* decl)
{
  StringBuf result;

  while(1) {

    while( decl->node_type() == AST_Decl::NT_typedef )
      decl = o2be_typedef::narrow_from_decl(decl)->base_type();

    switch( decl->node_type() ) {

    case AST_Decl::NT_array:
      {
	// we go through all dimensions of this array and sub-arrays
	o2be_array* array = o2be_array::narrow_from_decl(decl);
	o2be_array::dim_iterator dimit(array);
	size_t ndims = array->getNumOfDims();

	for( size_t i = 0; i < ndims; i++ ) {
	  size_t dim = dimit();
	  char strdim[20];
	  sprintf(strdim, "%d", (int)dim);

	  result += ARRAY_SEPARATOR;
	  result += strdim;
	}
	decl = array->getElementType();
	break;
      }

    case AST_Decl::NT_sequence:
      {
	o2be_sequence* seq = o2be_sequence::narrow_from_decl(decl);
	char strbound[20];
	sprintf(strbound, "%d", (int)seq->bound());

	result += SEQ_SEPARATOR;
	result += strbound;

	decl = seq->base_type();
	break;
      }

    default:
      {
	result += CANNON_NAME_SEPARATOR;
	result += o2be_name::narrow_and_produce__idname(decl);

	return result;
      }

    }
  }

  // dummy
  return 0;
}


// This is going to need some work to reduce the size of mangled
// operations signatures.
//  It is important to remember that these names will necassarily
// have global scope (as used for classes with virtual functions),
// so clashes must be highly improbable, or impossible.
//  If improbable, then must be sure that regenerating the file will
// produce a different output - so that they can just recompile to
// get rid of any nasty clashes.
//  We may want to supply from common call descriptors in the
// library (eg. void op() ), so it will also be necassary to make
// these names predictable.


char*
o2be_name_mangler::produce_operation_signature(o2be_operation& op)
{
  StringBuf op_sig;

  if( op.flags() == AST_Operation::OP_oneway )  op_sig += ONEWAY_SEPARATOR;

  if( op.return_is_void() )
    op_sig += "void";
  else
    op_sig += o2be_name::narrow_and_produce_canonical_name(op.return_type());

  {
    UTL_ScopeActiveIterator i(&op, UTL_Scope::IK_decls);

    while( !i.is_done() ) {

      o2be_argument* arg = o2be_argument::narrow_from_decl(i.item());

      switch( arg->direction() ) {
      case AST_Argument::dir_IN:
	op_sig += IN_SEPARATOR;
	break;
      case AST_Argument::dir_OUT:
	op_sig += OUT_SEPARATOR;
	break;
      case AST_Argument::dir_INOUT:
	op_sig += INOUT_SEPARATOR;
	break;
      }

      op_sig +=
	o2be_name::narrow_and_produce_canonical_name(arg->field_type());

      i.next();
    }
  }

  {
    //?? We really ought to sort the exceptions - so that
    //  void op1() throws(a, b)
    //  void op2() throws(b, a)
    // are identical ...

    UTL_ExceptlistActiveIterator i(op.exceptions());

    while( !i.is_done() ) {
      op_sig += EXCEPTION_SEPARATOR;
      op_sig += o2be_name::narrow_and_produce_canonical_name(i.item());

      i.next();
    }
  }

  return op_sig;
}


char*
o2be_name_mangler::produce_attribute_read_signature(o2be_attribute& attr)
{
  return o2be_name::narrow_and_produce_canonical_name(attr.field_type());
}


char*
o2be_name_mangler::produce_attribute_write_signature(o2be_attribute& attr)
{
  StringBuf op_sig;

  op_sig += "void";
  op_sig += IN_SEPARATOR;
  op_sig += o2be_name::narrow_and_produce_canonical_name(attr.field_type());

  return op_sig;
}
