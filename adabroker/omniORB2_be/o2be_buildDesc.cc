// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_buildDesc.cc        Created on: 10/1998
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
//    Support for the implementation of Any.
//

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>


void
o2be_buildDesc::produce_decls(std::fstream& s, AST_Decl* decl,
			      idl_bool even_if_in_main_file)
{
  //?? We could palm this off into each decl object, and there ensure
  // that a declaration is only produced once per source file.

  while( decl->node_type() == AST_Decl::NT_typedef )
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();

  switch( decl->node_type() ) {

    // The following cases are already catered for in tcParser.h
  case AST_Decl::NT_pre_defined:
  case AST_Decl::NT_string:
    break;

    // Code for arrays and sequences is always generated where
    // needed, and declared static to prevent conflicts. Thus we
    // need to generate the actual buildDesc code and not just
    // a declaration here.
  case AST_Decl::NT_sequence:
    o2be_sequence::narrow_from_decl(decl)->produce_buildDesc_support(s);
    break;

  case AST_Decl::NT_array:
    o2be_array::narrow_from_decl(decl)->produce_buildDesc_support(s);
    break;

  case AST_Decl::NT_interface:
    o2be_interface::narrow_from_decl(decl)
      ->produce_buildDesc_decls(s, even_if_in_main_file);
    break;

  case AST_Decl::NT_interface_fwd:
    o2be_interface_fwd::narrow_from_decl(decl)
      ->produce_buildDesc_decls(s, even_if_in_main_file);
    break;

    // Otherwise just declare the buildDesc function, if it is not
    // defined in this source file.
  default:
    if( !decl->in_main_file() || even_if_in_main_file ) {
      s << "extern void _0RL_buildDesc"
	<< o2be_name::narrow_and_produce_canonical_name(decl)
	<< "(tcDescriptor &, const "
	<< o2be_name::
	narrow_and_produce_unambiguous_name(decl, o2be_global::root(), I_TRUE)
	<< " &);\n";
    }
    break;
  }
}


void
o2be_buildDesc::call_buildDesc(std::fstream& s, AST_Decl* decl,
			       const char* newdesc, const char* instance_name)
{
  while( decl->node_type() == AST_Decl::NT_typedef )
    decl = o2be_typedef::narrow_from_decl(decl)->base_type();

  const char* canon_name = o2be_name::narrow_and_produce_canonical_name(decl);

  switch( decl->node_type() ) {

    // Arrays present a nasty special case because of brain-dead compilers.
  case AST_Decl::NT_array:
    o2be_array::narrow_from_decl(decl)
      ->call_buildDesc(s, newdesc, instance_name);
    break;

  default:
    IND(s); s << "_0RL_buildDesc" << canon_name << '(' << newdesc << ", "
	      << instance_name << ");\n";
    break;
  }
}
