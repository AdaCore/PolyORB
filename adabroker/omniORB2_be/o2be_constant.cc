// -*- Mode: C++; -*-
//                          Package   : omniidl2
// o2be_constant.cc         Created on: 12/08/1996
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
  $Log: o2be_constant.cc,v $
  Revision 1.1  1999/02/14 17:45:23  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.9  1999/01/07 09:48:01  djr
  *** empty log message ***

  Revision 1.8  1998/08/19 15:51:01  sll
  o2be_name::VarToken is now replaced by a better name
  o2be_name::variable_qualifier.

  Revision 1.7  1998/08/13 22:36:24  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available

  Revision 1.6  1998/08/10 16:55:08  sll
  Remove redundent quote ' from constant char definition.

  Revision 1.5  1998/04/09 19:14:31  sll
  For const integral type, specify the initializer in its declaration.

  Revision 1.4  1998/04/07 18:42:41  sll
  Use std::fstream instead of fstream.
  Stub code now contains workaround for MSVC++ to initialise constants properly.

  Revision 1.3  1997/12/09 19:55:24  sll
  *** empty log message ***

// Revision 1.2  1997/05/06  13:52:40  sll
// Public release.
//
  */

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

o2be_constant::o2be_constant(AST_Expression::ExprType et,
			 AST_Expression *v,
			 UTL_ScopedName *n,
			 UTL_StrList *p)
	   : AST_Constant(et, v, n, p),
	     AST_Decl(AST_Decl::NT_const, n, p),
	     o2be_name(AST_Decl::NT_const,n,p)
{
}

void
o2be_constant::produce_hdr(std::fstream& s)
{
  char *initializer = "_init_in_decl_( ";
  idl_bool intfconst = 0;

  if (defined_in()->scope_node_type()==AST_Decl::NT_interface) {
    initializer = "_init_in_cldecl_( ";
    intfconst = 1;
  }

  IND(s); s << variable_qualifier();
  AST_Expression::ExprType etype = et();
  switch (etype) {
  case AST_Expression::EV_short:
    s << (!intfconst?"INT":"") << " const CORBA::Short";
    break;
  case AST_Expression::EV_ushort:
    s << (!intfconst?"INT":"") << " const CORBA::UShort";
    break;
  case AST_Expression::EV_long:
    s << (!intfconst?"INT":"") << " const CORBA::Long";
    break;
  case AST_Expression::EV_ulong:
    s << (!intfconst?"INT":"") << " const CORBA::ULong";
    break;
  case AST_Expression::EV_float:
    initializer = 0;
    s << " const CORBA::Float";
    break;
  case AST_Expression::EV_double:
    initializer = 0;
    s << " const CORBA::Double";
    break;
  case AST_Expression::EV_char:
    s <<  (!intfconst?"INT":"") << " const CORBA::Char";
    break;
  case AST_Expression::EV_octet:
    s << (!intfconst?"INT":"") << " const CORBA::Octet";
    break;
  case AST_Expression::EV_bool:
    s << (!intfconst?"INT":"") << " const CORBA::Boolean";
    break;
  case AST_Expression::EV_string:
    initializer = 0;
    s << " const char *";
    break;
  default:
    throw o2be_internal_error(__FILE__,__LINE__,"unexpected type under constant class");
    break;
  }
  s << " " << uqname();
  if (initializer) {
    s << " " << initializer << " = ";
    constant_value()->dump(s);
    s << " )";
  }
  s << ";\n";
  return;
}


void
o2be_constant::produce_skel(std::fstream& s)
{
  char *quote = "";
  char *initializer = "_init_in_def_( ";
  char *typestr;

  if (defined_in()->scope_node_type()==AST_Decl::NT_interface) {
    initializer = "_init_in_cldef_( ";
  }

  AST_Expression::ExprType etype = et();
  switch (etype) {
  case AST_Expression::EV_short:
    typestr =  "const CORBA::Short";
    break;
  case AST_Expression::EV_ushort:
    typestr =  "const CORBA::UShort";
    break;
  case AST_Expression::EV_long:
    typestr =  "const CORBA::Long";
    break;
  case AST_Expression::EV_ulong:
    typestr =  "const CORBA::ULong";
    break;
  case AST_Expression::EV_float:
    initializer = 0;
    typestr = "const CORBA::Float";
    break;
  case AST_Expression::EV_double:
    initializer = 0;
    typestr = "const CORBA::Double";
    break;
  case AST_Expression::EV_char:
    typestr = "const CORBA::Char";
    break;
  case AST_Expression::EV_octet:
    typestr = "const CORBA::Octet";
    break;
  case AST_Expression::EV_bool:
    typestr = "const CORBA::Boolean";
    break;
  case AST_Expression::EV_string:
    initializer = 0;
    typestr = "const char *";
    quote = "\"";
    break;
  default:
    throw o2be_internal_error(__FILE__,__LINE__,"unexpected type under constant class");
    break;
  }

  if (defined_in()->scope_node_type()==AST_Decl::NT_interface) {
    IND(s); s << typestr << " " << fqname() << " "
	      << (initializer?initializer:"") << " = " << quote;
    constant_value()->dump(s);
    s << quote << (initializer?" )":"") << ";\n";
  }
  else if (defined_in() == idl_global->root()) {
    IND(s); s << (initializer?initializer:"") 
	      << typestr << " " << fqname() << " = " << quote;
    constant_value()->dump(s);
    s << quote << (initializer?"; )":";") << "\n";
  }
  else {
    if (initializer) {
      IND(s); s << (initializer?initializer:"") 
		<< typestr << " " << fqname() << " = " << quote;
      constant_value()->dump(s);
      s << quote << "; )\n";
    }
    else {
      s << "\n#if defined(HAS_Cplusplus_Namespace) && defined(_MSC_VER)\n";
      IND(s); s << "// MSVC++ does not give the constant external linkage othewise.\n";
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

      IND(s); s << "extern " << typestr << " " << uqname() << "="
		<< ((quote != NULL) ? quote : "");
      constant_value()->dump(s);
      s << ((quote != NULL) ? quote : "") << ";\n";


      DEC_INDENT_LEVEL();
      IND(s); s << "}\n";
      s << "#else\n";
      IND(s); s << (initializer?initializer:"") 
		<< typestr << " " << fqname() << " = " << quote;
      constant_value()->dump(s);
      s << quote << (initializer?"; )":";") << "\n";
      s << "#endif\n";
    }
  }
}


void
o2be_constant::produce_dynskel(std::fstream& s)
{
}


// Narrowing
IMPL_NARROW_METHODS1(o2be_constant, AST_Constant)
IMPL_NARROW_FROM_DECL(o2be_constant)
