//                          Package   : omniidl2
// o2be_module.cc           Created on: 8/8/1996
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
   $Log: o2be_module.cc,v $
   Revision 1.1  1999/02/14 17:45:26  niebel
   Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

   Revision 1.11  1999/01/07 09:45:58  djr
   Changes for new output file ...DynSK.cc

   Revision 1.10  1998/08/19 15:53:06  sll
   New member functions void produce_binary_operators_in_hdr and the like
   are responsible for generating binary operators <<= etc in the global
   namespace.

   Revision 1.9  1998/08/13 22:38:08  sll
   Added pragma hdrstop to control pre-compile header if the compiler feature
   is available.

   Revision 1.8  1998/05/20 18:23:50  sll
   New option (-t) enable the generation of tie implementation template.

   Revision 1.7  1998/04/09 19:15:21  sll
   Added extra newlines to make the stub more readable.

   Revision 1.6  1998/04/07 18:48:29  sll
   Stub code modified to accommodate the use of namespace to represent module.
   Use std::fstream instead of fstream.

   Revision 1.5  1997/12/09 19:55:33  sll
   *** empty log message ***

// Revision 1.4  1997/05/06  13:59:35  sll
// Public release.
//
   */

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif

o2be_module::o2be_module(UTL_ScopedName *n, UTL_StrList *p)
                 : AST_Decl(AST_Decl::NT_module, n, p),
		   UTL_Scope(AST_Decl::NT_module),
                   o2be_name(AST_Decl::NT_module,n,p) 
{
}


void
o2be_module::produce_hdr(std::fstream &s)
{
  if (!(in_main_file()))
    return;

  if (defined_in() != 0)
    {
      s << "\n";
      IND(s); s << "_CORBA_MODULE " << uqname() << "\n\n";
      IND(s); s << "_CORBA_MODULE_BEG\n\n";
      INC_INDENT_LEVEL();
    }

  UTL_ScopeActiveIterator  i(this,UTL_Scope::IK_decls);
  AST_Decl                 *decl;

  while (!(i.is_done()))
    {
      decl = i.item();
      if ((decl->in_main_file()))
	{
	  switch(decl->node_type()) {
	  case AST_Decl::NT_module:
	    o2be_module::narrow_from_decl(decl)->produce_hdr(s); 
	    break;
	  case AST_Decl::NT_interface:
	    o2be_interface::narrow_from_decl(decl)->produce_hdr(s);
	    break;
	  case AST_Decl::NT_except:
	    o2be_exception::narrow_from_decl(decl)->produce_hdr(s);
	    break;
	  case AST_Decl::NT_struct:
	    o2be_structure::narrow_from_decl(decl)->produce_hdr(s);
	    break;
	  case AST_Decl::NT_typedef:
	    o2be_typedef::narrow_from_decl(decl)->produce_hdr(s);
	    break;
	  case AST_Decl::NT_union:
	    o2be_union::narrow_from_decl(decl)->produce_hdr(s);
	    break;
	  case AST_Decl::NT_const:
	    o2be_constant::narrow_from_decl(decl)->produce_hdr(s);
	    break;
	  case AST_Decl::NT_interface_fwd:
	    o2be_interface_fwd::narrow_from_decl(decl)->produce_hdr(s);
	    break;
	  case AST_Decl::NT_enum:
	    o2be_enum::narrow_from_decl(decl)->produce_hdr(s);
	    break;
	  case AST_Decl::NT_enum_val:
	    break;
	  default:
	    throw o2be_internal_error(__FILE__, __LINE__,
				      "Unrecognised node type");
	  }
	}
      i.next();
    }

  if (defined_in() != 0)
    {
      DEC_INDENT_LEVEL();
      s << "\n";
      IND(s); s << "_CORBA_MODULE_END\n\n";
    }
}


void
o2be_module::produce_skel(std::fstream &s)
{
  if (!(in_main_file()))
    return;

  UTL_ScopeActiveIterator  i(this,UTL_Scope::IK_decls);
  AST_Decl                 *decl;

  while (!(i.is_done()))
    {
      decl = i.item();
      if ((decl->in_main_file()))
	{
	  switch(decl->node_type()) {
	  case AST_Decl::NT_module:
	    o2be_module::narrow_from_decl(decl)->produce_skel(s); 
	    break;
	  case AST_Decl::NT_interface:
	    o2be_interface::narrow_from_decl(decl)->produce_skel(s);
	    break;
	  case AST_Decl::NT_except:
	    o2be_exception::narrow_from_decl(decl)->produce_skel(s);
	    break;
	  case AST_Decl::NT_struct:
	    o2be_structure::narrow_from_decl(decl)->produce_skel(s);
	    break;
	  case AST_Decl::NT_typedef:
	    o2be_typedef::narrow_from_decl(decl)->produce_skel(s);
	    break;
	  case AST_Decl::NT_union:
	    o2be_union::narrow_from_decl(decl)->produce_skel(s);
	    break;
	  case AST_Decl::NT_const:
	    o2be_constant::narrow_from_decl(decl)->produce_skel(s);
	    break;
	  case AST_Decl::NT_interface_fwd:
	    o2be_interface_fwd::narrow_from_decl(decl)->produce_skel(s);
	    break;
	  case AST_Decl::NT_enum:
	    o2be_enum::narrow_from_decl(decl)->produce_skel(s);
	    break;
	  case AST_Decl::NT_enum_val:
	    break;
	  default:
	    throw o2be_internal_error(__FILE__,__LINE__,
				      "Unrecognised node type");
	  }
	}
      i.next();
    }
  return;
}


void
o2be_module::produce_dynskel(std::fstream &s)
{
  if (!(in_main_file()))
    return;

  UTL_ScopeActiveIterator  i(this,UTL_Scope::IK_decls);
  AST_Decl                 *decl;

  while (!(i.is_done()))
    {
      decl = i.item();
      if ((decl->in_main_file()))
	{
	  switch(decl->node_type()) {
	  case AST_Decl::NT_module:
	    o2be_module::narrow_from_decl(decl)->produce_dynskel(s); 
	    break;
	  case AST_Decl::NT_interface:
	    o2be_interface::narrow_from_decl(decl)->produce_dynskel(s);
	    break;
	  case AST_Decl::NT_except:
	    o2be_exception::narrow_from_decl(decl)->produce_dynskel(s);
	    break;
	  case AST_Decl::NT_struct:
	    o2be_structure::narrow_from_decl(decl)->produce_dynskel(s);
	    break;
	  case AST_Decl::NT_typedef:
	    o2be_typedef::narrow_from_decl(decl)->produce_dynskel(s);
	    break;
	  case AST_Decl::NT_union:
	    o2be_union::narrow_from_decl(decl)->produce_dynskel(s);
	    break;
	  case AST_Decl::NT_const:
	    o2be_constant::narrow_from_decl(decl)->produce_dynskel(s);
	    break;
	  case AST_Decl::NT_interface_fwd:
	    o2be_interface_fwd::narrow_from_decl(decl)->produce_dynskel(s);
	    break;
	  case AST_Decl::NT_enum:
	    o2be_enum::narrow_from_decl(decl)->produce_dynskel(s);
	    break;
	  case AST_Decl::NT_enum_val:
	    break;
	  default:
	    throw o2be_internal_error(__FILE__,__LINE__,
				      "Unrecognised node type");
	  }
	}
      i.next();
    }
  return;
}


void
o2be_module::produce_decls_at_global_scope_in_hdr(std::fstream& s)
{
  if( !(in_main_file()) )  return;

  UTL_ScopeActiveIterator  i(this,UTL_Scope::IK_decls);
  AST_Decl*                decl;

  while( !i.is_done() ) {

    decl = i.item();
    i.next();

    if ((decl->in_main_file())) {

      switch(decl->node_type()) {
      case AST_Decl::NT_module:
	o2be_module::
	  narrow_from_decl(decl)->produce_decls_at_global_scope_in_hdr(s); 
	break;
      case AST_Decl::NT_union:
	o2be_union::
	  narrow_from_decl(decl)->produce_decls_at_global_scope_in_hdr(s);
	break;
      case AST_Decl::NT_struct:
	o2be_structure::
	  narrow_from_decl(decl)->produce_decls_at_global_scope_in_hdr(s);
	break;
      default:
	break;
      }

    }
  }
}


void
o2be_module::produce_binary_operators_in_hdr(std::fstream &s)
{
  if (!(in_main_file()))
    return;

  UTL_ScopeActiveIterator  i(this,UTL_Scope::IK_decls);
  AST_Decl                 *decl;

  while (!(i.is_done()))
    {
      decl = i.item();
      if ((decl->in_main_file()))
	{
	  switch(decl->node_type()) {
	  case AST_Decl::NT_module:
	    o2be_module::
	      narrow_from_decl(decl)->produce_binary_operators_in_hdr(s); 
	    break;
	  case AST_Decl::NT_interface:
	    o2be_interface::
	      narrow_from_decl(decl)->produce_binary_operators_in_hdr(s);
	    break;
	  case AST_Decl::NT_except:
	    o2be_exception::
	      narrow_from_decl(decl)->produce_binary_operators_in_hdr(s);
	    break;
	  case AST_Decl::NT_struct:
	    o2be_structure::
	      narrow_from_decl(decl)->produce_binary_operators_in_hdr(s);
	    break;
	  case AST_Decl::NT_typedef:
	    o2be_typedef::
	      narrow_from_decl(decl)->produce_binary_operators_in_hdr(s);
	    break;
	  case AST_Decl::NT_union:
	    o2be_union::
	      narrow_from_decl(decl)->produce_binary_operators_in_hdr(s);
	    break;
	  case AST_Decl::NT_enum:
	    o2be_enum::
	      narrow_from_decl(decl)->produce_binary_operators_in_hdr(s);
	    break;
	  default:
	    break;
	  }
	}
      i.next();
    }
}

void
o2be_module::produce_binary_operators_in_dynskel(std::fstream &s)
{
  if (!(in_main_file()))
    return;

  UTL_ScopeActiveIterator  i(this,UTL_Scope::IK_decls);
  AST_Decl                 *decl;

  while (!(i.is_done()))
    {
      decl = i.item();
      if ((decl->in_main_file()))
	{
	  switch(decl->node_type()) {
	  case AST_Decl::NT_module:
	    o2be_module::
	      narrow_from_decl(decl)->produce_binary_operators_in_dynskel(s); 
	    break;
	  case AST_Decl::NT_interface:
	    o2be_interface::
	      narrow_from_decl(decl)->produce_binary_operators_in_dynskel(s);
	    break;
	  case AST_Decl::NT_except:
	    o2be_exception::
	      narrow_from_decl(decl)->produce_binary_operators_in_dynskel(s);
	    break;
	  case AST_Decl::NT_struct:
	    o2be_structure::
	      narrow_from_decl(decl)->produce_binary_operators_in_dynskel(s);
	    break;
	  case AST_Decl::NT_typedef:
	    o2be_typedef::
	      narrow_from_decl(decl)->produce_binary_operators_in_dynskel(s);
	    break;
	  case AST_Decl::NT_union:
	    o2be_union::
	      narrow_from_decl(decl)->produce_binary_operators_in_dynskel(s);
	    break;
	  case AST_Decl::NT_enum:
	    o2be_enum::
	      narrow_from_decl(decl)->produce_binary_operators_in_dynskel(s);
	    break;
	  default:
	    break;
	  }
	}
      i.next();
    }
}


void
o2be_module::produce_tie_templates(std::fstream &s)
{
  if (!(in_main_file()))
    return;

  UTL_ScopeActiveIterator  i(this,UTL_Scope::IK_decls);
  AST_Decl                 *decl;

  while (!(i.is_done()))
    {
      decl = i.item();
      if ((decl->in_main_file()))
	{
	  switch(decl->node_type()) {
	  case AST_Decl::NT_module:
	    o2be_module::narrow_from_decl(decl)->produce_tie_templates(s); 
	    break;
	  case AST_Decl::NT_interface:
	    o2be_interface::narrow_from_decl(decl)->produce_tie_templates(s);
	    break;
	  default:
	    break;
	  }
	}
      i.next();
    }
}


// Narrowing
IMPL_NARROW_METHODS1(o2be_module, AST_Module)
IMPL_NARROW_FROM_DECL(o2be_module)
IMPL_NARROW_FROM_SCOPE(o2be_module)

