//                          Package   : omniidl2
// o2be_predefined_type.cc  Created on: 9/8/96
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
//   omniORB2 BE for the class AST_PredefinedType
//

/*
  $Log: o2be_predefined_type.cc,v $
  Revision 1.1  1999/02/14 17:45:28  niebel
  Ajout des sources d'omniidl2 en vue de l'ajout de notre back end.

  Revision 1.8  1999/01/07 09:37:45  djr
  Changes to support new implementation of TypeCode.

  Revision 1.7  1998/08/13 22:44:20  sll
  Added pragma hdrstop to control pre-compile header if the compiler feature
  is available.

  Revision 1.6  1998/04/07 18:49:55  sll
  Use std::fstream instead of fstream.

// Revision 1.5  1998/01/27  16:48:06  ewc
// Added support for type Any and TypeCode
//
  Revision 1.4  1997/12/09 19:55:03  sll
  *** empty log message ***

// Revision 1.3  1997/05/06  14:04:04  sll
// Public release.
//
  */

#include <idl.hh>
#include <idl_extern.hh>
#include <o2be.h>

#ifdef HAS_pch
#pragma hdrstop
#endif


o2be_predefined_type::
o2be_predefined_type(AST_PredefinedType::PredefinedType t,
		     UTL_ScopedName *sn, UTL_StrList *p)
  : AST_PredefinedType(t, sn, p),
    AST_Decl(AST_Decl::NT_pre_defined, sn, p),
    o2be_name(AST_Decl::NT_pre_defined,sn,p),
    o2be_sequence_chain(AST_Decl::NT_pre_defined,sn,p)
{
  const char *name;
  const char *tckind;
  const char *tcname;

  switch(pt())
    {
    case AST_PredefinedType::PT_long:
      name = "Long";
      tckind = "CORBA::tk_long";
      tcname = "_tc_long";
      break;
    case AST_PredefinedType::PT_ulong:
      name = "ULong";
      tckind = "CORBA::tk_ulong";
      tcname = "_tc_ulong";
      break;
    case AST_PredefinedType::PT_short:
      name = "Short";
      tckind = "CORBA::tk_short";
      tcname = "_tc_short";
      break;
    case AST_PredefinedType::PT_ushort:
      name = "UShort";
      tckind = "CORBA::tk_ushort";
      tcname = "_tc_ushort";
      break;
    case AST_PredefinedType::PT_float:
      name = "Float";
      tckind = "CORBA::tk_float";
      tcname = "_tc_float";
     break;
    case AST_PredefinedType::PT_double:
      name = "Double";
      tckind = "CORBA::tk_double";
      tcname = "_tc_double";
      break;
    case AST_PredefinedType::PT_char:
      name = "Char";
      tckind = "CORBA::tk_char";
      tcname = "_tc_char";
      break;
    case AST_PredefinedType::PT_boolean:
      name = "Boolean";
      tckind = "CORBA::tk_boolean";
      tcname = "_tc_boolean";
      break;
    case AST_PredefinedType::PT_octet:
      name = "Octet";
      tckind = "CORBA::tk_octet";
      tcname = "_tc_octet";
      break;
    case AST_PredefinedType::PT_any:
      name = "Any";
      tckind = "CORBA::tk_any";
      tcname = "_tc_any";
      break;
    case AST_PredefinedType::PT_void:
      name = "<void>";
      tckind = "CORBA::tk_void";
      tcname = "_tc_void";
      break;
    case AST_PredefinedType::PT_TypeCode:
      name = "TypeCode_ptr";
      tckind = "CORBA::tk_TypeCode";
      tcname = "_tc_TypeCode";
      break;
    case AST_PredefinedType::PT_pseudo:
      name = "<pseudo>";
      tckind = "tk_<psuedo>";
      tcname = "_tc_<psuedo>";
      break;
    case AST_PredefinedType::PT_longlong:
      name = "<longlong>";
      tckind = "tk_<longlong>";
      tcname = "_tc_<longlong>";
      break;
    case AST_PredefinedType::PT_ulonglong:
      name = "<ulonglong>";
      tckind = "tk_<ulonglong>";
      tcname = "_tc_<ulonglong>";
      break;
    case AST_PredefinedType::PT_longdouble:
      name = "<longdouble>";
      tckind = "tk_<longdouble>";
      tcname = "_tc_<longdouble>";
      break;
    case AST_PredefinedType::PT_wchar:
      name = "<wchar>";
      tckind = "tk_<wchar>";
      tcname = "_tc_<wchar>";
      break;
    default:
      name = "<unknown>";
      tckind = "tk_<unknown>";
      tcname = "_tc_<unknown>";
      break;
    }

  char *n = new char [strlen((const char *)"CORBA::")+strlen(name)+1];
  strcpy(n,(const char *)"CORBA::");
  strcat(n,name);
  set_uqname(n);

  n = new char[strlen(uqname())+1];
  strcpy(n,uqname());
  set_fqname(n);

  n = new char[strlen((const char *)"CORBA_")+strlen(name)+1];
  strcpy(n,(const char *)"CORBA_");
  strcat(n,name);
  set__fqname(n);

  n = new char[strlen((const char *)"CORBA")+1];
  strcpy(n,(const char *)"CORBA");
  set_scopename(n);

  n = new char[strlen(scopename())+1];
  strcpy(n,scopename());
  set__scopename(n);

  pd_tckname = new char[strlen(tckind)+1];
  strcpy(pd_tckname,tckind);

  n = new char[strlen(tcname)+1];
  strcpy(n,tcname);
  set_tcname(n);

  n = new char [strlen((const char *)"CORBA::")+strlen(tcname)+1];
  strcpy(n,(const char *)"CORBA::");
  strcat(n,tcname);
  set_fqtcname(n);

  // *** What should this be???
  set__fqtcname("");

  return;
}


void
o2be_predefined_type::produce_typedef_hdr(std::fstream &s, o2be_typedef *tdef)
{
  switch(pt())
    {
    case AST_PredefinedType::PT_any:
      IND(s); s << "typedef " << fqname() << " " << tdef->uqname() << ";\n";
      IND(s); s << "typedef " << fqname() << "_var " << tdef->uqname()
		<< "_var;\n";
      break;

    case AST_PredefinedType::PT_TypeCode:
      IND(s); s << "typedef CORBA::TypeCode_ptr " << tdef->uqname()
		<< "_ptr;\n";
      IND(s); s << "typedef CORBA::TypeCode_var " << tdef->uqname()
		<< "_var;\n";
      break;
	
    default:
      IND(s); s << "typedef " << fqname() << " " << tdef->uqname() << ";\n";
      break;
    }
}


const char*
o2be_predefined_type::fieldMemberTypeName()
{
  switch(pt())
    {
    case AST_PredefinedType::PT_TypeCode:
      return o2be_predefined_type::TypeCodeMemberName();      
    default:
      return uqname();
    }
}


const char*
o2be_predefined_type::TypeCodeMemberName()
{
  return "CORBA::TypeCode_member";
}


void
o2be_predefined_type::produce_typecode_member(std::fstream &s)
{
  const char* str;
  switch( pt() ) {
  case AST_PredefinedType::PT_long:        str = "long";        break;
  case AST_PredefinedType::PT_ulong:       str = "ulong";       break;
  case AST_PredefinedType::PT_longlong:    str = "longlong";    break;
  case AST_PredefinedType::PT_ulonglong:   str = "ulonglong";   break;
  case AST_PredefinedType::PT_short:       str = "short";       break;
  case AST_PredefinedType::PT_ushort:      str = "ushort";      break;
  case AST_PredefinedType::PT_float:       str = "float";       break;
  case AST_PredefinedType::PT_double:      str = "double";      break;
  case AST_PredefinedType::PT_longdouble:  str = "longdouble";  break;
  case AST_PredefinedType::PT_char:        str = "char";        break;
  case AST_PredefinedType::PT_wchar:       str = "wchar";       break;
  case AST_PredefinedType::PT_boolean:     str = "boolean";     break;
  case AST_PredefinedType::PT_octet:       str = "octet";       break;
  case AST_PredefinedType::PT_any:         str = "any";         break;
  case AST_PredefinedType::PT_void:        str = "void";        break;
  case AST_PredefinedType::PT_pseudo:      str = "pseudo";      break;
  case AST_PredefinedType::PT_TypeCode:    str = "TypeCode";    break;
  }

  s << "CORBA::TypeCode::PR_" << str << "_tc()";
}


// Narrowing
IMPL_NARROW_METHODS1(o2be_predefined_type, AST_PredefinedType)
IMPL_NARROW_FROM_DECL(o2be_predefined_type)
