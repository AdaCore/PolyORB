//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.1 $
//                                                                          //
//         Copyright (C) 1999-2000 ENST Paris University, France.           //
//                                                                          //
// AdaBroker is free software; you  can  redistribute  it and/or modify it  //
// under terms of the  GNU General Public License as published by the  Free //
// Software Foundation;  either version 2,  or (at your option)  any  later //
// version. AdaBroker  is distributed  in the hope that it will be  useful, //
// but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- //
// TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public //
// License  for more details.  You should have received  a copy of the GNU  //
// General Public License distributed with AdaBroker; see file COPYING. If  //
// not, write to the Free Software Foundation, 59 Temple Place - Suite 330, //
// Boston, MA 02111-1307, USA.                                              //
//                                                                          //
// As a special exception,  if other files  instantiate  generics from this //
// unit, or you link  this unit with other files  to produce an executable, //
// this  unit  does not  by itself cause  the resulting  executable  to  be //
// covered  by the  GNU  General  Public  License.  This exception does not //
// however invalidate  any other reasons why  the executable file  might be //
// covered by the  GNU Public License.                                      //
//                                                                          //
//             AdaBroker is maintained by ENST Paris University.            //
//                     (email: broker@inf.enst.fr)                          //
//                                                                          //
//--------------------------------------------------------------------------//
#include  <adabe.h>


AST_Root  *
adabe_generator::create_root (UTL_ScopedName *n, UTL_StrList *p)
{
  return (AST_Root *) new adabe_root (n, p);
}

AST_PredefinedType *
adabe_generator::create_predefined_type (AST_PredefinedType::PredefinedType t,
				     UTL_ScopedName *n,
				     UTL_StrList *p)
{
  return (AST_PredefinedType *) new adabe_predefined_type (t, n, p);
}

AST_Module *
adabe_generator::create_module (UTL_ScopedName *n, UTL_StrList *p)
{
  return (AST_Module *) new adabe_module (n, p);
}

AST_Interface *
adabe_generator::create_interface (UTL_ScopedName *n,
			       AST_Interface **ih,
			       long nih,
			       UTL_StrList *p)
{
  return (AST_Interface *) new adabe_interface (n, ih, nih, p);
}

AST_InterfaceFwd *
adabe_generator::create_interface_fwd (UTL_ScopedName *n, UTL_StrList *p)
{
  return (AST_InterfaceFwd *) new adabe_interface_fwd (n, p);
}

AST_Exception *
adabe_generator::create_exception (UTL_ScopedName *n, UTL_StrList *p)
{
  return (AST_Exception *) new adabe_exception (n, p);
}

AST_Structure *
adabe_generator::create_structure (UTL_ScopedName *n, UTL_StrList *p)
{
  return (AST_Structure *) new adabe_structure (n, p);
}

AST_Enum *
adabe_generator::create_enum (UTL_ScopedName *n, UTL_StrList *p)
{
  return (AST_Enum *) new adabe_enum (n, p);
}

AST_Operation *
adabe_generator::create_operation (AST_Type *rt,
			       AST_Operation::Flags fl,
			       UTL_ScopedName *n,
			       UTL_StrList *p)
{
  return (AST_Operation *) new adabe_operation (rt, fl, n, p);
}

AST_Field *
adabe_generator::create_field (AST_Type *ft, UTL_ScopedName *n, UTL_StrList *p)
{
  return (AST_Field *) new adabe_field (ft, n, p);
}

AST_Argument *
adabe_generator::create_argument (AST_Argument::Direction d,
			      AST_Type *ft,
			      UTL_ScopedName *n,
			      UTL_StrList *p)
{
  return (AST_Argument *) new adabe_argument (d, ft, n, p);
}

AST_Attribute *
adabe_generator::create_attribute (idl_bool ro,
			       AST_Type *ft,
			       UTL_ScopedName *n,
			       UTL_StrList *p)
{
  return (AST_Attribute *) new adabe_attribute (ro, ft, n, p);
}

AST_Union *
adabe_generator::create_union (AST_ConcreteType *dt,
			   UTL_ScopedName *n,
			   UTL_StrList *p)
{
  return (AST_Union *) new adabe_union (dt, n, p);
}

AST_UnionBranch *
adabe_generator::create_union_branch (AST_UnionLabel *lab,
				  AST_Type *ft,
				  UTL_ScopedName *n,
				  UTL_StrList *p)
{
  return (AST_UnionBranch *) new adabe_union_branch (lab, ft, n, p);
}

AST_Constant *
adabe_generator::create_constant (AST_Expression::ExprType et,
			      AST_Expression *ev,
			      UTL_ScopedName *n,
			      UTL_StrList *p)
{
  return (AST_Constant *) new adabe_constant (et, ev, n, p);
}

AST_EnumVal *
adabe_generator::create_enum_val (unsigned long v,
			      UTL_ScopedName *n,
			      UTL_StrList *p)
{
  return (AST_EnumVal *) new adabe_enum_val (v, n, p);
}

AST_Array *
adabe_generator::create_array (UTL_ScopedName *n,
			   unsigned long ndims,
			   UTL_ExprList *dims)
{
  return (AST_Array *) new adabe_array (n, ndims, dims);
}

AST_Sequence *
adabe_generator::create_sequence (AST_Expression *v, AST_Type *bt)
{
  return (AST_Sequence *) new adabe_sequence (v, bt);
}

AST_String *
adabe_generator::create_string (AST_Expression *v)
{
  return (AST_String *) new adabe_string (v);
}
/*
  AST_String *
  adabe_generator::create_wstring (AST_Expression *v)
  {
  return (AST_String *) new adabe_string (v, sizeof (wchar_t));
  }
*/

AST_Typedef *
adabe_generator::create_typedef (AST_Type *bt, UTL_ScopedName *n, UTL_StrList *p)
{
  return (AST_Typedef *) new adabe_typedef (bt, n, p);
}








