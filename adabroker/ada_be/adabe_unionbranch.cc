//--------------------------------------------------------------------------//
//                                                                          //
//                          ADABROKER COMPONENTS                            //
//                                                                          //
//                            A D A B R O K E R                             //
//                                                                          //
//                            $Revision: 1.3 $
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
#include <adabe.h>
#include <strstream>

////////////////////////////////////////////////////////////////////////
///////////////////////   produce_disc_value   /////////////////////////
////////////////////////////////////////////////////////////////////////
static string
produce_disc_value (AST_ConcreteType *t, AST_Expression *exp);
// just a forward declaration, see the end of this file


////////////////////////////////////////////////////////////////////////
///////////////////////    adabe_union_branch   ////////////////////////
////////////////////////////////////////////////////////////////////////
adabe_union_branch::adabe_union_branch (AST_UnionLabel * lab,
					AST_Type       * ft,
					UTL_ScopedName * n,
					UTL_StrList *p)
  : AST_Decl (AST_Decl::NT_union_branch, n, p),
    AST_Field (AST_Decl::NT_union_branch, ft, n, p),
    AST_UnionBranch (lab, ft, n, p),
    adabe_field (ft, n, p),
    adabe_name (AST_Decl::NT_union_branch, n, p)
{
}


////////////////////////////////////////////////////////////////////////
////////////////////////    produce_ads    /////////////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_union_branch::produce_ads (dep_list         & with,
				 string           & body,
				 string           & previous,
				 AST_ConcreteType * concrete)
  // this method produces the code of the ads file for a given union branch
{
  // adds a when clause
  body += "      when ";

  // makes difference between a "real" clause and a default one and
  // produces the when statement
  if (label ()->label_kind () != AST_UnionLabel::UL_default)
    {
      body += produce_disc_value (concrete, label ()->label_val ());
      body += " => \n   ";
    }
  else if (label ()->label_kind () == AST_UnionLabel::UL_default)
    {
      body += "others ";
      body += "=> \n   ";
    }
  
  // calls the produce_ads method one hte underlying field for the body of the
  // when statement
  adabe_field::produce_ads (with, body, previous);   
}


////////////////////////////////////////////////////////////////////////
////////////////////////   produce_stream_adb   ///////////////////////
////////////////////////////////////////////////////////////////////////
void
adabe_union_branch::produce_stream_adb (dep_list         & with,
					string           & marshall,
					string           & unmarshall,
					string           & align_size,
					AST_ConcreteType * concrete)
  // Produce code for a given union branch.  Produce simultaneously 3
  // functions code : marshall, unmarshall and align_size
{
  string tmp = "      when "; // local temporary string

  // First, produce the when statement.
  if (label ()->label_kind () != AST_UnionLabel::UL_default)
    {
      tmp += produce_disc_value (concrete, label ()->label_val ());
      tmp += " => ";
    }
  else if (label ()->label_kind () == AST_UnionLabel::UL_default)
    {
      tmp += "others ";
      tmp += "=> ";
    }

  // Then, create the body of the statement for the 3 functions :
  // marshall, unmarshall and align_size.
  marshall += tmp;
  marshall += "Marshall (A.";
  marshall += get_ada_local_name ();
  marshall += ", S);\n";
  
  unmarshall += tmp;
  unmarshall += "Unmarshall (Tmp.";
  unmarshall += get_ada_local_name ();
  unmarshall += ", S);   \n";
  
  align_size += tmp;
  align_size += "Tmp := Align_Size (A.";
  align_size += get_ada_local_name ();
  align_size += ", Tmp);\n";
}


////////////////////////////////////////////////////////////////////////
///////////////////////   produce_disc_value   /////////////////////////
////////////////////////////////////////////////////////////////////////
static string
produce_disc_value (AST_ConcreteType * t,
		    AST_Expression   * exp)
  // Producecode for the discrimination value of an union branch.
{
  char temp[10];   // local temporary string

  // First case : we do not have a enum type but a simple type.
  if (t->node_type () != AST_Decl::NT_enum)
    {
      // Get expression value as an AST_ExprValue object.
      AST_Expression::AST_ExprValue *v = exp->ev ();

      // For each possible type, produce a string corresponding to the
      // value.
      switch (v->et) 
	 {
	 case AST_Expression::EV_short:
	   sprintf (temp, "%d", v->u.sval);
	   break;
	 case AST_Expression::EV_ushort:
	   sprintf (temp, "%d", v->u.usval);
	   break;
	 case AST_Expression::EV_long:
	   sprintf (temp, "%ld", v->u.lval);
	   break;
	 case AST_Expression::EV_ulong:
	   sprintf (temp, "%ld", v->u.ulval);
	   break;
	 case AST_Expression::EV_bool:
	   return ((v->u.bval == 0) ? "False" : "True");
	 case AST_Expression::EV_char:        
	   sprintf (temp, "'%c'", v->u.cval);  
	   break;
	 default:
	   // never get here
	   throw adabe_internal_error
	     (__FILE__,__LINE__, "Unexpected union discriminant value");
	 }
    }
  else
    // If we get here, we have an enum type. Get enum value as an
    // adabe_enum_val object and get the name by the
    // get_ada_local_name function.
    {
      adabe_enum_val* v = adabe_enum_val::narrow_from_decl
	(adabe_enum::narrow_from_decl (t)->lookup_by_value (exp));
      return (v->get_ada_local_name ());
    }
  
  return temp;
}
IMPL_NARROW_METHODS1 (adabe_union_branch, AST_UnionBranch)
IMPL_NARROW_FROM_DECL (adabe_union_branch)
