/*************************************************************************************************
***                              ADA BACK-END COMPILER                                         ***
***                             file:  adabe_union_branch                                      ***
***                                                                                            ***
***      This file provides the implementation of class adabe_union_branch declared in adabe.h ***
***   (L 319). This class is the correspondant of the Sun's Front-End class AST_UnionBranch.   ***
***   It provides produce functions for spec of the main file, and for the marshall file.      ***
***                                                                                            ***
***   Copyright 1999                                                                           ***
***   Jean Marie Cottin, Laurent Kubler, Vincent Niebel                                        ***
***                                                                                            ***
***   This is free software; you can redistribute it and/or modify it under terms of the GNU   ***
***   General Public License, as published by the Free Software Foundation.                    ***
***                                                                                            ***
***  This back-end is distributed in the hope that it will be usefull, but WITHOUT ANY         ***
***  WARRANTY; without even the implied waranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR ***
***  PURPOSE.                                                                                  ***
***                                                                                            ***
***  See the GNU General Public License for more details.                                      ***
***                                                                                            ***
***                                                                                            ***
*************************************************************************************************/
#include <adabe.h>
#include <strstream>


// produce_disc_value
//-------------------
static string
produce_disc_value(AST_ConcreteType *t, AST_Expression *exp);
// just a forward declaration, see the end of this file


// adabe_union_branch
//-------------------
adabe_union_branch::adabe_union_branch(AST_UnionLabel *lab, AST_Type *ft, UTL_ScopedName *n,
		  UTL_StrList *p)
                    : AST_Decl(AST_Decl::NT_union_branch, n, p),
                      AST_Field(AST_Decl::NT_union_branch, ft, n, p),
                      AST_UnionBranch(lab, ft, n, p),
		      adabe_field(ft,n,p),
		      adabe_name(AST_Decl::NT_union_branch, n, p)
{
}


// produce_ads
//------------
void
adabe_union_branch::produce_ads(dep_list& with, string &body, string &previous, AST_ConcreteType *concrete)
  // this method produces the code of the ads file for a given union branch
{
  // adds a when clause
  body += "      when ";

  // makes difference between a "real" clause and a default one and
  // produces the when statement
  if (label()->label_kind() != AST_UnionLabel::UL_default)
    {
      body += produce_disc_value(concrete, label()->label_val());
      body += " => \n      ";
    }
  else if (label()->label_kind() == AST_UnionLabel::UL_default)
    {
      body += "others ";
      body += "=> \n      ";
    }
  
  // calls the produce_ads method one hte underlying field for the body of the
  // when statement
  adabe_field::produce_ads(with, body, previous);   
}


// produce_marshal_adb
//--------------------
void
adabe_union_branch::produce_marshal_adb (dep_list& with,
					 string &marshall,
					 string &unmarshall,
					 string &align_size,
					 AST_ConcreteType *concrete)
  // this method produces the code of the marshal.adb file for a given union branch
  // it produces simultaneously 3 functions code : marshall, unmarshall and align_size
{
  string tmp = "         when "; // local temporary string

  // first produce the when statement, differencing default and others
  if (label()->label_kind() != AST_UnionLabel::UL_default)
    {
      tmp += produce_disc_value(concrete, label()->label_val());
      tmp += " => ";
    }
  else if (label()->label_kind() == AST_UnionLabel::UL_default)
    {
      tmp += "others ";
      tmp += "=> ";
    }

  // then create the body of the statement for the 3 functions : marshall,
  // unmarshall and align_size
  marshall += tmp;
  marshall += "Marshall (A.";
  marshall += get_ada_local_name ();
  marshall += ",S) ;\n";
  
  unmarshall += tmp;
  unmarshall += "UnMarshall (Tmp.";
  unmarshall += get_ada_local_name ();
  unmarshall += ",S) ;   \n";
  
  align_size += tmp;
  align_size += "Tmp := Align_Size (A.";
  align_size += get_ada_local_name ();
  align_size += ",Tmp) ;\n";
}


// produce_disc_value
//-------------------
static string
produce_disc_value( AST_ConcreteType* t,AST_Expression* exp)
  // this method produces a string corresponding to the discrimination value
  // of this union branch
{
  char temp[10];   // local temporary string

  // first case : we do not have a enum type but a simple type
  if (t->node_type() != AST_Decl::NT_enum)
    {
      // gets the the expression value as an AST_ExprValue object
      AST_Expression::AST_ExprValue *v = exp->ev();

      // for each possible type, produce the string corresponding to the value
      switch (v->et) 
	 {
	 case AST_Expression::EV_short:
	   sprintf(temp, "%d",v->u.sval);
	   break;
	 case AST_Expression::EV_ushort:
	   sprintf(temp, "%d",v->u.usval);
	   break;
	 case AST_Expression::EV_long:
	   sprintf(temp, "%ld",v->u.lval);
	   break;
	 case AST_Expression::EV_ulong:
	   sprintf(temp, "%ld",v->u.ulval);
	   break;
	 case AST_Expression::EV_bool:
	   return ((v->u.bval == 0) ? "FALSE" : "TRUE");
	 case AST_Expression::EV_char:        
	   sprintf(temp, "'%c'",v->u.cval);  
	   break;
	 default:
	   // never get here
	   throw adabe_internal_error(__FILE__,__LINE__,
				      "Unexpected union discriminant value");
	 }
    }
  else
    // if we get here, we have an enum type, so gets the enum value as an adabe_enum_val object
    // and gets the name by the get_ada_local_name function
    {
      adabe_enum_val* v = adabe_enum_val::narrow_from_decl(adabe_enum::narrow_from_decl(t)->lookup_by_value(exp));
      return (v->get_ada_local_name());
    }
  
  return temp;
}
IMPL_NARROW_METHODS1(adabe_union_branch, AST_UnionBranch)
IMPL_NARROW_FROM_DECL(adabe_union_branch)







