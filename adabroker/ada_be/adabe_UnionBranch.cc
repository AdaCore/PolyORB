// File adabe_union_branch

adabe_union_branch::adabe_union_branch(AST_UnionLabel *lab, AST_Type *ft, UTL_ScopedName *n,
		  UTL_StrList *p);
//constructor

void
adabe_union_branch::produce_ads(dep_list with,string &String, string &previousdefinition, AST_ConcreteType *concrete);
/*
      temp += "when "
      if (label()->label_kind() != UL_default)
           temp += produce_disc_value(concrete, label()->label_value());
      else if (branche->label()->label_kind() == UL_default)
           temp =+ "others "

      temp =+ "=> "
      adabe_field.produce_ads(with,&temp,&previousdefinition)
      previousdefinition += temp;

 */
/////////////////////// peut etre inutile ///////////////////////
void
adabe_union_branch::produce_impl_ads(dep_list with,string &String, string &previousdefinition, AST_ConcreteType *concrete);
/*
  produce_ads(with, &String, &previousdefinition, *concrete);

 */

  
string
adabe_union_branch::produce_disc_value( AST_ConcreteType* t,AST_Expression* exp)
  /*
  if (t->node_type() != AST_Decl::NT_enum)
    {
      AST_Expression::AST_ExprValue *v = exp->ev();
      switch (v->et) 
	{
	case AST_Expression::EV_short:
	  return (toString (v->u.sval));

	case AST_Expression::EV_ushort:
	  return (toString (v->u.usval));

	case AST_Expression::EV_long:
	  return (toString (v->u.lval)°;

	case AST_Expression::EV_ulong:
	  return (toString (v->u.ulval));

	case AST_Expression::EV_bool:
	  return ((v->u.bval == 0) ? "FALSE" : "TRUE");

	case AST_Expression::EV_char:         
	  {
	    return (c = v->u.cval);
//	    if (c >= ' ' && c <= '~')
//	      s << "'" << c << "'";
//	    else {
//	      s << "'\\"
//		<< (int) ((c & 0100) >> 6)
//		<< (int) ((c & 070) >> 3)
//		<< (int) (c & 007)
//		<< "'";
	    }
	  }
	  break;
	default:
	  throw o2be_internal_error(__FILE__,__LINE__,
				    "Unexpected union discriminant value");

	}
    }
  else
    {
       AST_EnumVal* v = AST_Enum::narrow_from_decl(t)->lookup_by_value(exp);
	return (v.get_ada_name());					      
    }
}

    



   */
IMPL_NARROW_METHODS1(adabe_union_branch, AST_UnionBranch);
IMPL_NARROW_FROM_DECL(adabe_union_branch);


