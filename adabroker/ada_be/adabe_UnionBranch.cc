// File adabe_union_branch

adabe_union_branch::adabe_union_branch(AST_UnionLabel *lab, AST_Type *ft, UTL_ScopedName *n,
		  UTL_StrList *p);
//constructor

adabe_union-branch::produce_ads(dep_list with,string &String, string &previousdefinition);
/*
      temp += "when "
      if (label()->label_kind() != UL_default)
          label()->label_val().produce(with,&tmp,&previousdefinition)
      else if (branche->label()->label_kind() == UL_default)
           temp =+ "others "

      temp =+ "=> "
      adabe_field.produce_ads(with,&temp,&previousdefinition)


 */


IMPL_NARROW_METHODS1(adabe_union_branch, AST_UnionBranch);
IMPL_NARROW_FROM_DECL(adabe_union_branch);


