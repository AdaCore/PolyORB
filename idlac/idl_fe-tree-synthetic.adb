with Idl_Fe.Types;
with Idl_Fe.Tree.Accessors; use Idl_Fe.Tree.Accessors;

package body Idl_Fe.Tree.Synthetic is

   function Head
     (NL : Node_List)
     return N_Root_Acc
   is
      It : Node_Iterator;
   begin
      Init (It, NL);
      return Get_Node (It);
   end Head;

   function Is_Empty
     (NL : Node_List)
     return Boolean
   is
      It : Node_Iterator;
   begin
      Init (It, NL);
      return Is_End (It);
   end Is_Empty;

   function Is_Interface_Type
     (Node : N_Root_Acc)
     return Boolean is
   begin
      case Get_Kind (Node.all) is
         when
           K_Interface         |
           K_Forward_Interface =>
            return True;
         when K_Scoped_Name =>
            return Is_Interface_Type
              (N_Root_Acc (N_Named_Acc'(Value (Node))));
         when K_Declarator =>
            if Is_Empty (Array_Bounds (Node)) then
               --  return Is_Interface_Type (Parent (Node));
               raise Program_Error;
            else
               return False;
            end if;
         when K_Type_Declarator =>
            return Is_Interface_Type (N_Root_Acc (T_Type (Node)));
         when others =>
            return False;
      end case;
   end Is_Interface_Type;

   function Is_Scope
     (Node : N_Root_Acc)
     return Boolean
   is
      K : constant Node_Kind
        := Get_Kind (Node.all);
   begin
      return (False
        or else K = K_Repository
        or else K = K_Module
        or else K = K_Interface
        or else K = K_ValueType);
   end Is_Scope;

end Idl_Fe.Tree.Synthetic;
