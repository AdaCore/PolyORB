with Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;

package body Idl_Fe.Tree.Synthetic is

   function Head
     (NL : Node_List)
     return Node_Id
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

   function Length
     (NL : Node_List)
     return Natural
   is
      It : Node_Iterator;
      Count : Natural
        := 0;
   begin
      Init (It, NL);
      while not Is_End (It) loop
         Count := Count + 1;
         --  Dummy := Get_Node (It);
         Next (It);
      end loop;

      return Count;
   end Length;

   function Is_Interface_Type
     (Node : Node_Id)
     return Boolean is
   begin
      case Kind (Node) is
         when
           K_Interface         |
           K_Forward_Interface =>
            return True;

         when K_Scoped_Name =>
            return Is_Interface_Type
              (Node_Id (Value (Node)));

         when K_Declarator =>
            declare
               P_Node : constant Node_Id
                 := Parent (Node);
            begin
               pragma Assert (Is_Type_Declarator (P_Node));

               if Is_Empty (Array_Bounds (Node)) then
                  return Is_Interface_Type (T_Type (P_Node));
               else
                  return False;
               end if;
            end;

         when others =>
            return False;
      end case;
   end Is_Interface_Type;

   function Is_Gen_Scope
     (Node : Node_Id)
     return Boolean
   is
      K : constant Node_Kind
        := Kind (Node);
   begin
      return (False
        or else K = K_Repository
        or else K = K_Module
        or else K = K_Interface
        or else K = K_ValueType);
   end Is_Gen_Scope;

end Idl_Fe.Tree.Synthetic;
