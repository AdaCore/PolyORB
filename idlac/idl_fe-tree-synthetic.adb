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
        or else K = K_Ben_Idl_File
        or else K = K_Module
        or else K = K_Interface
        or else K = K_ValueType);
   end Is_Gen_Scope;

   function Name
     (Node : in Node_Id)
     return String is
   begin
      if Definition (Node) /= null then
         return Definition (Node).Name.all;
      else
         return "##null##";
      end if;
   end Name;

   function Parent_Scope
     (Node : in Node_Id)
     return Node_Id is
   begin
      if Definition (Node) /= null then
         return Definition (Node).Parent_Scope;
      else
         return No_Node;
      end if;
   end Parent_Scope;

   function Idl_Repository_Id
     (Node : in Node_Id)
     return String
   is
      Format : constant String
        := "IDL";
      Prefix : constant String
        := "adabroker.unique.prefix";
      Version : constant String
        := "1.0";

      function Repository_Identifiers
        (Node : Node_Id)
        return String;
      --  The list of identifiers that make up the
      --  IDL repository id of Node.

      function Repository_Identifiers
        (Node : Node_Id)
        return String is
      begin
         case Kind (Node) is
            when K_Scoped_Name =>
               return Repository_Identifiers (Value (Node));

            when K_Ben_Idl_File =>
               return Name (Node);

            when K_Repository =>
               --  XXX Should be an error.
               return "";

            when others =>
               declare
                  P_Node : constant Node_Id
                    := Parent_Scope (Node);
               begin
                  pragma Assert (P_Node /= No_Node);

                  if Kind (P_Node) = K_Ben_Idl_File
                    and then Is_Gen_Scope (Node) then
                     return Name (Node);
                  else
                     --  return Idl_Repository_Identifiers
                     --  (Parent_Scope (Node))
                     --    & "." & Name (Node);
                     --  XXX TEMPORARY WORKAROUND
                     declare
                        FN : constant String
                          := Repository_Identifiers (Parent_Scope (Node));
                     begin
                        if FN'Length = 0 then
                           return Name (Node);
                        else
                           return FN & "/" & Name (Node);
                        end if;
                     end;
                  end if;
               end;
         end case;
      end Repository_Identifiers;

   begin
      return Format
        & ":" & Prefix & "/" & Repository_Identifiers (Node)
        & ":" & Version;
   end Idl_Repository_Id;

   function All_Ancestors
     (Node : Node_Id;
      Exclude : Node_List := Nil_List)
     return Node_List
   is
      It : Node_Iterator;
      I_Node : Node_Id;
      --  A scoped name in the inheritance spec.

      P_Node : Node_Id;
      --  The corresponding actual parent node.

      Result : Node_List
        := Nil_List;
   begin
      Init (It, Parents (Node));

      while not Is_End (It) loop
         I_Node := Get_Node (It);
         Next (It);

         P_Node := Value (I_Node);
         if not (False
           or else Is_In_List (Exclude, P_Node)
           or else Is_In_List (Result, P_Node)) then
            Append_Node (Result, P_Node);
            Merge_List
              (Into => Result,
               From => All_Ancestors (P_Node));
         end if;
      end loop;

      return Result;
   end All_Ancestors;

end Idl_Fe.Tree.Synthetic;
