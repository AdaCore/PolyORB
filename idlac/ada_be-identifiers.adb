with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

package body Ada_Be.Identifiers is

   function Ada_Full_Name
     (Node : Node_Id)
     return String is
   begin
      case Kind (Node) is
         when K_Scoped_Name =>
            return Ada_Full_Name (Value (Node));

         when K_Ben_IDL_File =>
            return Name (Node);

         when K_Repository =>
            --  XXX Should be an error.
            return "REPOSITORY";

         when others =>
            declare
               P_Node : constant Node_Id
                 := Parent_Scope (Node);
            begin
               if P_Node = No_Node then
                  return "ANONYMOUS_Parent." & Name (Node);
               end if;

               if Kind (P_Node) = K_Ben_IDL_File
                 and then Is_Gen_Scope (Node) then
                  return Name (Node);
               else
                  return Ada_Full_Name (Parent_Scope (Node))
                    & "." & Name (Node);
               end if;
            end;
      end case;
   end Ada_Full_Name;

--     function Ada_Name
--       (Node : Node_Id)
--       return String
--     is
--        Full_Name : constant String
--          := Ada_Full_Name (Node);
--        Last_Dot : Integer := Full_Name'First - 1;
--     begin
--        for I in Full_Name'Range loop
--           if Full_Name (I) = '.' then
--              Last_Dot := Integer (I);
--           end if;
--        end loop;
--        return Full_Name (Last_Dot + 1 .. Full_Name'Last);
--     end Ada_Name;

   function Ada_Name
     (Node : Node_Id)
     return String is
   begin
      return Name (Node);
   end Ada_Name;

   function Scope_Name
     (Node : Node_Id)
     return String is
   begin
      return Ada_Full_Name (Parent_Scope (Node));
   end Scope_Name;

end Ada_Be.Identifiers;
