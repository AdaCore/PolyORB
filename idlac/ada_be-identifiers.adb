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
            return "";

         when others =>
            declare
               P_Node : constant Node_Id
                 := Parent_Scope (Node);
            begin
               if P_Node = No_Node then
                  return "NO_PARENT." & Name (Node);
               end if;

               if Kind (P_Node) = K_Ben_IDL_File
                 and then Is_Gen_Scope (Node) then
                  return Name (Node);
               else
                  --  return Ada_Full_Name (Parent_Scope (Node))
                  --    & "." & Name (Node);
                  --  XXX TEMPORARY WORKAROUND
                  declare
                     FN : constant String
                       := Ada_Full_Name (Parent_Scope (Node));
                  begin
                     if FN'Length = 0 then
                        return Name (Node);
                     else
                        return FN & "." & Name (Node);
                     end if;
                  end;
               end if;
            end;
      end case;
   end Ada_Full_Name;

   function Ada_Name
     (Node : Node_Id)
     return String
   is
      Result : String
        := Name (Node);
      First : Integer := Result'First;

   begin
      while First <= Result'Last
        and then Result (First) = '_' loop
         First := First + 1;
      end loop;

      for I in First .. Result'Last loop
         if Result (I) = '_'
           and then I < Result'Last
           and then Result (I + 1) = '_' then
            Result (I + 1) := 'U';
         end if;
      end loop;

      --  FIXME: Check for collisions with Ada reserved
      --  words and other name clashes (part of this task
      --  should probably be done during expansion).

      return Result (First .. Result'Last);
   end Ada_Name;

   function Scope_Name
     (Node : Node_Id)
     return String is
   begin
      return Ada_Full_Name (Parent_Scope (Node));
   end Scope_Name;

end Ada_Be.Identifiers;
