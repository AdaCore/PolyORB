with Idl_Fe.Types; use Idl_Fe.Types;
with Idl_Fe.Tree; use Idl_Fe.Tree;
--  with Idl_Fe.Tree.Synthetic; use Idl_Fe.Tree.Synthetic;

package body Ada_Be.Identifiers is

   function Ada_Full_Name
     (Node : Node_Id)
     return String is
   begin
      case Kind (Node) is
         when K_Scoped_Name =>
            return Get_Name (Value (Node));

         when others =>
            return Get_Name (Node);
      end case;
   end Ada_Full_Name;

   function Ada_Name
     (Node : Node_Id)
     return String
   is
      Full_Name : constant String
        := Ada_Full_Name (Node);
      Last_Dot : Integer := Full_Name'First - 1;
   begin
      for I in Full_Name'Range loop
         if Full_Name (I) = '.' then
            Last_Dot := Integer (I);
         end if;
      end loop;
      return Full_Name (Last_Dot + 1 .. Full_Name'Last);
   end Ada_Name;

end Ada_Be.Identifiers;
