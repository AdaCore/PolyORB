with Errors; use Errors;
with Namet;  use Namet;
with Nodes;  use Nodes;

package body Names is

   -------------------
   -- First_Homonym --
   -------------------

   function First_Homonym (N : Node_Id) return Node_Id
   is
      HN : constant Name_Id := Name (N);
   begin
      return Node_Id (Get_Name_Table_Info (HN));
   end First_Homonym;

   ----------------------
   -- Check_Identifier --
   ----------------------

   procedure Check_Identifier (Ref, Def : Node_Id) is
   begin
      if Present (Ref)
        and then Present (Def)
        and then IDL_Name (Ref) /= IDL_Name (Def)
      then
         Error_Loc  (1) := Loc  (Ref);
         Error_Name (1) := Name (Def);
         Error_Loc  (2) := Loc  (Def);
         DE ("bad casing of#declared!");
      end if;
   end Check_Identifier;

   -----------------------
   -- Set_First_Homonym --
   -----------------------

   procedure Set_First_Homonym (N : Node_Id; V : Node_Id) is
   begin
      Set_Name_Table_Info (Name (N), Int (V));
   end Set_First_Homonym;

end Names;
