with Types;     use Types;

package Names is

   procedure Check_Identifier (Ref, Def : Node_Id);
   --  Return true when L and R have the same IDL names

   function First_Homonym (N : Node_Id) return Node_Id;
   procedure Set_First_Homonym (N : Node_Id; V : Node_Id);

end Names;
