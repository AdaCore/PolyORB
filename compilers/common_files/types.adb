package body Types is

   -----------
   -- Dummy --
   -----------

   procedure Dummy (E : Node_Id) is
   begin
      if Present (E) then
         null;
      end if;
   end Dummy;

   --------
   -- No --
   --------

   function No (E : Node_Id) return Boolean is
   begin
      return E = No_Node;
   end No;

   -------------
   -- Present --
   -------------

   function Present (E : Node_Id) return Boolean is
   begin
      return E /= No_Node;
   end Present;

end Types;
