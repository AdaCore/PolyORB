package body Types is

   -----------
   -- Dummy --
   -----------

   procedure Dummy (E : Entity_Id) is
   begin
      if Present (E) then
         null;
      end if;
   end Dummy;

   --------
   -- No --
   --------

   function No (E : Entity_Id) return Boolean is
   begin
      return E = No_Entity;
   end No;

   --------
   -- No --
   --------

   function No (N : Node_Id) return Boolean is
   begin
      return N = No_Node;
   end No;

   -------------
   -- Present --
   -------------

   function Present (E : Entity_Id) return Boolean is
   begin
      return E /= No_Entity;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (N : Node_Id) return Boolean is
   begin
      return N /= No_Node;
   end Present;

end Types;
