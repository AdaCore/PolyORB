package body Test_Servant is

   use PolyORB.Types;

   ---------------------
   -- Execute_Servant --
   ---------------------

   function Execute_Servant
     (S   : access My_Servant;
      Msg : PolyORB.Components.Message'Class)
     return PolyORB.Components.Message'Class
   is
   begin
      pragma Warnings (Off);
      return Handle_Message (S, Msg);
      pragma Warnings (On);
   end Execute_Servant;

   ----------
   -- Left --
   ----------

   function "=" (Left, Right : My_Servant)
                return Standard.Boolean is
   begin
      if Left.Nb = Right.Nb
        and then Left.Name = Right.Name
      then
         return True;
      end if;
      return False;
   end "=";

end Test_Servant;
