package body AdaBroker.Sysdep is

   function To_Bool (B : Boolean) return Bool is
   begin
      if B then
         return Bool (True);
      else
         return Bool (False);
      end if;
   end To_Bool;

   function To_Boolean (B : Bool) return Boolean is
   begin
      if B = Bool (True) then
         return Standard.True;
      else
         return Standard.False;
      end if;
   end To_Boolean;

end AdaBroker.Sysdep;
