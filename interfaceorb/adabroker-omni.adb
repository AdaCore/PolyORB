with CORBA;

package body AdaBroker.Omni is

   use type CORBA.Unsigned_Long;

   --------------
   -- Align_To --
   --------------

   function Align_To
     (Size  : in CORBA.Unsigned_Long;
      Align : in Alignment_T)
      return CORBA.Unsigned_Long
   is
      Temp : CORBA.Unsigned_Long;
   begin
      Temp := Size mod CORBA.Unsigned_Long (Align);
      if Temp = 0 then
         return Size;
      else
         return Size + CORBA.Unsigned_Long (Align) - Temp;
      end if;
   end Align_To;

end AdaBroker.Omni;
