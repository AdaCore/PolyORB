package body System.PolyORB_Interface is

   --------------------
   -- Handle_Message --
   --------------------

   function Handle_Message
     (Self : access Component;
      Msg  : PolyORB.Components.Message'Class)
      return PolyORB.Components.Message'Class
   is
   begin
      pragma Assert (Self.Handler /= null);
      return Self.Handler.all (Msg);
   end Handle_Message;

   ---------------
   -- TA_String --
   ---------------

   function TA_String (S : String) return PolyORB.Any.Any is
   begin
      return PolyORB.Any.To_Any (PolyORB.Types.To_PolyORB_String (S));
   end TA_String;

   --------------
   -- TC_Build --
   --------------

   function TC_Build
     (Base : PolyORB.Any.TypeCode.Object;
      Parameters : Any_Array)
      return PolyORB.Any.TypeCode.Object
   is
      Result : PolyORB.Any.TypeCode.Object
        := Base;
   begin
      for I in Parameters'Range loop
         PolyORB.Any.TypeCode.Add_Parameter
           (Result, Parameters (I));
      end loop;
      return Result;
   end TC_Build;

end System.PolyORB_Interface;
