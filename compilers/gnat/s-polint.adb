package body System.PolyORB_Interface is

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
     (Base : PolyORB.TypeCode.Object;
      Parameters : Any_Array)
      return PolyORB.TypeCode.Object
   is
      Result : PolyORB.TypeCode.Object
        := Base;
   begin
      for I in Parameters'Range loop
         PolyORB.Any.TypeCode.Add_Parameter
           (Result, Parameters (I));
      end loop;
      return Result;
   end TC_Build;

end System.PolyORB_Interface;
