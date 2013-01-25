package body Normal_JSON is
   function Wrap (J : GNATCOLL.JSON.JSON_Value) return RT_JSON.JSON_Wrapper is
   begin
      return RT_JSON.JSON_Wrapper'
        (H => new JSON_Holder'(RT_JSON.JSON_Holder with Value => J));
   end Wrap;

   function Unwrap (W : RT_JSON.JSON_Wrapper) return GNATCOLL.JSON.JSON_Value is
   begin
      return JSON_Holder (W.H.all).Value;
   end Unwrap;

end Normal_JSON;
