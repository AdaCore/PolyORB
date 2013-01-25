with RT_JSON;
with GNATCOLL.JSON;

package Normal_JSON is
   type JSON_Holder is new RT_JSON.JSON_Holder with record
      Value : GNATCOLL.JSON.JSON_Value;
   end record;

   function Wrap (J : GNATCOLL.JSON.JSON_Value) return RT_JSON.JSON_Wrapper;
   function Unwrap (W : RT_JSON.JSON_Wrapper) return GNATCOLL.JSON.JSON_Value;

end Normal_JSON;
