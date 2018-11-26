with Normal_JSON;   use Normal_JSON;
with GNATCOLL.JSON; use GNATCOLL.JSON;

package body RCI is

   function Frob (X : RT_JSON.JSON_Wrapper) return RT_JSON.JSON_Wrapper is
      J_Input  : JSON_Value := Unwrap (X);
      J_Output : JSON_Value := Create_Object;
   begin
      J_Output.Set_Field ("input", J_Input);
      J_Output.Set_Field ("output",  Create (Integer'(456)));
      return Wrap (J_Output);
   end Frob;

end RCI;
