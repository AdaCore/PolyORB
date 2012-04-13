with Normal_JSON;
with GNATCOLL.JSON;

package body RT_JSON is

   procedure Read (S : access Root_Stream_Type'Class; V : out JSON_Wrapper) is
      Str : constant String := String'Input (S);
      pragma Unmodified (V);
   begin
      Normal_JSON.JSON_Holder (V.H.all).Value := GNATCOLL.JSON.Read (Str, "");
   end Read;

   procedure Write (S : access Root_Stream_Type'Class; V : JSON_Wrapper) is
   begin
      String'Output (S, GNATCOLL.JSON.Write (Normal_JSON.Unwrap (V)));
   end Write;

   function Input (S : access Root_stream_Type'Class) return JSON_Wrapper is
   begin
      return V : JSON_Wrapper (H => new Normal_JSON.JSON_Holder) do
         JSON_Wrapper'Read (S, V);
      end return;
   end Input;

   procedure Output (S : access Root_Stream_Type'Class; V : JSON_Wrapper) is
   begin
      JSON_Wrapper'Write (S, V);
   end Output;

end RT_JSON;
