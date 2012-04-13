with Ada.Streams; use Ada.Streams;

package RT_JSON is
   pragma Remote_Types;

   type JSON_Holder is abstract tagged private;

   type JSON_Wrapper (H : access JSON_Holder'Class) is null record;

   procedure Read (S : access Root_Stream_Type'Class; V : out JSON_Wrapper);
   procedure Write (S : access Root_Stream_Type'Class; V : JSON_Wrapper);
   function  Input (S : access Root_stream_Type'Class) return JSON_Wrapper;
   procedure Output (S : access Root_Stream_Type'Class; V : JSON_Wrapper);

   for JSON_Wrapper'Read use Read;
   for JSON_Wrapper'Write use Write;
   for JSON_Wrapper'Input use Input;
   for JSON_Wrapper'Output use Output;

private
   type JSON_Holder is abstract tagged null record;
end RT_JSON;
