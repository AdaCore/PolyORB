--  This is a debugging package for AdaBroker.
--
--  with Adabroker_Debug;
--  pragma Elaborate(Adabroker_Debug);
--
--  Debug : constant Boolean := Adabroker_Debug.Is_Active ("specific_name");
--
--  and then :
--
--  pragma Debug (Output (Debug, "debugging info"));
--
--  The output will be done if "adabroker_debug_options.txt" file contains
--  a line with "specific_name"

package Adabroker_Debug is

   function Is_Active (Flag : in String) return Boolean;
   --  returns True if Flag was found in the debug file

   procedure Output (Flag : in Boolean; Msg : in String);
   --  Prints S on standard output if Flag is true

end Adabroker_Debug;
