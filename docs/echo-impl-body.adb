with Ada.Text_IO;

with Echo.Skel;
pragma Warnings (Off, Echo.Skel);
--  No entity from Echo.Skel is referenced.

package body Echo.Impl is

   ----------------
   -- EchoString --
   ----------------

   function EchoString
     (Self : access Object;
      Mesg : CORBA.String) return CORBA.String
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

   begin
      Ada.Text_IO.Put_Line
        ("Echoing string: « " & CORBA.To_Standard_String (Mesg)
         & " »");
      return Mesg;
   end EchoString;

end Echo.Impl;
