--
--  $Id$
--

package Spif.IO is

   --  The goal of this package is to ease the use of IO routines.

   pragma Preelaborate;

   type IO_Device is (Terminal, Monitor);
   --  Terminal is the control terminal while the monitor is
   --  the private output from Spif.

   procedure Put
     (Msg  : in String;
      Onto : in IO_Device := Terminal);
   --  Output a string.

   procedure Put_Line
     (Msg  : in String;
      Onto : in IO_Device := Terminal);
   --  Output a line.

   procedure New_Line
     (Onto : in IO_Device := Terminal);
   --  New line.

   function Get_Line return String;
   --  Read a line from terminal.

   function Get_Character return Character;
   --  Read a character from terminal. It will raise
   --  Ada.IO_Exceptions.End_Error if an end of file is encountered.

end Spif.IO;
