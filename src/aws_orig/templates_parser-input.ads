------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               T E M P L A T E S _ P A R S E R . I N P U T                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

package Templates_Parser.Input is

   type File_Type is limited private;

   procedure Open
     (File : in out File_Type;
      Name : String;
      Form : String := "");
   pragma Inline (Open);
   --  Like Text_IO.Open and Mode = In_File.

   procedure Close (File : in out File_Type);
   pragma Inline (Close);
   --  Like Text_IO.Close. Raises text_IO.Status_Error is file is not open.

   function End_Of_File (File : File_Type) return Boolean;
   pragma Inline (End_Of_File);
   --  Like Text_IO.End_Of_File. Raises Text_IO.Status_Error is file is not
   --  open.

   function LF_Terminated (File : File_Type) return Boolean;
   pragma Inline (LF_Terminated);
   --  Returns True if last line returned by Get_Line was terminated with a LF
   --  or CR+LF on DOS based systems.

   procedure Get_Line
     (File   : File_Type;
      Buffer :    out String;
      Last   :    out Natural);
   pragma Inline (Get_Line);
   --  Like Text_IO.Get_Line. Raises Text_IO.Status_Error is file is not open.

private

   type File_Record;
   type File_Type is access File_Record;

end Templates_Parser.Input;
