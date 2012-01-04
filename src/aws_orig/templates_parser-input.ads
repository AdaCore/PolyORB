------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--               T E M P L A T E S _ P A R S E R . I N P U T                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
