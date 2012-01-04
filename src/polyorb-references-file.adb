------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--              P O L Y O R B . R E F E R E N C E S . F I L E               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

with PolyORB.Initialization;
with PolyORB.Utils.Strings;

package body PolyORB.References.File is

   File_Prefix : constant String := "file://";

   ----------------------
   -- String_To_Object --
   ----------------------

   function String_To_Object (Str : String) return Ref;

   function String_To_Object (Str : String) return Ref is
      use Ada.Text_IO;

      File : File_Type;
      Item : String (1 .. 4 * 1024);
      Last : Natural;

      Result : Ref;

   begin
      Open (File, In_File, Str (Str'First + File_Prefix'Length .. Str'Last));
      Get_Line (File, Item, Last);
      Close (File);

      PolyORB.References.String_To_Object (Item (1 .. Last), Result);

      return Result;
   end String_To_Object;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;

   procedure Initialize is
   begin
      Register_String_To_Object (File_Prefix, String_To_Object'Access);
   end Initialize;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"references.file",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"references",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.References.File;
