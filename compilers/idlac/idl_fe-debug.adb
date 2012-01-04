------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . D E B U G                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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

with Ada.Text_IO; use Ada.Text_IO;

package body Idl_Fe.Debug is

   Filename : constant String := "idl_fe.opt";

   type String_Ptr is access String;

   Flag_Table : array (1 .. 32) of String_Ptr;
   Last_Flag  : Natural := 0;

   ---------------
   -- Is_Active --
   ---------------

   function Is_Active
     (Flag : String)
      return Natural
   is
   begin
      for I in 1 .. Last_Flag loop
         if Flag_Table (I).all = Flag then
            return I;
         end if;
      end loop;
      return 0;
   end Is_Active;

   ------------
   -- Output --
   ------------

   procedure Output
     (Message : String)
   is
   begin
      if Flag /= 0 then
         Put_Line (Current_Error, Flag_Table (Flag).all & ": " & Message);
      end if;
   end Output;

   File : File_Type;
   Line : String (1 .. 256);
   Last : Natural;

begin
   begin
      Open (File, In_File, Filename);

      while not End_Of_File (File) loop
         Get_Line (File, Line, Last);
         if Last /= 0 then
            if Line (1) /= '#' then
               if Is_Active (Line (1 .. Last)) = 0 then
                  Last_Flag := Last_Flag + 1;
                  Flag_Table (Last_Flag) := new String'(Line (1 .. Last));
               end if;
            end if;
         end if;
      end loop;

      Close (File);

   exception
      when others =>
      null;
   end;
end Idl_Fe.Debug;
