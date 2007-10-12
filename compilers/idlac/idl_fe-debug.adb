------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                         I D L _ F E . D E B U G                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2006, Free Software Foundation, Inc.          --
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
