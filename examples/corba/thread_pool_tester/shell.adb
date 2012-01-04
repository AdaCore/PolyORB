------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                S H E L L                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2008-2012, Free Software Foundation, Inc.          --
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

package body Shell is

   procedure Interact is
      Line    : String (1 .. 80);
      Last    : Integer;
      Space   : Integer;
      Command : Command_Type;
   begin
      Main_Loop : loop
         begin
            Get_Command : loop
               Put ("> ");
               Get_Line (Line, Last);
               Space := 1;
               while Space <= Last and then Line (Space) /= ' ' loop
                  Space := Space + 1;
               end loop;

               begin
                  Command := Command_Type'Value (Line (1 .. Space - 1));
                  exit Get_Command;
               exception
                  when Constraint_Error =>
                     Put_Line ("?");
               end;
            end loop Get_Command;
            Handle_Command (Command, Line (Space + 1 .. Last));
         end;
      end loop Main_Loop;
   exception
      when Exit_Shell =>
         null;
   end Interact;
end Shell;
