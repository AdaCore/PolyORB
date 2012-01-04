------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T R A N S I E N T _ T A S K S                       --
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

package body Transient_Tasks is

   task body Transient_Task is
      My_Id : Natural;
   begin
      accept Start (Id : Natural) do
         My_Id := Id;
      end Start;

      Put_Line ("Transient" & My_Id'Img & ": enter");
      Transient_Task_Loop : loop
         select
            accept Quit do
               Put_Line ("Transient" & My_Id'Img & ": leave");
            end Quit;
            exit Transient_Task_Loop;
         or
            accept Enter do
               Put_Line ("Transient" & My_Id'Img & ": going transient");
            end Enter;
            Transient_Processing (My_Id);
            Put_Line ("Transient" & My_Id'Img & ": going dormant");
         end select;
      end loop Transient_Task_Loop;
   end Transient_Task;

   procedure Start (Count : Natural) is
   begin
      Transient_Tasks := new Transient_Task_Array (0 .. Count - 1);
      for J in Transient_Tasks'Range loop
         Transient_Tasks (J).Start (J);
      end loop;
   end Start;
end Transient_Tasks;
