------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      T R A N S I E N T _ T A S K S                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--           Copyright (C) 2008, Free Software Foundation, Inc.             --
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
