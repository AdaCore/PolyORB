------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                            T E S T _ S E M A                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
--                                                                          --
-- GLADE  is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GLADE  is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed  with GLADE;  see file COPYING.  If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line;          use Ada.Command_Line;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with GLADE.Semaphores.Local;    use GLADE.Semaphores, GLADE.Semaphores.Local;
with GLADE.Semaphores.Tracing;  use GLADE.Semaphores.Tracing;
with Tracing_Test;              use Tracing_Test;

procedure Test_Sema is

   task type Sema_Task is
      entry Get_Index (Task_Index : in Positive);
      entry Make_Request;
   end Sema_Task;

   task body Sema_Task is
      Index     : Positive;
      Semaphore : Semaphore_Ptr := new Semaphore_Type;
   begin
      accept Get_Index (Task_Index : in Positive) do
         Index := Task_Index;
      end Get_Index;
      Put_Line ("Task" & Positive'Image (Index) & " started");
      Initialize (Semaphore, "sema_test");
      Put_Line ("Semaphore initialized for task" & Positive'Image (Index));
      loop
         select
            accept Make_Request;
            Put_Line ("Task" & Positive'Image (Index) & " making request");
            P (Semaphore);
            Put_Line ("Task" & Positive'Image (Index) & " got semaphore");
            delay 1.0;
            V (Semaphore);
            Put_Line ("Task" & Positive'Image (Index) & " released semaphore");
         or
            terminate;
         end select;
      end loop;
   exception
      when E : others =>
         Put_Line ("Exception in task" & Positive'Image (Index) & ":" &
                   Exception_Information (E));
   end Sema_Task;

   Number_Of_Tasks : constant Positive := Positive'Value (Argument (1));
   Tasks           : array (1 .. Number_Of_Tasks) of Sema_Task;

begin
   Register_Callback (Tracing_Callback'Access);
   for I in 1 .. Number_Of_Tasks loop
      Tasks (I) .Get_Index (I);
   end loop;
   for I in 2 .. Argument_Count loop
      Put_Line ("Will signal task " & Argument (I));
      Tasks (Positive'Value (Argument (I))) .Make_Request;
      Put_Line ("Task " & Argument (I) & " signaled");
   end loop;
end Test_Sema;
