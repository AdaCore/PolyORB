------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                            M A I N _ S E M A                             --
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

with Ada.Numerics.Float_Random;
with GLADE.Semaphores.Local;    use GLADE.Semaphores, GLADE.Semaphores.Local;
with GLADE.Semaphores.Tracing;  use GLADE.Semaphores.Tracing;
with Tracing_Test;              use Tracing_Test;

procedure Main_Sema is
   Semaphore : Semaphore_Ptr := new Semaphore_Type;
   Partition : Natural := Main_Sema'Partition_ID;
   Generator : Ada.Numerics.Float_Random.Generator;
begin
   Register_Callback (Tracing_Callback'Access);
   Initialize (Semaphore, "sema_test");
   for I in 1 .. 20 loop
      P (Semaphore);
      Tracing_Report
        ("Critical section " & I'Img &
         " on partition " & Partition'Img);
      delay Duration (2.0 * Ada.Numerics.Float_Random.Random (Generator));
      V (Semaphore);
   end loop;
   Tracing_Report ("Job completed on partition " & Partition'Img);
end Main_Sema;
