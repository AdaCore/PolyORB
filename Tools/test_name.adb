------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                            T E S T _ N A M E                             --
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

with Ada.Text_IO;            use Ada.Text_IO;
with GLADE.Semaphores;       use GLADE.Semaphores;
with GLADE.Semaphores.Local; use GLADE.Semaphores.Local;

procedure Test_Name is

   Sema1 : Semaphore_Ptr := new Semaphore_Type;
   Sema2 : Semaphore_Ptr := new Semaphore_Type;
   Sema3 : Semaphore_Ptr := new Semaphore_Type;
   Sema4 : Semaphore_Ptr := new Semaphore_Type;
   Sema5 : Semaphore_Ptr := new Semaphore_Type;
   Sema6 : Semaphore_Ptr := new Semaphore_Type;

begin
   Initialize (Sema1, "abcde");
   Initialize (Sema2, "abcde");
   Initialize (Sema3, "abcde");
   Initialize (Sema4, "abcde");
   Initialize (Sema5, "abcde");
   Initialize (Sema6, "abcde");
   P (Sema1);
   V (Sema1);
   P (Sema2);
   V (Sema2);
   P (Sema3);
   V (Sema3);
end Test_Name;
