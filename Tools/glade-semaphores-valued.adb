------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--              G L A D E . S E M A P H O R E S . V A L U E D               --
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

with GLADE.Semaphores.Name_Table; use GLADE.Semaphores.Name_Table;
with GLADE.Protected_Objects;     use GLADE.Protected_Objects;
with GNAT.Table;

package body GLADE.Semaphores.Valued is

   type Barrier_Access is access Barrier_Type;

   package Barrier_Table is new GNAT.Table
     (Table_Component_Type => Barrier_Access,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 5,
      Table_Increment      => 100);

   Partition_Mutex : Mutex_Type;
   --  This mutex will lock all the semaphores. Since they will be locked
   --  for very short periods of time, this is affordable without any problem.

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Semaphore : access Valued_Semaphore_Type)
     return Data_Type
   is
      Previously_Acquired : constant Boolean := Acquired (Semaphore);
      Result              : Data_Type;
   begin
      if not Previously_Acquired then
	 P (Semaphore);
      end if;

      Result := Semaphore.Data;

      if not Previously_Acquired then
	 V (Semaphore);
      end if;

      return Result;
   end Get_Data;
   
   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Semaphore : access Valued_Semaphore_Type;
		       Data      : in Data_Type)
   is
      Previously_Acquired : constant Boolean := Acquired (Semaphore);
   begin
      if not Previously_Acquired then
	 P (Semaphore);
      end if;

      Semaphore.Data := Data;

      if not Previously_Acquired then
	 V (Semaphore);
      end if;
   end Set_Data;

   ------------
   -- Signal --
   ------------

   procedure Signal (Recipient : access Valued_Semaphore_Type;
		     Data      : in Data_Type)
   is
   begin
      Recipient.Data := Data;
      Barrier_Table.Table (Recipient.Barrier) .Signal (At_Least_One => True);
   end Signal;

   -------
   -- V --
   -------

   procedure V (Semaphore : access Valued_Semaphore_Type) is
   begin
      --  Check that the semaphore is used

      if not Semaphore.Is_Used then
         raise Program_Error;
      end if;

      --  Lock the partition

      Partition_Mutex.Enter;

      --  If the semaphore has been promised to someone else, then give it

      if Semaphore.Promised_To /= null then
         Signal (Valued_Semaphore_Access (Semaphore.Promised_To),
		 Semaphore.Data);
         Semaphore.Promised_To := null;
      end if;

      --  Semaphore is not in use

      Semaphore.Is_Used := False;

      --  Unlock the partition

      Partition_Mutex.Leave;

   end V;

end GLADE.Semaphores.Valued;
