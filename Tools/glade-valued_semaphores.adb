------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--              G L A D E . V A L U E D _ S E M A P H O R E S               --
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

package body GLADE.Valued_Semaphores is

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
   -- Acquired --
   --------------

   function Acquired (Semaphore : access Semaphore_Type) return Boolean is
   begin
      return Semaphore.Is_Used;
   end Acquired;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Semaphore : Semaphore_Type) return Data_Type
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
   
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Semaphore : access Semaphore_Type;
                         Name      : in String)
   is
   begin
      --  Lock the partition

      Partition_Mutex.Enter;

      --  Create the private mutex and barrier for the semaphore

      Semaphore.Barrier := Barrier_Table.Allocate;
      Barrier_Table.Table (Semaphore.Barrier) := new Barrier_Type;

      Semaphore.Probable_Owner :=
        Get_Probable_Owner (Semaphore_Access (Semaphore), Name);

      --  If we are the probable owner, initialize the data to its initial
      --  value.

      if Semaphore.Probable_Owner = Semaphore_Access (Semaphore) then
	 Semaphore.Data := Null_Data;
      end if;

      --  Unlock the partition

      Partition_Mutex.Leave;
   end Initialize;

   -------
   -- P --
   -------

   procedure P (Semaphore : access Semaphore_Type) is
   begin
      --  Check if the semaphore has been initialized

      if Semaphore.Probable_Owner = null then
         raise Program_Error;
      end if;

      --  Lock the partition

      Partition_Mutex.Enter;

      --  If we do already have the semaphore, lock it and return

      if Semaphore.Probable_Owner = Semaphore_Access (Semaphore) then
         if Semaphore.Is_Used then
            Partition_Mutex.Leave;
            raise Program_Error;
         end if;
         Semaphore.Is_Used := True;
         Partition_Mutex.Leave;
         return;
      end if;

      --  Unlock the partition since we will send a message to a semaphore
      --  that may need the lock.

      Partition_Mutex.Leave;

      --  Since we do not have the semaphore and cannot be the probable
      --  owner of this semaphore, then get it from someone else and set
      --  ourselves as probable owner (we will become the owner after
      --  the current one, so we will not forward the requests anymore).

      Send_Request_To (Semaphore.Probable_Owner,
                       Semaphore_Access (Semaphore));
      Semaphore.Probable_Owner := Semaphore_Access (Semaphore);

      --  Wait for the semaphore to arrive and mark the semaphore as used

      Barrier_Table.Table (Semaphore.Barrier) .Wait;
      Semaphore.Is_Used := True;

   end P;

   ---------------------
   -- Send_Request_To --
   ---------------------

   procedure Send_Request_To (Recipient : access Semaphore_Type;
                              Client    : in Semaphore_Access)
   is
      Old_Probable_Owner : Semaphore_Access;
   begin
      --  Lock the current partition

      Partition_Mutex.Enter;

      --  If we the probable owner of the semaphore, then we will either
      --  give it to the client if it is unused yet or promise it to the
      --  client if it is in use. In other cases, we do forward the request
      --  to the probable owner. In all the cases, the new probable owner is
      --  the client.

      if Recipient.Probable_Owner = Semaphore_Access (Recipient)
        and then Recipient.Promised_To = null
      then
         if Recipient.Is_Used then
            Recipient.Promised_To := Client;
         else
            Signal (Client, Recipient.Data);
         end if;
         Recipient.Probable_Owner := Client;
         Partition_Mutex.Leave;
      else
         Old_Probable_Owner := Recipient.Probable_Owner;
         Recipient.Probable_Owner := Client;
         Partition_Mutex.Leave;
         Send_Request_To (Old_Probable_Owner, Client);
      end if;
   end Send_Request_To;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data (Semaphore : access Semaphore_Type;
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

   procedure Signal (Recipient : access Semaphore_Type;
		     Data      : in Data_Type)
   is
   begin
      Recipient.Data := Data;
      Barrier_Table.Table (Recipient.Barrier) .Signal (At_Least_One => True);
   end Signal;

   -------
   -- V --
   -------

   procedure V (Semaphore : access Semaphore_Type) is
   begin
      --  Check that the semaphore is used

      if not Semaphore.Is_Used then
         raise Program_Error;
      end if;

      --  Lock the partition

      Partition_Mutex.Enter;

      --  If the semaphore has been promised to someone else, then give it

      if Semaphore.Promised_To /= null then
         Signal (Semaphore.Promised_To);
         Semaphore.Promised_To := null;
      end if;

      --  Semaphore is not in use

      Semaphore.Is_Used := False;

      --  Unlock the partition

      Partition_Mutex.Leave;

   end V;

end GLADE.Valued_Semaphores;
