------------------------------------------------------------------------------
--                                                                          --
--                              GLADE TOOLS                                 --
--                                                                          --
--                     G L A D E . S E M A P H O R E S                      --
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
with GLADE.Semaphores.Tracing;    use GLADE.Semaphores.Tracing;
with GNAT.Table;

package body GLADE.Semaphores is

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
      Record_Transaction (Semaphore.all'Access, Acquire_Semaphore);
      return Semaphore.Status = Locked;
   end Acquired;

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

      Get_Probable_Owner (Semaphore.all'Access, Name,
                          Semaphore.Probable_Owner, Semaphore.Index);

      --  Store the name of the semaphore
      Semaphore.Name := new String'(Name);

      Semaphore.Status := Unlocked;

      Record_Transaction (Semaphore.all'Access, Initialize_Semaphore,
                          Semaphore.Probable_Owner);

      --  Unlock the partition

      Partition_Mutex.Leave;
   end Initialize;

   -------
   -- P --
   -------

   procedure P (Semaphore : access Semaphore_Type) is
      Old_Probable_Owner : Semaphore_Access;

   begin
      --  Check if the semaphore has been initialized

      if Semaphore.Probable_Owner = null then
         raise Program_Error;
      end if;

      --  Lock the partition

      Partition_Mutex.Enter;

      --  If we do already have the semaphore, lock it and return

      if Semaphore.Probable_Owner = Semaphore.all'Access then
         if Semaphore.Status = Locked then
            Partition_Mutex.Leave;
            raise Program_Error;
         end if;
         Record_Transaction (Semaphore.all'Access, P);

         Semaphore.Status := Locked;
         Partition_Mutex.Leave;
         return;
      end if;

      Record_Transaction (Semaphore.all'Access,
                          Set_Probable_Owner,
                          Semaphore.all'Access);

      Old_Probable_Owner       := Semaphore.Probable_Owner;
      Semaphore.Status         := Waiting;
      Semaphore.Probable_Owner := Semaphore.all'Access;

      --  Unlock the partition since we will send a message to a semaphore
      --  that may need the lock.

      Partition_Mutex.Leave;

      --  Since we do not have the semaphore and cannot be the probable
      --  owner of this semaphore, then get it from someone else and set
      --  ourselves as probable owner (we will become the owner after
      --  the current one, so we will not forward the requests anymore).

      --  Record_Transaction (Semaphore.all'Access,
      --                      Ask_For_Semaphore,
      --                      Old_Probable_Owner);

      Send_Request_To (Recipient => Old_Probable_Owner,
                       Client    => Semaphore.all'Access);

      --  Wait for the semaphore to arrive and mark the semaphore as used

      Barrier_Table.Table (Semaphore.Barrier) .Wait;

      Record_Transaction (Semaphore.all'Access, P);

      Semaphore.Status := Locked;

   end P;

   ----------
   -- Read --
   ----------

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   Str    : out String_Access)
   is
   begin
      if Boolean'Input (Stream) then
         Str := new String'(String'Input (Stream));
      else
         Str := null;
      end if;
   end Read;

   ---------------------
   -- Semaphore_Index --
   ---------------------

   function Semaphore_Index (Semaphore : access Semaphore_Type)
     return Positive
   is
   begin
      return Semaphore.Index;
   end Semaphore_Index;

   --------------------
   -- Semaphore_Name --
   --------------------

   function Semaphore_Name (Semaphore : access Semaphore_Type)
     return String
   is
   begin
      return Semaphore.Name.all;
   end Semaphore_Name;

   -------------------------
   -- Semaphore_Partition --
   -------------------------

   function Semaphore_Partition (Semaphore : access Semaphore_Type)
     return Natural
   is
   begin
      return Glade.Semaphores'Partition_ID;
   end Semaphore_Partition;

   ---------------------
   -- Send_Request_To --
   ---------------------

   procedure Send_Request_To (Recipient : access Semaphore_Type;
                              Client    : in Semaphore_Access) is
      Old_Probable_Owner : Semaphore_Access;

   begin
      --  Lock the current partition

      Partition_Mutex.Enter;

      --  If we are the probable owner of the semaphore, then we will either
      --  give it to the client if it is unused yet or promise it to the
      --  client if it is in use. In other cases, we do forward the request
      --  to the probable owner. In all the cases, the new probable owner is
      --  the client.

      --  Record_Transaction (Semaphore_Access (Recipient),
      --                      Comparaison,
      --                      Recipient.Probable_Owner);

      if Recipient.Probable_Owner = Recipient.all'Access then
         pragma Assert (Recipient.Promised_To = null);
         if Recipient.Status = Unlocked then
            Record_Transaction (Recipient.all'Access,
                                Abandon_Semaphore,
                                Client);
            Signal (Client);
            Recipient.Probable_Owner := Client;
            Record_Transaction (Recipient.all'Access,
                                Set_Probable_Owner,
                                Client);
         else
            Record_Transaction (Recipient.all'Access,
                                Promise_Semaphore,
                                Client);
            Recipient.Promised_To := Client;
            Recipient.Probable_Owner := Client;
         end if;
         Partition_Mutex.Leave;
      else
         Record_Transaction (Recipient.all'Access,
                             Set_Probable_Owner,
                             Client);
         Old_Probable_Owner       := Recipient.Probable_Owner;
         Recipient.Probable_Owner := Client;
         Partition_Mutex.Leave;
         --  Record_Transaction (Recipient.all'Access,
         --                      Forward_Request,
         --                      Old_Probable_Owner);
         Send_Request_To (Recipient => Old_Probable_Owner,
                          Client    => Client);
      end if;
   end Send_Request_To;

   ------------
   -- Signal --
   ------------

   procedure Signal (Recipient : access Semaphore_Type) is
   begin
      Barrier_Table.Table (Recipient.Barrier) .Signal (At_Least_One => True);
   end Signal;

   -------
   -- V --
   -------

   procedure V (Semaphore : access Semaphore_Type) is
   begin
      --  Check that the semaphore is used

      if Semaphore.Status /= Locked then
         raise Program_Error;
      end if;

      Record_Transaction (Semaphore.all'Access, V);

      --  Lock the partition

      Partition_Mutex.Enter;

      --  If the semaphore has been promised to someone else, then give it

      if Semaphore.Promised_To /= null then
         Record_Transaction (Semaphore.all'Access,
                             Abandon_Semaphore,
                             Semaphore.Promised_To);
         if Semaphore.Probable_Owner /= null then
            Record_Transaction (Semaphore.all'Access,
                                Set_Probable_Owner,
                                Semaphore.Probable_Owner);
         end if;
         Signal (Semaphore.Promised_To);
         Semaphore.Promised_To := null;
      end if;

      --  Semaphore is not in use

      Semaphore.Status := Unlocked;

      --  Unlock the partition

      Partition_Mutex.Leave;

   end V;

   -----------
   -- Write --
   -----------

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    Str    : in String_Access)
   is
   begin
      if Str = null then
         Boolean'Output (Stream, False);
      else
         Boolean'Output (Stream, True);
         String'Output (Stream, Str.all);
      end if;
   end Write;

end GLADE.Semaphores;
