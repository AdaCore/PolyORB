------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                      S Y S T E M . R P C . P O O L                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$                             --
--                                                                          --
--         Copyright (C) 1996,1997 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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

with Ada.Dynamic_Priorities;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with System.Garlic.Debug;        use System.Garlic.Debug;
with System.Garlic.Heart;        use System.Garlic.Heart;

package body System.RPC.Pool is

   use System.RPC.Util;

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("POO", "(s-rpcpoo): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   Max_Tasks : constant := 512;

   type Cancel_Type is record
      Partition : Partition_ID;
      Id        : Request_Id;
      Valid     : Boolean := False;
   end record;

   type Cancel_Array is array (1 .. Max_Tasks) of Cancel_Type;

   protected type Task_Pool_Type is
      entry Get_One;
      procedure Free_One;
      procedure Abort_One
        (Partition : in Partition_ID;
         Id        : in Request_Id);
      procedure Unabort_One
        (Partition : in Partition_ID;
         Id        : in Request_Id);
      entry Is_Aborted (Partition : in Partition_ID; Id : in Request_Id);
   private
      entry Is_Aborted_Waiting
        (Partition : in Partition_ID; Id : in Request_Id);
      Cancel_Map  : Cancel_Array;
      In_Progress : Boolean := False;
      Count       : Natural := 0;
      Count_Abort : Natural := 0;
   end Task_Pool_Type;
   --  This protected object requeues on Is_Aborted_Waiting; this may look
   --  inefficient, but we hope that remote abortion won't occur too much
   --  (or at least that remote abortion won't occur too often when there is
   --  a lot of other remote calls in progress). Count_Abort contains the
   --  number of abortion in progress.

   type Task_Pool_Access is access Task_Pool_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Task_Pool_Type, Task_Pool_Access);

   Task_Pool : Task_Pool_Access := new Task_Pool_Type;

   task type Anonymous_Task (Partition    : Partition_ID;
                             Id           : Request_Id;
                             Params       : Params_Stream_Access;
                             Asynchronous : Boolean) is
      pragma Storage_Size (300_000);
   end Anonymous_Task;
   type Anonymous_Task_Access is access Anonymous_Task;
   --  An anonymous task will serve a request.

   ----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (Partition : in Partition_ID;
                         Id        : in Request_Id)
   is
   begin
      Task_Pool.Abort_One (Partition, Id);
   end Abort_Task;

   -------------------
   -- Allocate_Task --
   -------------------

   procedure Allocate_Task (Partition    : in Partition_ID;
                            Id           : in Request_Id;
                            Params       : in Params_Stream_Access;
                            Asynchronous : in Boolean)
   is
      Anonymous : Anonymous_Task_Access;
   begin
      Anonymous := new Anonymous_Task (Partition, Id, Params, Asynchronous);
   end Allocate_Task;

   --------------------
   -- Anonymous_Task --
   --------------------

   task body Anonymous_Task
   is
      Dest      : Partition_ID;
      Receiver  : RPC_Receiver;
      Result    : Params_Stream_Access := new Params_Stream_Type (0);
      Cancelled : Boolean := False;
      Prio      : Any_Priority;

      use Ada.Exceptions;
   begin
      pragma Debug (D (D_Debug, "Anonymous task starting"));
      Task_Pool.Get_One;
      Task_Pool.Unabort_One (Partition, Id);
      Partition_ID'Read (Params, Dest);
      if not Dest'Valid then
         pragma Debug (D (D_Debug, "Invalid destination received"));
         raise Constraint_Error;
      end if;
      Any_Priority'Read (Params, Prio);
      if not Prio'Valid then
         pragma Debug (D (D_Debug, "Invalid priority received"));
         raise Constraint_Error;
      end if;
      Ada.Dynamic_Priorities.Set_Priority (Prio);
      Receiver := Receiver_Map.Get (Dest);
      if Receiver = null then

         --  Well, we won't query it, it should be automatically set.

         Receiver := Receiver_Map.Get (Dest);
      end if;
      select
         Task_Pool.Is_Aborted (Partition, Id);
         declare
            Empty  : aliased Params_Stream_Type (0);
            Header : constant Request_Header :=
              (RPC_Cancellation_Accepted, Id);
         begin
            pragma Debug (D (D_Debug, "Abortion queried by caller"));
            Insert_Request (Empty'Access, Header);
            Send (Partition, Remote_Call, Empty'Access);
            Cancelled := True;
         end;
      then abort
         Receiver (Params, Result);
         pragma Debug (D (D_Debug, "Job achieved without abortion"));
      end select;

      declare
         Params_Copy : Params_Stream_Access := Params;
      begin

         --  Yes, we deallocate a copy, because Params is readonly (it's
         --  a discriminant). We must *not* use Params later in this task.

         Deep_Free (Params_Copy);
      end;
      if Asynchronous or else Cancelled then
         pragma Debug (D (D_Debug, "Result not sent"));
         Deep_Free (Result);
      else
         declare
            Header : constant Request_Header := (RPC_Answer, Id);
         begin
            pragma Debug (D (D_Debug, "Result will be sent"));
            Insert_Request (Result, Header);
            Send (Partition, Remote_Call, Result);
            Free (Result);
         end;
      end if;
      Task_Pool.Free_One;
      pragma Debug (D (D_Debug, "Anonymous task finishing"));

   exception
      when E : others =>
         pragma Debug (D (D_Debug, "Error in anonymous task " &
                          "(exception " & Exception_Name (E) & ")"));
         null;

   end Anonymous_Task;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Free (Task_Pool);
   end Shutdown;

   --------------------
   -- Task_Pool_Type --
   --------------------

   protected body Task_Pool_Type is

      ---------------
      -- Abort_One --
      ---------------

      procedure Abort_One
        (Partition : in Partition_ID;
         Id        : in Request_Id)
      is
      begin
         for I in Cancel_Map'Range loop
            if not Cancel_Map (I) .Valid then
               Cancel_Map (I) := (Partition => Partition,
                                  Id        => Id,
                                  Valid     => True);
               Count_Abort := Count_Abort + 1;
               if Is_Aborted_Waiting'Count > 0 then
                  In_Progress := True;
                  pragma Debug (D (D_Debug, "Will signal abortion"));
               end if;
               return;
            end if;
         end loop;
      end Abort_One;

      --------------
      -- Free_One --
      --------------

      procedure Free_One is
      begin
         Count := Count - 1;
      end Free_One;

      -------------
      -- Get_One --
      -------------

      entry Get_One when Count < Max_Tasks is
      begin
         Count := Count + 1;
      end Get_One;

      ----------------
      -- Is_Aborted --
      ----------------

      entry Is_Aborted (Partition : in Partition_ID; Id : in Request_Id)
      when Count_Abort > 0 and then not In_Progress is
      begin
         for I in Cancel_Map'Range loop
            declare
               Ent : Cancel_Type renames Cancel_Map (I);
            begin
               if Ent.Valid and then Ent.Id = Id and then
                 Ent.Partition = Partition then
                  Count_Abort := Count_Abort - 1;
                  Ent.Valid := False;
                  return;
               end if;
            end;
         end loop;
         requeue Is_Aborted_Waiting with abort;
      end Is_Aborted;

      ------------------------
      -- Is_Aborted_Waiting --
      ------------------------

      entry Is_Aborted_Waiting
        (Partition : in Partition_ID; Id : in Request_Id)
      when In_Progress is
      begin
         if Is_Aborted_Waiting'Count = 0 then
            In_Progress := False;
         end if;
         requeue Is_Aborted with abort;
      end Is_Aborted_Waiting;

      -----------------
      -- Unabort_One --
      -----------------

      procedure Unabort_One
        (Partition : in Partition_ID;
         Id        : in Request_Id)
      is
      begin
         for I in Cancel_Map'Range loop
            declare
               Ent : Cancel_Type renames Cancel_Map (I);
            begin
               if Ent.Valid and then Ent.Id = Id and then
                 Ent.Partition = Partition then
                  Ent.Valid := False;
                  Count_Abort := Count_Abort - 1;
               end if;
            end;
         end loop;
      end Unabort_One;

   end Task_Pool_Type;

end System.RPC.Pool;
