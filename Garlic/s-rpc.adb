------------------------------------------------------------------------------
--                                                                          --
--                           GARLIC COMPONENTS                              --
--                                                                          --
--                          S Y S T E M . R P C                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            1.25                             --
--                                                                          --
--           Copyright (C) 1996 Free Software Foundation, Inc.              --
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
--               GARLIC is maintained by ACT Europe.                        --
--            (email:distribution@act-europe.gnat.com).                     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Dynamic_Priorities;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with System.Garlic.Caching;
with System.Garlic.Debug; use System.Garlic.Debug;
with System.Garlic.Heart; use System.Garlic.Heart;
with System.Garlic.Termination;

package body System.RPC is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("RPC", "(s-rpc   ): ");
   procedure D
     (Level   : in Debug_Levels;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use Ada.Streams;
   --  A lot of shortcuts.

   Min_Size : constant Stream_Element_Count := 1024;
   --  No packet below this size will be allowed.

   procedure Free is
      new Ada.Unchecked_Deallocation (Node, Node_Ptr);

   type Params_Stream_Access is access all Params_Stream_Type;

   procedure Copy
     (Source : in out Params_Stream_Type;
      Dest   : access Params_Stream_Type);
   pragma Inline (Copy);
   --  Deep copy Source into Dest and read the original packet. This is
   --  needed to be able to drop the Params_Stream_Type without losing its
   --  content.

   procedure Free is
      new Ada.Unchecked_Deallocation
          (Params_Stream_Type, Params_Stream_Access);
   --  Deallocate a Params_Stream_Access.

   procedure Deep_Free (Stream : in out Params_Stream_Access);
   --  This procedure make sure that unconsumed data has been freed. This
   --  may occur in case of cancellation.

   type RPC_Opcode is (RPC_Request,
                       RPC_Answer,
                       RPC_Request_Cancellation,
                       RPC_Cancellation_Accepted,
                       APC_Request);
   --  Type of operation.

   type Request_Id is mod 2 ** 8;
   --  Request_Id.

   type Request_Header (Kind : RPC_Opcode) is record
      case Kind is
         when RPC_Request | RPC_Answer |
           RPC_Request_Cancellation | RPC_Cancellation_Accepted =>
            Id : Request_Id;
         when APC_Request =>
            null;
      end case;
   end record;

   type Request_Array is array (Request_Id) of Boolean;

   protected type Request_Id_Server_Type is
      entry Get  (Id : out Request_Id);
      procedure Free (Id : in Request_Id);
   private
      Latest : Request_Id := Request_Id'First;
      In_Use : Request_Array := (others => False);
      Count  : Request_Id := 0;
   end Request_Id_Server_Type;

   type Request_Id_Server_Access is access Request_Id_Server_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Request_Id_Server_Type,
                                      Request_Id_Server_Access);

   Request_Id_Server : Request_Id_Server_Access :=
     new Request_Id_Server_Type;

   procedure Insert_Request
     (Params : access Params_Stream_Type;
      Header : in Request_Header);
   --  This procedure adds a Request_Header in front of Params.

   package Receiver_Map is
      new System.Garlic.Caching (Index_Type => Partition_ID,
                                 Data_Type  => RPC_Receiver,
                                 Unset      => null);

   type Result_Type is record
      Result    : Params_Stream_Access;
      Cancelled : Boolean := False;
   end record;

   type Result_Array is array (Request_Id) of Result_Type;
   type Valid_Result_Array is array (Request_Id) of Boolean;

   protected type Result_Watcher_Type is
      procedure Set (Id : in Request_Id; Result : in Result_Type);
      procedure Get (Id : in Request_Id; Result : out Result_Type);
      entry Wait (Request_Id);
   private
      Valid       : Valid_Result_Array := (others => False);
      Results     : Result_Array;
   end Result_Watcher_Type;

   type Result_Watcher_Access is access Result_Watcher_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Result_Watcher_Type,
                                      Result_Watcher_Access);

   Result_Watcher : Result_Watcher_Access :=
     new Result_Watcher_Type;

   type Abort_Keeper is new Ada.Finalization.Controlled with record
      Sent      : Boolean := False;
      Partition : Partition_ID;
      Id        : Request_Id;
   end record;
   procedure Finalize (Keeper : in out Abort_Keeper);
   --  Handle abortion from Do_RPC.

   procedure Send_Abort_Message
     (Partition : in Partition_ID;
      Id        : in Request_Id);
   --  Send an abort message for a request.

   Public_Receiver_Is_Installed : Boolean := False;

   protected Public_Receiver_Installed is
      procedure Check;
      entry Wait;
   end Public_Receiver_Installed;
   --  This is redundant, but this is the fastest way to do this. This is
   --  called whenever Establish_RPC_Receiver, Do_APC and Do_RPC are called
   --  since we don't need to set it before this.

   procedure Public_RPC_Receiver
     (Partition : in Partition_ID;
      Operation : in Opcode;
      Params    : access System.RPC.Params_Stream_Type);
   --  Receive data.

   task type Anonymous_Task (Partition    : Partition_ID;
                             Id           : Request_Id;
                             Params       : Params_Stream_Access;
                             Asynchronous : Boolean) is
      pragma Storage_Size (150_000);
   end Anonymous_Task;
   type Anonymous_Task_Access is access Anonymous_Task;
   --  An anonymous task will serve a request.

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
   end Task_Pool_Type;
   --  This protected object requeues on Is_Aborted_Waiting; this may look
   --  inefficient, but we hope that remote abortion won't occur too much
   --  (or at least that remote abortion won't occur too often when there is
   --  a lot of other remote calls in progress).

   type Task_Pool_Access is access Task_Pool_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Task_Pool_Type, Task_Pool_Access);

   Task_Pool : Task_Pool_Access := new Task_Pool_Type;

   procedure Shutdown;
   task Shutdown_Waiter;
   --  Shutdown mechanism.

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
   begin
      D (D_Debug, "Anonymous task starting");
      Task_Pool.Get_One;
      Task_Pool.Unabort_One (Partition, Id);
      Partition_ID'Read (Params, Dest);
      if not Dest'Valid then
         D (D_Debug, "Invalid destination received");
         raise Constraint_Error;
      end if;
      Any_Priority'Read (Params, Prio);
      if not Prio'Valid then
         D (D_Debug, "Invalid priority received");
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
            Insert_Request (Empty'Access, Header);
            Send (Partition, Remote_Call, Empty'Access);
            Cancelled := True;
         end;
      then abort
         Receiver (Params, Result);
      end select;

      declare
         Params_Copy : Params_Stream_Access := Params;
      begin

         --  Yes, we deallocate a copy, because Params is readonly (it's
         --  a discriminant). We must *not* use Params later in this task.

         Deep_Free (Params_Copy);
      end;
      if Asynchronous or else Cancelled then
         Deep_Free (Result);
      else
         declare
            Header : constant Request_Header := (RPC_Answer, Id);
         begin
            Insert_Request (Result, Header);
            Send (Partition, Remote_Call, Result);
            Free (Result);
         end;
      end if;
      Task_Pool.Free_One;
      D (D_Debug, "Anonymous task finishing");

   exception
      when others =>
         D (D_Debug, "Error in anonymous task");

   end Anonymous_Task;

   ----------
   -- Copy --
   ----------

   procedure Copy
     (Source : in out Params_Stream_Type;
      Dest   : access Params_Stream_Type)
   is
   begin
      if Dest.First /= null then
         Free (Dest.First);
      end if;
      Dest.First         := Source.First;
      Dest.Current       := Source.First;
      Dest.Special_First := Source.Special_First;
      Dest.Count         := Source.Count;
      Source.First       := null;
   end Copy;

   ---------------
   -- Deep_Free --
   ---------------

   procedure Deep_Free (Stream : in out Params_Stream_Access) is
      Next : Node_Ptr;
   begin
      if Stream = null then
         return;
      end if;
      while Stream.First /= null loop
         Next := Stream.First.Next;
         Free (Stream.First);
         Stream.First := Next;
      end loop;
      Free (Stream);
   end Deep_Free;

   ------------
   -- Do_APC --
   ------------

   procedure Do_APC
     (Partition : in Partition_ID;
      Params    : access Params_Stream_Type)
   is
      Header : constant Request_Header := (Kind => APC_Request);
   begin
      D (D_Debug,
         "Doing a APC for partition" & Partition_ID'Image (Partition));
      if Partition = Get_My_Partition_ID then
         D (D_Debug, "Cannot yet handle All_Calls_Remote");
         raise Communication_Error;
      end if;
      if not Public_Receiver_Is_Installed then
         D (D_Debug, "Checking the the GARLIC receiver is installed");
         Public_Receiver_Installed.Check;
      end if;
      Insert_Request (Params, Header);
      Partition_ID'Write (Params, Partition);
      Send (Partition, Remote_Call, Params);
   end Do_APC;

   ------------
   -- Do_RPC --
   ------------

   procedure Do_RPC
     (Partition  : in Partition_ID;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type)
   is
      Id     : Request_Id;
      Header : Request_Header (RPC_Request);

      Res    : Result_Type;

      Keeper : Abort_Keeper;

   begin
      D (D_Debug,
         "Doing a RPC for partition" & Partition_ID'Image (Partition));
      if Partition = Get_My_Partition_ID then
         D (D_Debug, "Cannot yet handle All_Calls_Remote");
         raise Communication_Error;
      end if;
      if not Public_Receiver_Is_Installed then
         D (D_Debug, "Checking the the GARLIC receiver is installed");
         Public_Receiver_Installed.Check;
      end if;
      begin
         pragma Abort_Defer;
         Request_Id_Server.Get (Id);
         Header.Id := Id;
         Insert_Request (Params, Header);
         Partition_ID'Write (Params, Partition);
         Any_Priority'Write (Params, Ada.Dynamic_Priorities.Get_Priority);
         Send (Partition, Remote_Call, Params);
         Keeper.Id        := Id;
         Keeper.Partition := Partition;
         Keeper.Sent      := True;
      end;
      D (D_Debug, "Waiting for the result");
      Result_Watcher.Wait (Id);
      D (D_Debug, "The result is available");
      begin
         pragma Abort_Defer;
         Result_Watcher.Get (Id, Res);
         Keeper.Sent := False;
         Request_Id_Server.Free (Id);
         D (D_Debug, "Copying the result");
         Copy (Res.Result.all, Result);
         Free (Res.Result);
      end;
      D (D_Debug, "Returning from Do_RPC");
   end Do_RPC;

   ----------------------------
   -- Establish_RPC_Receiver --
   ----------------------------

   procedure Establish_RPC_Receiver
     (Partition : in Partition_ID;
      Receiver  : in RPC_Receiver)
   is
   begin
      if not Public_Receiver_Is_Installed then
         D (D_Debug, "Checking installation of GARLIC receiver");
         Public_Receiver_Installed.Check;
      end if;
      D (D_Debug,
         "Setting RPC receiver for partition" &
         Partition_ID'Image (Partition));
      Receiver_Map.Set (Partition, Receiver);
   end Establish_RPC_Receiver;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Keeper : in out Abort_Keeper) is
      Id  : Request_Id renames Keeper.Id;
      Res : Result_Type;
   begin
      if Keeper.Sent then
         Send_Abort_Message (Keeper.Partition, Id);
         Result_Watcher.Wait (Id);
         Result_Watcher.Get (Id, Res);
         Deep_Free (Res.Result);
         Request_Id_Server.Free (Id);
      end if;
   end Finalize;

   --------------------
   -- Insert_Request --
   --------------------

   procedure Insert_Request
     (Params : access Params_Stream_Type;
      Header : in Request_Header)
   is
   begin
      Params.Special_First := True;
      Request_Header'Output (Params, Header);
   end Insert_Request;

   -------------------------------
   -- Public_Receiver_Installed --
   -------------------------------

   protected body Public_Receiver_Installed is

      -----------
      -- Check --
      -----------

      procedure Check is
      begin
         Receive (Remote_Call, Public_RPC_Receiver'Access);
         Public_Receiver_Is_Installed := True;
      end Check;

      ----------
      -- Wait --
      ----------

      entry Wait when Public_Receiver_Is_Installed is
      begin
         null;
      end Wait;

   end Public_Receiver_Installed;

   -------------------------
   -- Public_RPC_Receiver --
   -------------------------

   procedure Public_RPC_Receiver
     (Partition : in Partition_ID;
      Operation : in Opcode;
      Params    : access System.RPC.Params_Stream_Type) is
      Header : constant Request_Header := Request_Header'Input (Params);
   begin

      case Header.Kind is

         when RPC_Request | APC_Request =>

            --  First make sure that our elaboration is finished before
            --  handling any RPC call. We do not make the check for
            --  RPC answers because we do not want to stay locked if
            --  we have requested something ourselves.

            Wait_Until_Elaboration_Is_Terminated;

            declare
               Params_Copy  : Params_Stream_Access :=
                 new Params_Stream_Type (Params.Initial_Size);
               Anonymous    : Anonymous_Task_Access;
               Id           : Request_Id := Request_Id'First;
               Asynchronous : constant Boolean := Header.Kind = APC_Request;
            begin
               D (D_Debug,
                  "RPC or APC request received from partition" &
                  Partition_ID'Image (Partition));
               Copy (Params.all, Params_Copy);
               if not Asynchronous then
                  Id := Header.Id;
               end if;
               Anonymous := new Anonymous_Task (Partition,
                                                Id,
                                                Params_Copy,
                                                Asynchronous);
            end;

         when RPC_Answer =>
            declare
               Result : Result_Type;
            begin
               D (D_Debug,
                  "RPC answer received from partition" &
                  Partition_ID'Image (Partition));
               Result.Result := new Params_Stream_Type (Params.Initial_Size);
               Copy (Params.all, Result.Result);
               D (D_Debug, "Signaling that the result is available");
               Result_Watcher.Set (Header.Id, Result);
            end;

         when RPC_Request_Cancellation =>
            D (D_Debug,
               "RPC cancellation request received from partition" &
               Partition_ID'Image (Partition));
            Task_Pool.Abort_One (Partition, Header.Id);

         when RPC_Cancellation_Accepted =>
            declare
               Result : Result_Type;
            begin
               D (D_Debug,
                  "RPC cancellation ack received from partition" &
                  Partition_ID'Image (Partition));
               Result.Cancelled := True;
               Result_Watcher.Set (Header.Id, Result);
            end;

      end case;
   end Public_RPC_Receiver;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Params_Stream_Type;
      Item   : out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      First  : Node_Ptr renames Stream.First;
      Other  : Node_Ptr;
      Offset : Stream_Element_Offset := Item'First;
      Count  : Stream_Element_Count := Item'Length;
      Length : Stream_Element_Count;
   begin
      while First /= null and then Count > 0 loop
         Length := Stream_Element_Count'Min (First.Last - First.Current,
                                             Count);
         Item (Offset .. Offset + Length - 1) :=
           First.Content (First.Current .. First.Current + Length - 1);
         Count := Count - Length;
         Offset := Offset + Length;
         First.Current := First.Current + Length;
         if First.Current >= First.Last then
            Other := First;
            First := First.Next;
            Free (Other);
         end if;
      end loop;
      Last := Offset - 1;
      Stream.Count := Stream.Count - (Offset - Item'First);
   end Read;

   ----------------------------
   -- Request_Id_Server_Type --
   ----------------------------

   protected body Request_Id_Server_Type is

      ----------
      -- Free --
      ----------

      procedure Free (Id : in Request_Id) is
      begin
         if In_Use (Id) then
            In_Use (Id) := False;
            Count := Count - 1;
         end if;
      end Free;

      ---------
      -- Get --
      ---------

      entry Get (Id : out Request_Id)
      when Count < Request_Id'Last is
      begin
         while In_Use (Latest) loop
            Latest := Latest + 1;
         end loop;
         Id := Latest;
         In_Use (Id) := True;
         Count := Count + 1;
      end Get;

   end Request_Id_Server_Type;

   -------------------------
   -- Result_Watcher_Type --
   -------------------------

   protected body Result_Watcher_Type is

      ---------
      -- Get --
      ---------

      procedure Get (Id : in Request_Id; Result : out Result_Type) is
      begin
         if Valid (Id) then
            Result := Results (Id);
            Valid (Id) := False;
            Results (Id) .Result := null;
         end if;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (Id : in Request_Id; Result : in Result_Type) is
      begin
         Valid (Id) := True;
         Results (Id) := Result;
      end Set;

      ----------
      -- Wait --
      ----------

      entry Wait (for Id in Request_Id)
      when Valid (Id) is
      begin
         null;
      end Wait;

   end Result_Watcher_Type;

   ------------------------
   -- Send_Abort_Message --
   ------------------------

   procedure Send_Abort_Message
     (Partition : in Partition_ID;
      Id        : in Request_Id) is
      Params : aliased Params_Stream_Type (0);
      Header : constant Request_Header := (RPC_Request_Cancellation, Id);
   begin
      D (D_Debug, "Sending abortion message");
      Insert_Request (Params'Access, Header);
      Send (Partition, Remote_Call, Params'Access);
   end Send_Abort_Message;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Free (Request_Id_Server);
      Receiver_Map.Die;
      Free (Result_Watcher);
      Free (Task_Pool);
   end Shutdown;

   ---------------------
   -- Shutdown_Waiter --
   ---------------------

   task body Shutdown_Waiter is
   begin
      select
         Shutdown_Keeper.Wait;
         D (D_Debug, "Shutdown Waiter exiting because of Shutdown_Keeper");
         raise Communication_Error;
      then abort
         Public_Receiver_Installed.Wait;
      end select;
      System.Garlic.Termination.Add_Non_Terminating_Task;
      Shutdown_Keeper.Wait;
      Shutdown;
      System.Garlic.Termination.Sub_Non_Terminating_Task;
   end Shutdown_Waiter;

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
               if Is_Aborted_Waiting'Count > 0 then
                  In_Progress := True;
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
      when not In_Progress is
      begin
         for I in Cancel_Map'Range loop
            declare
               Ent : Cancel_Type renames Cancel_Map (I);
            begin
               if Ent.Valid and then Ent.Id = Id and then
                 Ent.Partition = Partition then
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
               end if;
            end;
         end loop;
      end Unabort_One;

   end Task_Pool_Type;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Params_Stream_Type;
      Item   : in Stream_Element_Array)
   is
      Length  : constant Stream_Element_Count := Item'Length;
      Current : Node_Ptr renames Stream.Current;
   begin
      if Current = null then
         if Stream.First.Size = 0 then

            --  This is the first call and the initial size was specified
            --  as being zero. We do not need to keep this packet, we will
            --  allocate another (non-empty) one immediately.

            Free (Stream.First);
            Stream.First :=
              new Node (Stream_Element_Count'Max (Min_Size, Length));
         end if;
         Current := Stream.First;
      end if;

      Stream.Count := Stream.Count + Item'Length;

      if Stream.Special_First then

         --  We make a special handling to add a header in front of
         --  the packet. Current points to the head, and new packets
         --  (if needed) will be added in order in front of regular
         --  packets.

         declare
            Special : Node_Ptr :=
             new Node (Stream_Element_Count'Max (Min_Size, Length));
         begin
            Special.Next := Stream.First;
            Stream.First := Special;
            Current := Stream.First;
            Current.Content (1 .. Length) := Item;
            Current.Last := Length + 1;
            Stream.Special_First := False;
            Stream.Count := Stream.Count + Length;
            return;
         end;
      end if;
      if Length + Current.Last - 1 > Current.Size then
         declare
            Old_Next : constant Node_Ptr := Current.Next;
         begin

            --  We chain to the 'Current' packet, while preserving the
            --  original Next field. This is used in the case where we
            --  insert the header at the beginning of the Stream.

            Current.Next :=
             new Node (Stream_Element_Count'Max (Min_Size, Length));
            Current := Current.Next;
            Current.Next := Old_Next;
         end;
      end if;
      Current.Content (Current.Last .. Current.Last + Length - 1) :=
        Item;
      Current.Last := Current.Last + Length;
   end Write;

end System.RPC;
