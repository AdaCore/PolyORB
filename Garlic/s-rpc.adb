------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                           S Y S T E M . R P C                            --
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
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with System.Garlic.Debug;        use System.Garlic.Debug;
with System.Garlic.Heart;        use System.Garlic.Heart;
with System.Garlic.Termination;
with System.Garlic.Utils;        use System.Garlic.Utils;
pragma Elaborate (System.Garlic.Utils);
with System.RPC.Pool;            use System.RPC.Pool;
with System.RPC.Util;            use System.RPC.Util;
with Ada.Exceptions;

package body System.RPC is

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("RPC", "(s-rpc   ): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   use Ada.Streams;
   --  A lot of shortcuts.

   Current_RPC_Receiver         : RPC_Receiver;
   pragma Atomic (Current_RPC_Receiver);
   Current_RPC_Receiver_Barrier : Barrier_Type;
   --  The current RPC receiver and its associated barrier.

   Min_Size : constant Stream_Element_Count := 1024;
   --  No packet below this size will be allowed.

   procedure Copy
     (Source : in out Params_Stream_Type;
      Dest   : access Params_Stream_Type);
   pragma Inline (Copy);
   --  Deep copy Source into Dest and read the original packet. This is
   --  needed to be able to drop the Params_Stream_Type without losing its
   --  content.

   type Request_Array is array (Request_Id) of System.RPC.Partition_Id;

   protected type Request_Id_Server_Type is
      entry Get  (Id : out Request_Id; Partition : in System.RPC.Partition_ID);
      procedure Free (Id : in Request_Id);
      procedure Raise_Partition_Error (Partition : in System.RPC.Partition_ID);
   private
      Latest      : Request_Id := Request_Id'First;
      Destination : Request_Array := (others => Null_Partition_ID);
      Count       : Request_Id := 0;
   end Request_Id_Server_Type;

   type Request_Id_Server_Access is access Request_Id_Server_Type;
   procedure Free is
      new Ada.Unchecked_Deallocation (Request_Id_Server_Type,
                                      Request_Id_Server_Access);

   Request_Id_Server : Request_Id_Server_Access :=
     new Request_Id_Server_Type;

   type Result_Type is record
      Result    : Params_Stream_Access;
      Cancelled : Boolean := False;
   end record;

   type Result_Array is array (Request_Id) of Result_Type;
   type Valid_Result_Array is array (Request_Id) of Boolean;

   protected type Result_Watcher_Type is
      procedure Set (Id : in Request_Id; Result : in Result_Type);
      procedure Get (Id : in Request_Id; Result : out Result_Type);
      procedure Invalidate (Id : in Request_Id);
      procedure Raise_Error (Id : in Request_Id);
      entry Wait (Request_Id);
   private
      Valid       : Valid_Result_Array := (others => False);
      Cancelled   : Valid_Result_Array := (others => False);
      Results     : Result_Array;
      procedure Free (Id : in Request_Id);
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

   procedure Public_RPC_Receiver
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access System.RPC.Params_Stream_Type);
   --  Receive data.

   procedure Shutdown;
   task Shutdown_Waiter;
   --  Shutdown mechanism.

   procedure Partition_Error_Notification
     (Partition : in System.RPC.Partition_ID);
   --  Call this procedure to unblock tasks waiting for RPC results from
   --  a dead partition.

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

   ------------
   -- Do_APC --
   ------------

   procedure Do_APC
     (Partition : in Partition_ID;
      Params    : access Params_Stream_Type)
   is
      Header : constant Request_Header := (Kind => APC_Request);
   begin
      pragma Debug
        (D (D_Debug, "Doing a APC for partition" & Partition'Img));
      --  if Partition = Get_My_Partition_ID then
      --     pragma Debug (D (D_Debug, "Cannot yet handle All_Calls_Remote");
      --     raise Communication_Error;
      --  end if;
      Insert_Request (Params, Header);
      Partition_ID'Write (Params, Partition);
      Any_Priority'Write (Params, Ada.Dynamic_Priorities.Get_Priority);
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
      pragma Debug
        (D (D_Debug, "Doing a RPC for partition" & Partition'Img));
      --  if Partition = Get_My_Partition_ID then
      --     pragma Debug (D (D_Debug, "Cannot yet handle All_Calls_Remote");
      --     raise Communication_Error;
      --  end if;
      begin
         pragma Abort_Defer;
         Request_Id_Server.Get (Id, Partition);
         Header.Id := Id;
         Insert_Request (Params, Header);
         Partition_ID'Write (Params, Partition);
         Any_Priority'Write (Params, Ada.Dynamic_Priorities.Get_Priority);
         Send (Partition, Remote_Call, Params);
         Keeper.Id        := Id;
         Keeper.Partition := Partition;
         Keeper.Sent      := True;
      end;
      pragma Debug (D (D_Debug, "Waiting for the result"));
      Result_Watcher.Wait (Id);
      begin
         pragma Abort_Defer;
         pragma Debug (D (D_Debug, "The result is available"));
         Keeper.Sent := False;
         Result_Watcher.Get (Id, Res);
         pragma Assert (not Res.Cancelled);
         Request_Id_Server.Free (Id);
         pragma Debug (D (D_Debug, "Copying the result"));
         pragma Assert (Res.Result /= null);
         Copy (Res.Result.all, Result);
         Free (Res.Result);
      end;
      pragma Debug (D (D_Debug, "Returning from Do_RPC"));
   end Do_RPC;

   ----------------------------
   -- Establish_RPC_Receiver --
   ----------------------------

   procedure Establish_RPC_Receiver
     (Partition : in Partition_ID;
      Receiver  : in RPC_Receiver)
   is
   begin
      pragma Debug
        (D (D_Debug, "Setting RPC receiver for partition" & Partition'Img));
      Current_RPC_Receiver := Receiver;
      Current_RPC_Receiver_Barrier.Signal_All (Permanent => True);
      Register_Partition_Error_Notification
        (Partition_Error_Notification'Access);
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
         pragma Debug (D (D_Debug, "Will invalidate object" & Id'Img));
         Result_Watcher.Invalidate (Id);
      end if;
   end Finalize;

   ----------------------
   -- Get_RPC_Receiver --
   ----------------------

   function Get_RPC_Receiver return RPC_Receiver is
   begin
      if Current_RPC_Receiver = null then
         Current_RPC_Receiver_Barrier.Wait;
      end if;
      return Current_RPC_Receiver;
   end Get_RPC_Receiver;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Receive (Remote_Call, Public_RPC_Receiver'Access);
   end Initialize;

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

   ----------------------------------
   -- Partition_Error_Notification --
   ----------------------------------

   procedure Partition_Error_Notification
     (Partition : in System.RPC.Partition_ID) is
   begin
      Request_Id_Server.Raise_Partition_Error (Partition);
   end Partition_Error_Notification;

   -------------------------
   -- Public_RPC_Receiver --
   -------------------------

   procedure Public_RPC_Receiver
     (Partition : in Partition_ID;
      Operation : in Public_Opcode;
      Params    : access System.RPC.Params_Stream_Type) is
      Header : constant Request_Header := Request_Header'Input (Params);
   begin

      case Header.Kind is

         when RPC_Request | APC_Request =>

            declare
               Params_Copy  : Params_Stream_Access :=
                 new Params_Stream_Type (Params.Initial_Size);
               Id           : Request_Id := Request_Id'First;
               Asynchronous : constant Boolean := Header.Kind = APC_Request;
            begin
               pragma Debug
                 (D (D_Debug,
                     "RPC or APC request received from partition" &
                     Partition'Img));
               Copy (Params.all, Params_Copy);
               if not Asynchronous then
                  Id := Header.Id;
                  pragma Debug
                    (D (D_Debug,
                        "(request id is" & Id'Img & ")"));
               end if;
               Allocate_Task (Partition, Id, Params_Copy, Asynchronous);
            end;

         when RPC_Answer =>
            declare
               Result : Result_Type;
            begin
               pragma Debug
                 (D (D_Debug,
                     "RPC answer received from partition" &
                     Partition'Img & " (request" & Header.Id'Img & ")"));
               Result.Result := new Params_Stream_Type (Params.Initial_Size);
               Copy (Params.all, Result.Result);
               pragma Debug
                 (D (D_Debug, "Signaling that the result is available"));
               Result_Watcher.Set (Header.Id, Result);
            end;

         when RPC_Request_Cancellation =>
            pragma Debug
                  (D (D_Debug,
                      "RPC cancellation request received from partition" &
                      Partition'Img & " (request" & Header.Id'Img & ")"));
            Abort_Task (Partition, Header.Id);

         when RPC_Cancellation_Accepted =>
            declare
               Result : Result_Type;
            begin
               pragma Debug
                  (D (D_Debug,
                      "RPC cancellation ack received from partition" &
                      Partition'Img & " (request" & Header.Id'Img & ")"));
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
      if First = null then

         --  Set Current to null to allow further Write to be done.

         Stream.Current := null;

      end if;
   end Read;

   ----------------------------
   -- Request_Id_Server_Type --
   ----------------------------

   protected body Request_Id_Server_Type is

      ---------------------------
      -- Raise_Partition_Error --
      ---------------------------

      procedure Raise_Partition_Error
        (Partition : in System.RPC.Partition_ID) is
      begin
         for Id in Request_Id'First .. Latest loop
            if Destination (Id) = Partition then
               pragma Warnings (Off);  --  XXXXX
               Result_Watcher.Raise_Error (Id);
               pragma Warnings (On);
               Free (Id);
            end if;
         end loop;
      end Raise_Partition_Error;

      ----------
      -- Free --
      ----------

      procedure Free (Id : in Request_Id) is
      begin
         if Destination (Id) /= Null_Partition_ID then
            Destination (Id) := Null_Partition_ID;
            Count := Count - 1;
         end if;
      end Free;

      ---------
      -- Get --
      ---------

      entry Get (Id : out Request_Id; Partition : in Partition_ID)
      when Count < Request_Id'Last is
      begin
         while Destination (Latest) /= Null_Partition_ID loop
            Latest := Latest + 1;
         end loop;
         Id := Latest;
         Destination (Id) := Partition;
         Count  := Count + 1;
         Latest := Latest + 1;
      end Get;

   end Request_Id_Server_Type;

   -------------------------
   -- Result_Watcher_Type --
   -------------------------

   protected body Result_Watcher_Type is

      -----------------
      -- Raise_Error --
      -----------------

      procedure Raise_Error (Id : in Request_Id) is
      begin
         if not Valid (Id) then
            Cancelled (Id) := True;
            Valid (Id) := True;
         end if;
      end Raise_Error;

      ----------
      -- Free --
      ----------

      procedure Free (Id : in Request_Id) is
         Res : Result_Type;
      begin
         Get (Id, Res);
         Deep_Free (Res.Result);
         pragma Warnings (Off);  --  XXXXX
         Request_Id_Server.Free (Id);
         pragma Warnings (On);
      end Free;

      ---------
      -- Get --
      ---------

      procedure Get
        (Id     : in Request_Id;
         Result : out Result_Type) is
      begin
         pragma Assert (Valid (Id));
         if Cancelled (Id) then
            raise Communication_Error;
         end if;
         Result := Results (Id);
         Valid (Id) := False;
         Results (Id) .Result := null;
      end Get;

      ----------------
      -- Invalidate --
      ----------------

      procedure Invalidate (Id : in Request_Id) is
      begin
         if Valid (Id) then
            Free (Id);
         else
            Cancelled (Id) := True;
         end if;
      end Invalidate;

      ---------
      -- Set --
      ---------

      procedure Set (Id : in Request_Id; Result : in Result_Type) is
      begin
         Valid (Id) := True;
         Results (Id) := Result;
         if Cancelled (Id) then
            Cancelled (Id) := False;
            Free (Id);
         end if;
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
      pragma Debug (D (D_Debug, "Sending abortion message"));
      Insert_Request (Params'Access, Header);
      Send (Partition, Remote_Call, Params'Access);
   end Send_Abort_Message;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      Free (Request_Id_Server);
      Free (Result_Watcher);
      Pool.Shutdown;
   end Shutdown;

   ---------------------
   -- Shutdown_Waiter --
   ---------------------

   task body Shutdown_Waiter is
   begin
      select
         Shutdown_Keeper.Wait;
         pragma Debug
           (D (D_Debug, "Shutdown Waiter exiting because of Shutdown_Keeper"));
         raise Communication_Error;
      then abort
         Wait_Until_Elaboration_Is_Terminated;
      end select;
      System.Garlic.Termination.Add_Non_Terminating_Task;
      Shutdown_Keeper.Wait;
      Shutdown;
      System.Garlic.Termination.Sub_Non_Terminating_Task;
   end Shutdown_Waiter;

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
         if Stream.First = null then

            --  This is the first call (maybe after a full read).

            Stream.First :=
              new Node (Stream_Element_Count'Max
                        (Stream.Initial_Size,
                         Stream_Element_Count'Max (Min_Size, Length)));
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
