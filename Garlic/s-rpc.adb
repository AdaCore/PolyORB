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
--         Copyright (C) 1996-1998 Free Software Foundation, Inc.           --
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
with Ada.Exceptions;             use Ada.Exceptions;
with Ada.Finalization;
with Ada.Unchecked_Deallocation;
with System.Garlic;              use System.Garlic;
with System.Garlic.Debug;        use System.Garlic.Debug;
with System.Garlic.Heart;        use System.Garlic.Heart;
pragma Elaborate_All (System.Garlic.Heart);
with System.Garlic.Soft_Links;   use System.Garlic.Soft_Links;
with System.Garlic.Startup;
pragma Warnings (Off, System.Garlic.Startup);
with System.Garlic.Streams;
with System.Garlic.Types;
with System.Garlic.Units;        use System.Garlic.Units;
with System.Garlic.Utils;        use System.Garlic.Utils;
with System.RPC.Pool;            use System.RPC.Pool;
pragma Elaborate (System.RPC.Pool);
with System.RPC.Stream_IO;
pragma Elaborate (System.RPC.Stream_IO);

package body System.RPC is

   use Ada.Streams;
   use System.Garlic.Units.Table;
   use type System.Garlic.Streams.Params_Stream_Access;
   use type System.Garlic.Streams.Params_Stream_Type;
   use type System.Garlic.Types.Partition_ID;

   --  This package needs extra comments ???

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_RPC", "(s-rpc   ): ");
   procedure D
     (Level   : in Debug_Level;
      Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   RPC_Allowed : Boolean := False;
   RPC_Barrier : Barrier_Type;
   --  The current RPC receiver and its associated barrier

   type Request_Array is array (Request_Id) of Types.Partition_ID;

   protected type Request_Id_Server_Type is
      entry Get  (Id : out Request_Id; Partition : in Types.Partition_ID);
      procedure Free (Id : in Request_Id);
      procedure Raise_Partition_Error (Partition : in Types.Partition_ID);
   private
      Latest      : Request_Id := Request_Id'First;
      Destination : Request_Array :=
        (others => System.Garlic.Types.Null_Partition_ID);
      Count       : Request_Id := 0;
   end Request_Id_Server_Type;

   type Request_Id_Server_Access is access Request_Id_Server_Type;
   procedure Free is
     new Ada.Unchecked_Deallocation
     (Request_Id_Server_Type, Request_Id_Server_Access);

   Request_Id_Server : Request_Id_Server_Access :=
     new Request_Id_Server_Type;
   --  Kludge to raise Program_Error at deallocation time. To be removed
   --  in the future ???

   type Result_Type is record
      Result    : Streams.Params_Stream_Access;
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
     new Ada.Unchecked_Deallocation
     (Result_Watcher_Type, Result_Watcher_Access);

   Result_Watcher : Result_Watcher_Access := new Result_Watcher_Type;

   type Abort_Keeper is new Ada.Finalization.Controlled with record
      Sent      : Boolean := False;
      Partition : Types.Partition_ID;
      Id        : Request_Id;
   end record;

   procedure Invalidate_RCI_Units (Partition : in Types.Partition_ID);
   pragma Inline (Invalidate_RCI_Units);
   --  For a given partition, send an invalidation message to the boot
   --  server and unregister its RCI units locally.

   procedure Finalize (Keeper : in out Abort_Keeper);
   --  Handle abortion from Do_RPC

   procedure Send_Abort_Message
     (Partition : in Types.Partition_ID;
      Id        : in Request_Id);
   --  Send an abort message for a request

   procedure Public_RPC_Receiver
     (Partition : in Types.Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Streams.Params_Stream_Type);
   --  Receive data

   procedure Shutdown;
   --  Shutdown System.RPC and its private child packages

   procedure Partition_Error_Notification
     (Partition : in Types.Partition_ID);
   --  Call this procedure to unblock tasks waiting for RPC results from
   --  a dead partition.

   ------------
   -- Do_APC --
   ------------

   procedure Do_APC
     (Partition : in Partition_ID;
      Params    : access Params_Stream_Type) is
      Header : constant Request_Header := (Kind => APC_Request);
   begin
      pragma Debug
        (D (D_Debug, "Doing a APC for partition" & Partition'Img));
      Insert_Request (Params.X'Access, Header);
      Partition_ID'Write (Params, Partition);
      Any_Priority'Write (Params, Ada.Dynamic_Priorities.Get_Priority);
      Send (Types.Partition_ID (Partition), Remote_Call, Params.X'Access);
   exception
      when E : System.Garlic.Communication_Error =>
         Raise_Exception (Communication_Error'Identity,
                          Exception_Message (E));
   end Do_APC;

   ------------
   -- Do_RPC --
   ------------

   procedure Do_RPC
     (Partition  : in Partition_ID;
      Params     : access Params_Stream_Type;
      Result     : access Params_Stream_Type) is
      Id     : Request_Id;
      Header : Request_Header (RPC_Request);
      Tmp    : Result_Type;
      Keeper : Abort_Keeper;

   begin
      pragma Debug
        (D (D_Debug, "Doing a RPC for partition" & Partition'Img));
      begin
         pragma Abort_Defer;
         Request_Id_Server.Get (Id, Types.Partition_ID (Partition));
         Header.Id := Id;
         Insert_Request (Params.X'Access, Header);
         Partition_ID'Write (Params, Partition);
         Any_Priority'Write (Params, Ada.Dynamic_Priorities.Get_Priority);
         Send (Types.Partition_ID (Partition), Remote_Call, Params.X'Access);
         Keeper.Id        := Id;
         Keeper.Partition := Types.Partition_ID (Partition);
         Keeper.Sent      := True;
      end;
      pragma Debug (D (D_Debug, "Waiting for the result"));
      Result_Watcher.Wait (Id);
      begin
         pragma Abort_Defer;
         pragma Debug (D (D_Debug, "The result is available"));
         Keeper.Sent := False;
         Result_Watcher.Get (Id, Tmp);
         pragma Assert (not Tmp.Cancelled);
         Request_Id_Server.Free (Id);
         pragma Debug (D (D_Debug, "Copying the result"));
         pragma Assert (Tmp.Result /= null);
         Streams.Deep_Copy (Tmp.Result.all, Result.X'Access);
         Streams.Free (Tmp.Result);
      end;
      pragma Debug (D (D_Debug, "Returning from Do_RPC"));
   exception
      when E : System.Garlic.Communication_Error =>
         Raise_Exception (Communication_Error'Identity,
                          Exception_Message (E));
   end Do_RPC;

   ----------------------------
   -- Establish_RPC_Receiver --
   ----------------------------

   procedure Establish_RPC_Receiver
     (Partition : in Partition_ID;
      Receiver  : in RPC_Receiver) is
   begin
      pragma Debug
        (D (D_Debug, "Setting RPC receiver for partition" & Partition'Img));
      RPC_Allowed := True;
      RPC_Barrier.Signal_All (Permanent => True);
      Register_Partition_Error_Notification
        (Partition_Error_Notification'Access);
   exception
      when E : System.Garlic.Communication_Error =>
         Raise_Exception (Communication_Error'Identity,
                          Exception_Message (E));
   end Establish_RPC_Receiver;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Keeper : in out Abort_Keeper) is
   begin
      if Keeper.Sent then
         Send_Abort_Message (Keeper.Partition, Keeper.Id);
         pragma Debug (D (D_Debug, "Will invalidate object" & Keeper.Id'Img));
         Result_Watcher.Invalidate (Keeper.Id);
      end if;
   end Finalize;

   ----------------------
   -- When_Established --
   ----------------------

   procedure When_Established is
   begin
      if not RPC_Allowed then
         RPC_Barrier.Wait;
      end if;
   end When_Established;

   --------------------
   -- Insert_Request --
   --------------------

   procedure Insert_Request
     (Params : access Streams.Params_Stream_Type;
      Header : in Request_Header)
   is
   begin
      Params.Special_First := True;
      Request_Header'Output (Params, Header);
   end Insert_Request;

   --------------------------
   -- Invalidate_RCI_Units --
   --------------------------

   procedure Invalidate_RCI_Units (Partition : in Types.Partition_ID) is
      Invalidation : Request_Type := (Invalidate, Partition, 0, null, null);

   begin
      pragma Debug
        (D (D_Debug, "Invalidate partition" & Partition'Img & "'s RCI units"));

      Apply (Partition_RCI_List (Partition), Invalidation, Process'Access);
   end Invalidate_RCI_Units;

   ----------------------------------
   -- Partition_Error_Notification --
   ----------------------------------

   procedure Partition_Error_Notification
     (Partition : in Types.Partition_ID) is
   begin
      Invalidate_RCI_Units (Partition);
      Request_Id_Server.Raise_Partition_Error (Partition);
   end Partition_Error_Notification;

   -------------------------
   -- Public_RPC_Receiver --
   -------------------------

   procedure Public_RPC_Receiver
     (Partition : in Types.Partition_ID;
      Operation : in Public_Opcode;
      Params    : access Streams.Params_Stream_Type) is
      Header : constant Request_Header := Request_Header'Input (Params);
   begin

      case Header.Kind is

         when RPC_Request | APC_Request =>

            declare
               Params_Copy  : Streams.Params_Stream_Access :=
                 new Streams.Params_Stream_Type (Params.Initial_Size);
               Id           : Request_Id := Request_Id'First;
               Asynchronous : constant Boolean := Header.Kind = APC_Request;
            begin
               pragma Debug
                 (D (D_Debug,
                     "RPC or APC request received from partition" &
                     Partition'Img));
               Streams.Deep_Copy (Params.all, Params_Copy);
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
               Result.Result :=
                new Streams.Params_Stream_Type (Params.Initial_Size);
               Streams.Deep_Copy (Params.all, Result.Result);
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
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset) is
   begin
      System.Garlic.Streams.Read (Stream.X, Item, Last);
   exception
      when E : System.Garlic.Communication_Error =>
         Raise_Exception (Communication_Error'Identity,
                          Exception_Message (E));
   end Read;

   ----------------------------
   -- Request_Id_Server_Type --
   ----------------------------

   protected body Request_Id_Server_Type is

      ---------------------------
      -- Raise_Partition_Error --
      ---------------------------

      procedure Raise_Partition_Error
        (Partition : in Types.Partition_ID) is
      begin
         for Id in Request_Id'First .. Latest loop
            if Destination (Id) = Partition then
               pragma Warnings (Off);  -- ??? To be checked
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
         if Destination (Id) /= System.Garlic.Types.Null_Partition_ID then
            Destination (Id) := System.Garlic.Types.Null_Partition_ID;
            Count := Count - 1;
         end if;
      end Free;

      ---------
      -- Get --
      ---------

      entry Get (Id : out Request_Id; Partition : in Types.Partition_ID)
      when Count < Request_Id'Last is
      begin
         while
           Destination (Latest) /= System.Garlic.Types.Null_Partition_ID
         loop
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
         Tmp : Result_Type;
      begin
         Get (Id, Tmp);
         Streams.Deallocate (Tmp.Result);
         pragma Warnings (Off);  -- ??? To be checked
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
     (Partition : in Types.Partition_ID;
      Id        : in Request_Id) is
      Params : aliased Streams.Params_Stream_Type (0);
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
      pragma Debug (D (D_Debug, "Shutdown has been called"));
      Free (Request_Id_Server);
      Free (Result_Watcher);
      Pool.Shutdown;
   end Shutdown;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Params_Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array) is
   begin
      Streams.Write (Stream.X, Item);
   exception
      when E : System.Garlic.Communication_Error =>
         Raise_Exception (Communication_Error'Identity,
                          Exception_Message (E));
   end Write;

begin
   Receive (Remote_Call, Public_RPC_Receiver'Access);
   Pool.Initialize;
   Stream_IO.Initialize;
   Register_RPC_Shutdown (Shutdown'Access);
end System.RPC;
