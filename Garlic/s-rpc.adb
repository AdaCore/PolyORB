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
with System.RPC.Stream_IO;

package body System.RPC is

   use Ada.Streams;
   use System.Garlic.Units.Complex;

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

   type RPC_Status is (Unknown, Running, Aborted, Completed);

   type RPC_Info is
      record
         PID    : Types.Partition_ID;
         Result : Streams.Stream_Element_Access;
         Status : RPC_Status;
      end record;

   subtype Valid_RPC_Id is RPC_Id range APC + 1 .. RPC_Id'Last;

   Callers : array (Valid_RPC_Id) of RPC_Info;
   Callers_Mutex : Mutex_Access := Create;

   type Abort_Keeper is new Ada.Finalization.Controlled with record
      Sent : Boolean := False;
      PID  : Types.Partition_ID;
      RPC  : RPC_Id;
   end record;

   procedure Allocate (RPC : out RPC_Id; PID : Types.Partition_ID);
   procedure Deallocate (RPC : in RPC_Id);
   procedure Raise_Partition_Error (PID : in Types.Partition_ID);

   procedure Wait_For
     (RPC    : in  RPC_Id;
      Result : out Streams.Stream_Element_Access);

   procedure Invalidate_RCI_Units (Partition : in Types.Partition_ID);
   pragma Inline (Invalidate_RCI_Units);
   --  For a given partition, send an invalidation message to the boot
   --  server and unregister its RCI units locally.

   procedure Finalize (Keeper : in out Abort_Keeper);
   --  Handle abortion from Do_RPC

   procedure Send_Abort_Message
     (PID : in Types.Partition_ID;
      RPC : in RPC_Id);
   --  Send an abort message for a request

   procedure Handle_Request
     (Partition : in Types.Partition_ID;
      Opcode    : in Public_Opcode;
      Params    : access Streams.Params_Stream_Type);
   --  Receive data

   procedure Shutdown;
   --  Shutdown System.RPC and its private child packages

   procedure Partition_Error_Notification
     (Partition : in Types.Partition_ID);
   --  Call this procedure to unblock tasks waiting for RPC results from
   --  a dead partition.

   --------------
   -- Allocate --
   --------------

   procedure Allocate (RPC : out RPC_Id; PID : in Types.Partition_ID)
   is
   begin
      loop
         Enter (Callers_Mutex);
         for I in Callers'Range loop
            if Callers (I).Status = Unknown then
               Callers (I).Status := Running;
               Callers (I).PID := PID;
               RPC := I;
               Leave (Callers_Mutex, Modified);
               return;
            end if;
         end loop;
         Leave (Callers_Mutex, Postponed);
      end loop;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (RPC : in RPC_Id)
   is
   begin
      Enter (Callers_Mutex);
      Callers (RPC).Status := Unknown;
      Leave (Callers_Mutex, Modified);
   end Deallocate;

   ------------
   -- Do_APC --
   ------------

   procedure Do_APC
     (Partition : in Partition_ID;
      Params    : access Params_Stream_Type)
   is
      Header : constant RPC_Header := (Kind => APC_Query);
   begin
      pragma Debug
        (D (D_Debug, "Doing a APC for partition" & Partition'Img));
      Insert_RPC_Header (Params.X'Access, Header);
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
      Result     : access Params_Stream_Type)
   is
      RPC    : RPC_Id;
      Header : RPC_Header (RPC_Query);
      Stream : Streams.Stream_Element_Access;
      Keeper : Abort_Keeper;

   begin
      pragma Debug
        (D (D_Debug, "Doing a RPC for partition" & Partition'Img));
      begin
         pragma Abort_Defer;
         Allocate (RPC, Types.Partition_ID (Partition));
         Header.RPC := RPC;
         Insert_RPC_Header (Params.X'Access, Header);
         Partition_ID'Write (Params, Partition);
         Any_Priority'Write (Params, Ada.Dynamic_Priorities.Get_Priority);
         Send (Types.Partition_ID (Partition), Remote_Call, Params.X'Access);
         Keeper.RPC  := RPC;
         Keeper.PID  := Types.Partition_ID (Partition);
         Keeper.Sent := True;
      end;
      pragma Debug (D (D_Debug, "Waiting for the result"));
      Wait_For (RPC, Stream);
      begin
         pragma Abort_Defer;
         pragma Debug (D (D_Debug, "The result is available"));
         Keeper.Sent := False;
         pragma Debug (D (D_Debug, "Copying the result"));
         Streams.Write (Result.X, Stream.all);
         Streams.Free (Stream);
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
      Receiver  : in RPC_Receiver)
   is
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

   procedure Finalize (Keeper : in out Abort_Keeper)
   is
      Status : Status_Type := Unmodified;
   begin
      if Keeper.Sent then
         pragma Debug (D (D_Debug, "Will invalidate object" & Keeper.RPC'Img));
         Enter (Callers_Mutex);
         Send_Abort_Message (Keeper.PID, Keeper.RPC);
         if Callers (Keeper.RPC).Status = Running then
            Callers (Keeper.RPC).Status := Aborted;
            Status := Modified;
         end if;
         Leave (Callers_Mutex, Status);
      end if;
   end Finalize;

   ----------------------
   -- When_Established --
   ----------------------

   procedure When_Established
   is
   begin
      if not RPC_Allowed then
         RPC_Barrier.Wait;
      end if;
   end When_Established;

   --------------------
   -- Insert_RPC_Header --
   --------------------

   procedure Insert_RPC_Header
     (Params : access Streams.Params_Stream_Type;
      Header : in RPC_Header)
   is
   begin
      Params.Special_First := True;
      RPC_Header'Output (Params, Header);
   end Insert_RPC_Header;

   --------------------------
   -- Invalidate_RCI_Units --
   --------------------------

   procedure Invalidate_RCI_Units (Partition : in Types.Partition_ID)
   is
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
     (Partition : in Types.Partition_ID)
   is
   begin
      if Shutdown_In_Progress then
         return;
      end if;

      Invalidate_RCI_Units (Partition);
      Raise_Partition_Error (Partition);
   end Partition_Error_Notification;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Types.Partition_ID;
      Opcode    : in Public_Opcode;
      Params    : access Streams.Params_Stream_Type)
   is
      Header : constant RPC_Header := RPC_Header'Input (Params);
   begin

      case Header.Kind is
         when RPC_Query | APC_Query =>
            declare
               Params_Copy  : Streams.Params_Stream_Access :=
                 new Streams.Params_Stream_Type (Params.Initial_Size);
               RPC          : RPC_Id := RPC_Id'First;
               Asynchronous : constant Boolean := Header.Kind = APC_Query;
            begin
               pragma Debug
                 (D (D_Debug,
                     "RPC or APC request received from partition" &
                     Partition'Img));
               Streams.Deep_Copy (Params.all, Params_Copy);
               if not Asynchronous then
                  RPC := Header.RPC;
                  pragma Debug
                    (D (D_Debug,
                        "(request id is" & RPC'Img & ")"));
               end if;
               Allocate_Task (Partition, RPC, Params_Copy, Asynchronous);
            end;

         when RPC_Reply =>
            pragma Debug
              (D (D_Debug,
                  "RPC reply received from partition" & Partition'Img &
                  " (rpc" & Header.RPC'Img & ")"));
            Enter (Callers_Mutex);
            if Callers (Header.RPC).Status = Aborted then
               Callers (Header.RPC).Status := Unknown;
            else
               Callers (Header.RPC).Status := Completed;
               Callers (Header.RPC).Result
                 := Streams.To_Stream_Element_Access (Params);
            end if;
            Leave (Callers_Mutex, Modified);

         when Abortion_Query =>
            pragma Debug
                  (D (D_Debug,
                      "RPC abortion query received from partition" &
                      Partition'Img & " (rpc" & Header.RPC'Img & ")"));
            Abort_Task (Partition, Header.RPC);

         when Abortion_Reply =>
            pragma Debug
              (D (D_Debug,
                  "RPC abortion reply received from partition" &
                  Partition'Img & " (rpc" & Header.RPC'Img & ")"));
            Enter (Callers_Mutex);
            Callers (Header.RPC).Status := Unknown;
            Leave (Callers_Mutex, Modified);

      end case;
   end Handle_Request;

   ---------------------------
   -- Raise_Partition_Error --
   ---------------------------

   procedure Raise_Partition_Error (PID : in Types.Partition_ID)
   is
      Status : Status_Type := Unmodified;
   begin
      Enter (Callers_Mutex);
      for I in Callers'Range loop
         if Callers (I).Status = Running then
            Callers (I).Status := Aborted;
            Status := Modified;
         end if;
      end loop;
      Leave (Callers_Mutex, Status);
   end Raise_Partition_Error;

   ----------
   -- Read --
   ----------

   procedure Read
     (Stream : in out Params_Stream_Type;
      Item   : out Ada.Streams.Stream_Element_Array;
      Last   : out Ada.Streams.Stream_Element_Offset)
   is
   begin
      System.Garlic.Streams.Read (Stream.X, Item, Last);
   exception
      when E : System.Garlic.Communication_Error =>
         Raise_Exception (Communication_Error'Identity,
                          Exception_Message (E));
   end Read;

   ------------------------
   -- Send_Abort_Message --
   ------------------------

   procedure Send_Abort_Message
     (PID : in Types.Partition_ID;
      RPC : in RPC_Id)
   is
      Params : aliased Streams.Params_Stream_Type (0);
      Header : constant RPC_Header := (Abortion_Query, RPC);
   begin
      pragma Debug (D (D_Debug, "Sending abortion message"));
      Insert_RPC_Header (Params'Access, Header);
      Send (PID, Remote_Call, Params'Access);
   end Send_Abort_Message;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown
   is
   begin
      pragma Debug (D (D_Debug, "Shutdown has been called"));
      Destroy (Callers_Mutex);
      Pool.Shutdown;
   end Shutdown;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For
     (RPC    : in  RPC_Id;
      Result : out Streams.Stream_Element_Access)
   is
   begin
      loop
         Enter (Callers_Mutex);
         case Callers (RPC).Status is
            when Completed =>
               Result := Callers (RPC).Result;
               Callers (RPC).Status := Unknown;
               Callers (RPC).Result := null;
               Leave (Callers_Mutex, Modified);
               return;

            when Aborted =>
               Callers (RPC).Status := Unknown;
               Callers (RPC).Result := null;
               Leave (Callers_Mutex, Modified);
               raise Communication_Error;

            when others =>
               Leave (Callers_Mutex, Postponed);

         end case;
      end loop;
   end Wait_For;

   -----------
   -- Write --
   -----------

   procedure Write
     (Stream : in out Params_Stream_Type;
      Item   : in Ada.Streams.Stream_Element_Array)
   is
   begin
      Streams.Write (Stream.X, Item);
   exception
      when E : System.Garlic.Communication_Error =>
         Raise_Exception (Communication_Error'Identity,
                          Exception_Message (E));
   end Write;

begin
   Register_Handler (Remote_Call, Handle_Request'Access);
   Pool.Initialize;
   Stream_IO.Initialize;
   Register_RPC_Shutdown (Shutdown'Access);
end System.RPC;
