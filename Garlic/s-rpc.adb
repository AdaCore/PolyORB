------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                           S Y S T E M . R P C                            --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2000 Free Software Foundation, Inc.           --
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
with System.Garlic.Soft_Links;

with System.Garlic.Startup;
pragma Elaborate_All (System.Garlic.Startup);
pragma Warnings (Off, System.Garlic.Startup);

with System.Garlic.Streams;
with System.Garlic.Types;
with System.Garlic.Units;        use System.Garlic.Units;
with System.Garlic.Utils;        use System.Garlic.Utils;
with System.RPC.Pool;            use System.RPC.Pool;
with System.RPC.Stream_IO;

package body System.RPC is

   use Ada.Streams;
   use System.Garlic.Units;

   use type System.Garlic.Streams.Params_Stream_Access;
   use type System.Garlic.Streams.Params_Stream_Type;
   use type System.Garlic.Types.Partition_ID;

   --  This package needs extra comments ???

   Private_Debug_Key : constant Debug_Key :=
     Debug_Initialize ("S_RPC", "(s-rpc   ): ");
   procedure D
     (Message : in String;
      Key     : in Debug_Key := Private_Debug_Key)
     renames Print_Debug_Info;

   RPC_Allowed : Boolean := False;
   RPC_Barrier : System.Garlic.Soft_Links.Barrier_Access;
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
   Callers_Watcher : System.Garlic.Soft_Links.Watcher_Access;

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
      Stream : out Streams.Stream_Element_Access);

   procedure Finalize (Keeper : in out Abort_Keeper);
   --  Handle abortion from Do_RPC

   procedure Send_Abort_Message
     (PID   : in Types.Partition_ID;
      RPC   : in RPC_Id);
   --  Send an abort message for a request

   procedure Handle_Request
     (Partition : in Types.Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type;
      Error     : in out Error_Type);
   --  Receive data

   procedure Shutdown;
   --  Shutdown System.RPC and its private child packages

   procedure Notify_Partition_Error
     (Partition : in Types.Partition_ID);
   --  Call this procedure to unblock tasks waiting for RPC results from
   --  a dead partition.

   --------------
   -- Allocate --
   --------------

   procedure Allocate (RPC : out RPC_Id; PID : in Types.Partition_ID)
   is
      Version : Version_Id;
   begin
      loop
         System.Garlic.Soft_Links.Enter_Critical_Section;
         for I in Callers'Range loop
            if Callers (I).Status = Unknown then
               Callers (I).Status := Running;
               Callers (I).PID    := PID;
               RPC := I;
               System.Garlic.Soft_Links.Leave_Critical_Section;
               return;
            end if;
         end loop;
         System.Garlic.Soft_Links.Lookup (Callers_Watcher, Version);
         System.Garlic.Soft_Links.Leave_Critical_Section;
         System.Garlic.Soft_Links.Differ (Callers_Watcher, Version);
      end loop;
   end Allocate;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate (RPC : in RPC_Id)
   is
   begin
      System.Garlic.Soft_Links.Enter_Critical_Section;
      Callers (RPC).Status := Unknown;
      Callers (RPC).PID    := Types.Null_PID;
      System.Garlic.Soft_Links.Update (Callers_Watcher);
      System.Garlic.Soft_Links.Leave_Critical_Section;
   end Deallocate;

   ------------
   -- Do_APC --
   ------------

   procedure Do_APC
     (Partition : in Partition_ID;
      Params    : access Params_Stream_Type)
   is
      Header : constant RPC_Header := (Kind => APC_Query);
      Error  : aliased Error_Type;
   begin
      Insert_RPC_Header (Params.X'Access, Header);
      Types.Partition_ID'Write (Params, Types.Partition_ID (Partition));
      Any_Priority'Write (Params, Ada.Dynamic_Priorities.Get_Priority);
      Send (Types.Partition_ID (Partition),
            Remote_Call,
            Params.X'Access,
            Error);
      if Found (Error) then
         Raise_Exception (Communication_Error'Identity,
                          Content (Error'Access));
      end if;

      D ("Execute APC on partition" & Partition'Img);
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
      Error  : aliased Error_Type;
   begin
      begin
         pragma Abort_Defer;
         Allocate (RPC, Types.Partition_ID (Partition));
         Header.RPC := RPC;
         Insert_RPC_Header (Params.X'Access, Header);
         Types.Partition_ID'Write (Params, Types.Partition_ID (Partition));
         Any_Priority'Write (Params, Ada.Dynamic_Priorities.Get_Priority);
         Send (Types.Partition_ID (Partition),
               Remote_Call,
               Params.X'Access,
               Error);
         if Found (Error) then
            Raise_Exception (Communication_Error'Identity,
                             Content (Error'Access));
         end if;
         Keeper.RPC  := RPC;
         Keeper.PID  := Types.Partition_ID (Partition);
         Keeper.Sent := True;
      end;

      D ("Execute RPC number" & RPC'Img & " on partition" & Partition'Img);

      Wait_For (RPC, Stream);

      begin
         pragma Abort_Defer;
         Keeper.Sent := False;
         Streams.Write (Result.X, Stream.all);
         Streams.Free (Stream);
      end;

      D ("Complete RPC number " & RPC'Img & " on partition" & Partition'Img);
   end Do_RPC;

   ----------------------------
   -- Establish_RPC_Receiver --
   ----------------------------

   procedure Establish_RPC_Receiver
     (Partition : in Partition_ID;
      Receiver  : in RPC_Receiver)
   is
   begin
      D ("Accept RPCs on this partition" & Partition'Img);

      RPC_Allowed := True;
      System.Garlic.Soft_Links.Signal_All (RPC_Barrier);
      Register_RPC_Error_Notifier (Notify_Partition_Error'Access);
   end Establish_RPC_Receiver;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Keeper : in out Abort_Keeper)
   is
   begin
      if Keeper.Sent then
         System.Garlic.Soft_Links.Enter_Critical_Section;
         if Callers (Keeper.RPC).Status = Running then
            D ("Forward local abortion of RPC number" & Keeper.RPC'Img &
               " to partition" & Keeper.PID'Img);
            Send_Abort_Message (Keeper.PID, Keeper.RPC);
            Callers (Keeper.RPC).Status := Aborted;
         elsif Callers (Keeper.RPC).Status = Aborted then
            Callers (Keeper.RPC).Status := Unknown;
         end if;
         System.Garlic.Soft_Links.Update (Callers_Watcher);
         System.Garlic.Soft_Links.Leave_Critical_Section;
      end if;
   end Finalize;

   ----------------------
   -- When_Established --
   ----------------------

   procedure When_Established
   is
   begin
      if not RPC_Allowed then
         System.Garlic.Soft_Links.Wait (RPC_Barrier);
      end if;
   end When_Established;

   -----------------------
   -- Insert_RPC_Header --
   -----------------------

   procedure Insert_RPC_Header
     (Params : access Streams.Params_Stream_Type;
      Header : in RPC_Header)
   is
   begin
      Streams.Insert (Params.all);
      RPC_Header'Output (Params, Header);
   end Insert_RPC_Header;

   ----------------------------
   -- Notify_Partition_Error --
   ----------------------------

   procedure Notify_Partition_Error
     (Partition : in Types.Partition_ID)
   is
   begin
      if not Shutdown_Activated then
         Invalidate_Partition_Units (Partition);
         Raise_Partition_Error (Partition);
      end if;
   end Notify_Partition_Error;

   --------------------
   -- Handle_Request --
   --------------------

   procedure Handle_Request
     (Partition : in Types.Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type;
      Error     : in out Error_Type)
   is
      Header : constant RPC_Header := RPC_Header'Input (Query);
   begin

      case Header.Kind is
         when RPC_Query | APC_Query =>
            declare
               Params_Copy  : Streams.Params_Stream_Access :=
                 new Streams.Params_Stream_Type (Query.Initial_Size);
               RPC          : RPC_Id := RPC_Id'First;
               Asynchronous : constant Boolean := Header.Kind = APC_Query;
            begin
               Streams.Move (Query.all, Params_Copy.all);
               if not Asynchronous then
                  RPC := Header.RPC;
                  D ("Execute RPC number" & RPC'Img &
                     " from partition" & Partition'Img);
               else
                  D ("Execute APC from partition" & Partition'Img);
               end if;
               Allocate_Task (Partition, RPC, Params_Copy, Asynchronous);
            end;

         when RPC_Reply =>
            System.Garlic.Soft_Links.Enter_Critical_Section;
            if Callers (Header.RPC).Status = Aborted then
               Callers (Header.RPC).Status := Unknown;
            else
               Callers (Header.RPC).Status := Completed;
               Callers (Header.RPC).Result :=
                 Streams.To_Stream_Element_Access (Query);
            end if;
            System.Garlic.Soft_Links.Update (Callers_Watcher);
            System.Garlic.Soft_Links.Leave_Critical_Section;

         when Abortion_Query =>
            Abort_Task (Partition, Header.RPC);

         when Abortion_Reply =>
            System.Garlic.Soft_Links.Enter_Critical_Section;
            Callers (Header.RPC).Status := Unknown;
            Callers (Header.RPC).PID    := Types.Null_PID;
            System.Garlic.Soft_Links.Update (Callers_Watcher);
            System.Garlic.Soft_Links.Leave_Critical_Section;

      end case;
   end Handle_Request;

   ---------------------------
   -- Raise_Partition_Error --
   ---------------------------

   procedure Raise_Partition_Error (PID : in Types.Partition_ID)
   is
      Modified : Boolean := False;
   begin
      System.Garlic.Soft_Links.Enter_Critical_Section;
      for I in Callers'Range loop
         if Callers (I).PID = PID
           and then Callers (I).Status = Running
         then
            Callers (I).Status := Aborted;
            Modified := True;
         end if;
      end loop;
      if Modified then
         System.Garlic.Soft_Links.Update (Callers_Watcher);
      end if;
      System.Garlic.Soft_Links.Leave_Critical_Section;
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
     (PID   : in Types.Partition_ID;
      RPC   : in RPC_Id)
   is
      Params : aliased Streams.Params_Stream_Type (0);
      Header : constant RPC_Header := (Abortion_Query, RPC);
      Error  : Error_Type;
   begin
      Insert_RPC_Header (Params'Access, Header);
      Send (PID, Remote_Call, Params'Access, Error);
      Catch (Error);
   end Send_Abort_Message;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      pragma Debug (D ("Enter RPC shutdown"));
      Pool.Shutdown;
      Units.Shutdown;
      pragma Debug (D ("Leave RPC shutdown"));
   end Shutdown;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For
     (RPC    : in  RPC_Id;
      Stream : out Streams.Stream_Element_Access)
   is
      Version : Version_Id;
   begin
      loop
         System.Garlic.Soft_Links.Enter_Critical_Section;
         case Callers (RPC).Status is
            when Completed =>
               Stream := Callers (RPC).Result;
               Callers (RPC).Status := Unknown;
               Callers (RPC).PID    := Types.Null_PID;
               Callers (RPC).Result := null;
               System.Garlic.Soft_Links.Update (Callers_Watcher);
               System.Garlic.Soft_Links.Leave_Critical_Section;
               return;

            when Aborted =>
               Callers (RPC).Status := Unknown;
               Callers (RPC).PID    := Types.Null_PID;
               Callers (RPC).Result := null;
               System.Garlic.Soft_Links.Update (Callers_Watcher);
               System.Garlic.Soft_Links.Leave_Critical_Section;
               Raise_Exception (Communication_Error'Identity,
                                "remote procedure call aborted");
            when others =>
               System.Garlic.Soft_Links.Lookup (Callers_Watcher, Version);
               System.Garlic.Soft_Links.Leave_Critical_Section;

         end case;
         System.Garlic.Soft_Links.Differ (Callers_Watcher, Version);
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
   System.Garlic.Soft_Links.Create (RPC_Barrier);
   System.Garlic.Soft_Links.Create (Callers_Watcher);
   Register_Handler (Remote_Call, Handle_Request'Access);
   Pool.Initialize;
   Stream_IO.Initialize;
   System.Garlic.Soft_Links.Register_RPC_Shutdown (Shutdown'Access);
end System.RPC;
