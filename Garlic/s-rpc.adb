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
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
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

with Ada.Exceptions;                   use Ada.Exceptions;

with System.Garlic;                    use System.Garlic;
with System.Garlic.Debug;              use System.Garlic.Debug;
with System.Garlic.Exceptions;         use System.Garlic.Exceptions;
with System.Garlic.Heart;              use System.Garlic.Heart;
with System.Garlic.Priorities;         use System.Garlic.Priorities;
with System.Garlic.Priorities.Mapping; use System.Garlic.Priorities.Mapping;
with System.Garlic.Soft_Links;

with System.Garlic.Startup;
pragma Elaborate_All (System.Garlic.Startup);
pragma Warnings (Off, System.Garlic.Startup);

with System.Garlic.Streams;
with System.Garlic.Types;
with System.Garlic.Units;        use System.Garlic.Units;

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
   Establish_RPC_Watcher : System.Garlic.Soft_Links.Watcher_Access;
   --  The current RPC receiver and its associated barrier

   Allocate_Pool_Task   : Allocate_Task_Procedure;
   Abort_Pool_Task      : Abort_Task_Procedure;
   Initialize_Task_Pool : Parameterless_Procedure;
   Shutdown_Task_Pool   : Parameterless_Procedure;

   type Session_Status is (Unknown, Running, Aborted, Completed);

   type Session_Info is
      record
         PID    : Types.Partition_ID;
         Result : Streams.Stream_Element_Access;
         Status : Session_Status;
         Stamp  : System.Garlic.Types.Stamp_Type;
      end record;

   subtype Valid_Session_Type is
     Session_Type range No_Session + 1 .. Session_Type'Last;

   Callers : array (Valid_Session_Type) of Session_Info;
   Callers_Watcher : System.Garlic.Soft_Links.Watcher_Access;

   type Dummy_Abort_Handler_Type is
     new Garlic.Soft_Links.Abort_Handler_Type with null record;

   procedure Raise_Partition_Error (PID : in Types.Partition_ID);

   procedure Handle_Request
     (Partition : in Types.Partition_ID;
      Opcode    : in External_Opcode;
      Query     : access Streams.Params_Stream_Type;
      Reply     : access Streams.Params_Stream_Type;
      Error     : in out Error_Type);
   --  Receive data

   procedure Shutdown;
   --  Shutdown System.RPC and its private child packages

   procedure Wait_For
     (Session : in  Session_Type;
      Stream  : out System.Garlic.Streams.Stream_Element_Access);

   procedure Notify_Partition_Error
     (Partition : in Types.Partition_ID);
   --  Call this procedure to unblock tasks waiting for RPC results from
   --  a dead partition.

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Session   : out Session_Type;
      Partition : in Partition_ID)
   is
      Version : Types.Version_Id;
   begin
      loop
         System.Garlic.Soft_Links.Enter_Critical_Section;
         for I in Callers'Range loop
            if Callers (I).Status = Unknown then
               Callers (I).Status := Running;
               Callers (I).PID    := Types.Partition_ID (Partition);
               Session := I;
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

   procedure Deallocate (Session : in Session_Type)
   is
   begin
      System.Garlic.Soft_Links.Enter_Critical_Section;
      Callers (Session).Status := Unknown;
      Callers (Session).PID    := Types.Null_PID;
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
      use System.Garlic.Soft_Links;

      Header   : constant RPC_Header := (Kind => APC_Query);
      Error    : aliased Error_Type;
   begin
      Insert_RPC_Header (Params.X'Access, Header);
      Types.Partition_ID'Write (Params, Types.Partition_ID (Partition));
      Global_Priority'Write (Params, To_Global_Priority (Get_Priority));
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
      use System.Garlic.Soft_Links;

      Session : Session_Type;
      Header  : RPC_Header (RPC_Query);
      Stream  : Streams.Stream_Element_Access;
      Handler : System.Garlic.Soft_Links.Abort_Handler_Type'Class
        := System.Garlic.Soft_Links.Abort_Handler;
      Error   : aliased Error_Type;

   begin
      pragma Debug (System.Garlic.Soft_Links.Set_Stamp (Types.No_Stamp));
      pragma Debug (D (Stamp_Image ("rpc initiate")));
      --  Initialize stamp. As a convention, when set_stamp parameter
      --  is no stamp and when task stamp is no stamp, we initialize
      --  task stamp to clock.

      begin
         pragma Abort_Defer;
         Allocate (Session, Partition);
         Header.Session := Session;
         Insert_RPC_Header (Params.X'Access, Header);
         Types.Partition_ID'Write (Params, Types.Partition_ID (Partition));
         Global_Priority'Write (Params, To_Global_Priority (Get_Priority));
         Send (Types.Partition_ID (Partition),
               Remote_Call,
               Params.X'Access,
               Error);

         if Found (Error) then
            Raise_Exception (Communication_Error'Identity,
                             Content (Error'Access));
         end if;
         Handler.Session   := Natural (Session);
         Handler.Partition := Types.Partition_ID (Partition);
         Handler.Waiting   := True;
         System.Garlic.Soft_Links.Adjust (Handler);
      end;

      D ("Execute session" & Session'Img & " on partition" & Partition'Img);

      Wait_For (Session, Stream);

      begin
         pragma Abort_Defer;
         Streams.Write (Result.X, Stream.all);
         Streams.Free (Stream);
         Handler.Waiting := False;
         System.Garlic.Soft_Links.Adjust (Handler);
      end;

      pragma Debug (D (Stamp_Image ("rpc complete")));
      pragma Debug (Soft_Links.Set_Stamp (Types.No_Stamp));
      --  Reset task stamp to no stamp as the request has been
      --  processed. By convention, task stamp differs from no stamp.
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

      if Initialize_Task_Pool /= null then
         Initialize_Task_Pool.all;
      end if;
      RPC_Allowed := True;
      System.Garlic.Soft_Links.Update (Establish_RPC_Watcher);
      Register_RPC_Error_Notifier (Notify_Partition_Error'Access);
   end Establish_RPC_Receiver;

   --------------
   -- Finalize --
   --------------

   procedure Finalize
     (Partition : in Garlic.Types.Partition_ID;
      Waiting   : in Boolean;
      Session   : in Session_Type) is
   begin
      if Waiting then
         System.Garlic.Soft_Links.Enter_Critical_Section;
         if Callers (Session).Status = Running then
            declare
               Params : aliased Streams.Params_Stream_Type (0);
               Header : constant RPC_Header := (Abortion_Query, Session);
               Error  : Error_Type;
            begin
               pragma Debug
                 (D ("Forward local abortion of session" & Session'Img &
                     " to partition" & Partition'Img));
               Insert_RPC_Header (Params'Access, Header);
               Callers (Session).Status := Aborted;
               System.Garlic.Soft_Links.Update (Callers_Watcher);
               System.Garlic.Soft_Links.Leave_Critical_Section;

               Send (Partition, Remote_Call, Params'Access, Error);
               Catch (Error);
            end;
         else
            if Callers (Session).Status = Aborted then
               Callers (Session).Status := Unknown;
            end if;
            System.Garlic.Soft_Links.Update (Callers_Watcher);
            System.Garlic.Soft_Links.Leave_Critical_Section;
         end if;
      end if;
   end Finalize;

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
               Session      : Session_Type := Session_Type'First;
               Asynchronous : constant Boolean := Header.Kind = APC_Query;
            begin
               Streams.Move (Query.all, Params_Copy.all);
               if not Asynchronous then
                  Session := Header.Session;
                  D ("Execute session" & Session'Img &
                     " from partition" & Partition'Img);
               else
                  D ("Execute APC from partition" & Partition'Img);
               end if;
               Allocate_Pool_Task
                 (Partition,
                  Session,
                  Soft_Links.Get_Stamp,
                  Params_Copy,
                  Asynchronous);
            end;

         when RPC_Reply =>
            System.Garlic.Soft_Links.Enter_Critical_Section;
            if Callers (Header.Session).Status = Aborted then
               Callers (Header.Session).Status := Unknown;
            else
               Callers (Header.Session).Status := Completed;
               Callers (Header.Session).Result :=
                 Streams.To_Stream_Element_Access (Query);
               Callers (Header.Session).Stamp  :=
                 System.Garlic.Soft_Links.Get_Stamp;
            end if;
            System.Garlic.Soft_Links.Update (Callers_Watcher);
            System.Garlic.Soft_Links.Leave_Critical_Section;

         when Abortion_Query =>
            Abort_Pool_Task (Partition, Header.Session);

         when Abortion_Reply =>
            System.Garlic.Soft_Links.Enter_Critical_Section;
            Callers (Header.Session).Status := Unknown;
            Callers (Header.Session).PID    := Types.Null_PID;
            System.Garlic.Soft_Links.Update (Callers_Watcher);
            System.Garlic.Soft_Links.Leave_Critical_Section;

      end case;
   end Handle_Request;

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
   -- Register_Task_Pool --
   ------------------------

   procedure Register_Task_Pool
     (Allocate_Task : in Allocate_Task_Procedure;
      Abort_Task    : in Abort_Task_Procedure;
      Initialize    : in Parameterless_Procedure;
      Shutdown      : in Parameterless_Procedure) is
   begin
      Allocate_Pool_Task   := Allocate_Task;
      Abort_Pool_Task      := Abort_Task;
      Initialize_Task_Pool := Initialize;
      Shutdown_Task_Pool   := Shutdown;
   end Register_Task_Pool;

   --------------
   -- Shutdown --
   --------------

   procedure Shutdown is
   begin
      pragma Debug (D ("Enter RPC shutdown"));
      if Shutdown_Task_Pool /= null then
         Shutdown_Task_Pool.all;
      end if;
      Units.Shutdown;
      pragma Debug (D ("Leave RPC shutdown"));
   end Shutdown;

   --------------
   -- Wait_For --
   --------------

   procedure Wait_For
     (Session : in  Session_Type;
      Stream  : out Streams.Stream_Element_Access)
   is
      Version : Types.Version_Id;
   begin
      loop
         D ("check for session" & Session'Img);
         System.Garlic.Soft_Links.Enter_Critical_Section;
         case Callers (Session).Status is
            when Completed =>
               Stream := Callers (Session).Result;
               Callers (Session).Status := Unknown;
               Callers (Session).PID    := Types.Null_PID;
               Callers (Session).Result := null;
               System.Garlic.Soft_Links.Update (Callers_Watcher);
               System.Garlic.Soft_Links.Leave_Critical_Section;
               return;

            when Aborted =>
               Callers (Session).Status := Unknown;
               Callers (Session).PID    := Types.Null_PID;
               Callers (Session).Result := null;
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

   ----------------------
   -- When_Established --
   ----------------------

   procedure When_Established
   is
   begin
      if not RPC_Allowed then
         System.Garlic.Soft_Links.Differ
           (Establish_RPC_Watcher, System.Garlic.Types.No_Version);
      end if;
   end When_Established;

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
   System.Garlic.Soft_Links.Create
     (Establish_RPC_Watcher, System.Garlic.Types.No_Version);
   System.Garlic.Soft_Links.Create (Callers_Watcher);
   Register_Handler (Remote_Call, Handle_Request'Access);
   System.Garlic.Soft_Links.Register_RPC_Shutdown (Shutdown'Access);
   System.Garlic.Soft_Links.Register_Abort_Handler
     (new Dummy_Abort_Handler_Type);
end System.RPC;
