------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T R A N S P O R T . C O N N E C T E D . S O C K E T S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2017, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

pragma Ada_2012;

--  Socket implementation of transport service access points
--  and communication endpoints.

with Ada.Exceptions;
with System.Storage_Elements;

with PolyORB.Asynch_Ev;
with PolyORB.Asynch_Ev.Sockets;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.Parameters;
with PolyORB.Utils.Socket_Access_Points;
with PolyORB.Utils.Strings;

package body PolyORB.Transport.Connected.Sockets is

   use Ada.Streams;

   use PolyORB.Asynch_Ev;
   use PolyORB.Asynch_Ev.Sockets;
   use PolyORB.Log;
   use PolyORB.Parameters;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Utils.Socket_Access_Points;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.connected.sockets");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   procedure Initialize;
   --  Create Dummy_Selector

   Dummy_Selector : Selector_Type;
   --  Selector object used for Check_Validity, abortion is never used on this
   --  selector.
   --  WAG:6.3
   --  Such a dummy selector should be provided by GNAT.Sockets directly.

   -----------------------
   -- Accept_Connection --
   -----------------------

   overriding procedure Accept_Connection
     (TAP : Connected_Socket_AP;
      TE  : out Transport_Endpoint_Access)
   is
      New_Socket  : Socket_Type;
      New_Address : Sock_Addr_Type;
   begin
      TE := new Socket_Endpoint;
      Accept_Socket
        (Server  => TAP.Socket,
         Socket  => New_Socket,
         Address => New_Address);
      Set_Close_On_Exec (New_Socket);
      pragma Debug (C, O ("Accept_Connection: from " & Image (New_Address)));
      Create (Socket_Endpoint (TE.all), New_Socket);
   end Accept_Connection;

   ------------
   -- Create --
   ------------

   procedure Create
     (SAP     : in out Connected_Socket_AP;
      Socket  : Socket_Type;
      Address : Sock_Addr_Type)
   is
   begin
      pragma Debug (C, O ("Create: listening on " & Image (Address)));
      Bind_Socket (Socket, Address);
      Listen_Socket (Socket);

      SAP.Socket := Socket;
      SAP.Addr   := Get_Socket_Name (Socket);
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   overriding function Create_Event_Source
     (TAP : access Connected_Socket_AP) return Asynch_Ev_Source_Access
   is
      Ev_Src : constant Asynch_Ev_Source_Access :=
        Create_Event_Source (TAP.Socket);
   begin
      Set_Handler (Ev_Src.all, TAP.Handler'Access);
      return Ev_Src;
   end Create_Event_Source;

   ------------
   -- Create --
   ------------

   procedure Create
     (TE : in out Socket_Endpoint;
      S  : Socket_Type)
   is
   begin
      TE.Socket := S;

      if Get_Conf ("transport", "tcp.nodelay", True) then
         Set_Socket_Option
           (Socket => S,
            Level  => IP_Protocol_For_TCP_Level,
            Option => (Name => No_Delay, Enabled => True));
      end if;

      Create (TE.Mutex);
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   overriding function Create_Event_Source
     (TE : access Socket_Endpoint) return Asynch_Ev_Source_Access
   is
      Ev_Src : constant Asynch_Ev_Source_Access :=
        Create_Event_Source (TE.Socket);
   begin
      Set_Handler (Ev_Src.all, TE.Handler'Access);
      return Ev_Src;
   end Create_Event_Source;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (TAP : in out Connected_Socket_AP) is
   begin
      Free (TAP.Publish);
   end Destroy;

   -----------------------
   -- Is_Data_Available --
   -----------------------

   overriding function Is_Data_Available
     (TE : Socket_Endpoint;
      N  : Natural) return Boolean
   is
      Request : Request_Type (N_Bytes_To_Read);
   begin
      Control_Socket (TE.Socket, Request);
      pragma Debug (C, O ("Found" & Request.Size'Img & " bytes waiting"));
      return Request.Size >= N;
   end Is_Data_Available;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count;
      Error  :    out Errors.Error_Container)
   is
      use PolyORB.Buffers;
      use PolyORB.Errors;

      Data_Received : Stream_Element_Count;

      procedure Receive_Socket (V : access Iovec);
      --  Lowlevel socket receive

      --------------------
      -- Receive_Socket --
      --------------------

      procedure Receive_Socket (V : access Iovec) is
         Count : Ada.Streams.Stream_Element_Count;
         Vecs  : Vector_Type (1 .. 1);
         pragma Import (Ada, Vecs);
         for Vecs'Address use V.all'Address;
      begin
         PolyORB.Sockets.Receive_Vector (TE.Socket, Vecs, Count);
         V.Iov_Len := System.Storage_Elements.Storage_Offset (Count);
      end Receive_Socket;

      procedure Receive_Buffer is
        new PolyORB.Buffers.Receive_Buffer (Receive_Socket);

   --  Start of processing for Read

   begin
      begin
         Receive_Buffer (Buffer, Size, Data_Received);
      exception
         when E : PolyORB.Sockets.Socket_Error =>
            O ("receive failed: " & Ada.Exceptions.Exception_Message (E),
               Notice);
            Throw
              (Error, Comm_Failure_E,
               System_Exception_Members'
               (Minor => 0, Completed => Completed_Maybe));
            return;

         when others =>
            Throw
              (Error, Unknown_E,
               System_Exception_Members'
               (Minor => 0, Completed => Completed_Maybe));
            return;
      end;

      pragma Assert (Data_Received <= Size);
      Size := Data_Received;
   end Read;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Errors.Error_Container)
   is
      use PolyORB.Errors;
      use PolyORB.Buffers;

      procedure Socket_Send
        (V     : access Iovec;
         N     : Integer;
         Count : out System.Storage_Elements.Storage_Offset);
      --  Send gathered data

      -----------------
      -- Socket_Send --
      -----------------

      procedure Socket_Send
        (V     : access Iovec;
         N     : Integer;
         Count : out System.Storage_Elements.Storage_Offset)
      is
         subtype SV_T is PolyORB.Sockets.Vector_Type (1 .. N);
         SV : SV_T;
         pragma Import (Ada, SV);
         for SV'Address use V.all'Address;

         S_Count : Ada.Streams.Stream_Element_Count;
      begin
         PolyORB.Sockets.Send_Vector (TE.Socket, SV, S_Count);
         Count := System.Storage_Elements.Storage_Offset (S_Count);
      end Socket_Send;

      procedure Send_Buffer is new Buffers.Send_Buffer (Socket_Send);

   --  Start of processing for Write

   begin
      pragma Abort_Defer;

      pragma Debug (C, O ("Write: enter"));

      --  Send_Buffer is not atomic, needs to be protected.

      Enter (TE.Mutex);
      pragma Debug (C, O ("Write: TE mutex acquired"));

      begin
         Send_Buffer (Buffer);
      exception
         when E : PolyORB.Sockets.Socket_Error =>
            O ("send failed: " & Ada.Exceptions.Exception_Message (E),
               Notice);
            Throw
              (Error, Comm_Failure_E, System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));

         when others =>
            Throw
              (Error, Unknown_E, System_Exception_Members'
                (Minor => 0, Completed => Completed_Maybe));
      end;
      Leave (TE.Mutex);

      pragma Debug (C, O ("Write: leave"));
   end Write;

   --------------------
   -- Check_Validity --
   --------------------

   overriding procedure Check_Validity (TE : access Socket_Endpoint) is
      Buf  : Stream_Element_Array (1 .. 1);
      Last : Stream_Element_Offset;

      R_Set, W_Set : Socket_Set_Type;
      Status : Selector_Status;
   begin
      pragma Assert (TE.Socket /= No_Socket);
      Set (R_Set, TE.Socket);

      Check_Selector (Dummy_Selector, R_Set, W_Set, Status, 0.0);

      if Status = Completed and then Is_Set (R_Set, TE.Socket) then
         begin
            --  Note: it is important that TE is not currently being monitored
            --  by an ORB polling task, otherwise the fact that the above
            --  Check_Selector has completed would not guarantee that the
            --  Receive_Socket call below won't block -- the data might have
            --  been stolen by the polling task.

            Receive_Socket (TE.Socket, Buf, Last, Peek_At_Incoming_Data);

         exception
            when Socket_Error =>

               --  On Windows, Receive_Socket on a closed fd may raise an
               --  exception rather than returning 0 bytes.

               Last := Buf'First - 1;
         end;

         if Last < Buf'First then
            --  Connection closed

            Close (TE);
         end if;
      end if;
   end Check_Validity;

   -----------
   -- Close --
   -----------

   overriding procedure Close (TE : access Socket_Endpoint) is
   begin
      if TE.Closed then
         return;
      end if;

      --  Unregister this TE. Note that this will enter the ORB critical
      --  section, so we do it outside ot TE.Mutex, as a precaution to avoid
      --  a dead lock due to inconsistent lock ordering between TE.Mutex and
      --  the ORB critical section. In any case, holding TE.Mutex should not
      --  be necessary at this point.

      PolyORB.Transport.Connected.Close
        (Connected_Transport_Endpoint (TE.all)'Access);

      --  We can now safely close underlying socket. We do so under control
      --  of the TE mutex, so that Write never uses an invalid (or possibly
      --  reused) socket descriptor.

      declare
         SL : Scope_Lock (TE.Mutex);
         pragma Unreferenced (SL);
      begin
         if TE.Socket /= No_Socket then
            pragma Debug (C, O ("Closing socket"
                             & PolyORB.Sockets.Image (TE.Socket)));
            Close_Socket (TE.Socket);
            TE.Socket := No_Socket;
         end if;
      exception
         when E : others =>
            pragma Debug (C, O ("Close (Socket_Endpoint): got "
                             & Ada.Exceptions.Exception_Information (E)));
            null;
      end;
   end Close;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (TE : in out Socket_Endpoint) is
   begin
      Destroy (TE.Mutex);
      Connected.Destroy (Connected_Transport_Endpoint (TE));
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Create_Selector (Dummy_Selector);
   end Initialize;

   --------------------------------
   -- Set_Socket_AP_Publish_Name --
   --------------------------------

   overriding procedure Set_Socket_AP_Publish_Name
      (SAP  : in out Connected_Socket_AP;
       Name : Socket_Name)
   is
   begin
      SAP.Publish := new Socket_Name'(Name);
   end Set_Socket_AP_Publish_Name;

   -----------------------
   -- Socket_AP_Address --
   -----------------------

   overriding function Socket_AP_Address
     (SAP : Connected_Socket_AP) return Sock_Addr_Type
   is
   begin
      return SAP.Addr;
   end Socket_AP_Address;

   ----------------------------
   -- Socket_AP_Publish_Name --
   ----------------------------

   overriding function Socket_AP_Publish_Name
     (SAP : access Connected_Socket_AP) return Socket_Name
   is
   begin
      Set_Default_Publish_Name (SAP.Publish, SAP.Addr);
      return SAP.Publish.all;
   end Socket_AP_Publish_Name;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"transport.connected.sockets",
       Conflicts => Empty,
       Depends   => Empty,
       Provides  => +"transport",
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PolyORB.Transport.Connected.Sockets;
