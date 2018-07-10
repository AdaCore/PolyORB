------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.TRANSPORT.CONNECTED.SOCKETS.TLS                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2005-2017, Free Software Foundation, Inc.          --
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

with Ada.Exceptions;
with System.Storage_Elements;

with PolyORB.Asynch_Ev.Sockets.TLS;
with PolyORB.Log;
with PolyORB.QoS.Transport_Contexts;
with PolyORB.Security.Credentials.Compound;
with PolyORB.Security.Credentials.TLS;

package body PolyORB.Transport.Connected.Sockets.TLS is

   use PolyORB.Asynch_Ev;
   use PolyORB.Asynch_Ev.Sockets.TLS;
   use PolyORB.Log;
   use PolyORB.QoS;
   use PolyORB.QoS.Transport_Contexts;
   use PolyORB.Security.Credentials;
   use PolyORB.Security.Credentials.Compound;
   use PolyORB.Security.Credentials.TLS;
   use PolyORB.Security.Transport_Mechanisms;
   use PolyORB.TLS;
   use PolyORB.Transport;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.connected.sockets.tls");
   procedure O (Message : String; Level : Log.Log_Level := Log.Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   function Extract_TLS_Credentials
     (Credentials : Credentials_Ref)
      return TLS_Credentials_Access;

   -----------------------
   -- Accept_Connection --
   -----------------------

   overriding procedure Accept_Connection
     (TAP : TLS_Access_Point;
      TE  : out Transport_Endpoint_Access)
   is
      New_Socket  : Socket_Type;
      TLS_Socket  : TLS_Socket_Type
        := Create_Accepting_Socket (Extract_TLS_Credentials (TAP.Credentials));
      New_Address : Sock_Addr_Type;

   begin
      Accept_Socket (TAP.Socket, New_Socket, New_Address);
      Set_Socket (TLS_Socket, New_Socket);
      Accept_Socket (TLS_Socket);

      TE := new TLS_Endpoint;
      Create
        (TLS_Endpoint (TE.all), TLS_Socket, TAP.Transport, TAP.Credentials);

   exception
      when Socket_Error | TLS_Error =>
         Close_Socket (TLS_Socket);
   end Accept_Connection;

   -----------
   -- Close --
   -----------

   overriding procedure Close (TE : access TLS_Endpoint) is
   begin
      if TE.Closed then
         return;
      end if;

      Enter (TE.Mutex);
      begin
         PolyORB.Transport.Connected.Close
           (Connected_Transport_Endpoint (TE.all)'Access);

         if TE.TLS_Socket /= No_TLS_Socket then
            pragma Debug (C, O ("Closing socket"
                             & PolyORB.Sockets.Image (TE.Socket)));
            Close_Socket (TE.TLS_Socket);
            pragma Debug (C, O ("Closed socket"
                             & PolyORB.Sockets.Image (TE.Socket)));
            TE.TLS_Socket := No_TLS_Socket;
            TE.Socket := No_Socket;
         end if;

         Leave (TE.Mutex);

      exception
         when E : others =>
            pragma Debug (C, O ("Close (Socket_Endpoint): got "
                             & Ada.Exceptions.Exception_Information (E)));
            null;
      end;
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create
     (TE          : in out TLS_Endpoint;
      S           :        TLS_Socket_Type)
   is
   begin
      Create (Socket_Endpoint (TE), Socket_Of (S));
      TE.TLS_Socket := S;
   end Create;

   procedure Create
     (TE          : in out TLS_Endpoint;
      S           :        TLS_Socket_Type;
      Mechanism   :
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access;
      Credentials :        Credentials_Ref)
   is
   begin
      Create (Socket_Endpoint (TE), Socket_Of (S));
      TE.Transport := Mechanism;
      TE.Credentials := Credentials;
      TE.TLS_Socket := S;
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   overriding function Create_Event_Source
     (TAP : access TLS_Access_Point) return Asynch_Ev_Source_Access
   is
      Ev_Src : constant Asynch_Ev_Source_Access :=
        Create_Event_Source (TAP.Socket);

   begin
      Set_Handler (Ev_Src.all, TAP.Handler'Access);
      return Ev_Src;
   end Create_Event_Source;

   overriding function Create_Event_Source
     (TE : access TLS_Endpoint) return Asynch_Ev_Source_Access
   is
      use PolyORB.Annotations;

      Ev_Src : constant Asynch_Ev_Source_Access :=
        Create_Event_Source (TE.TLS_Socket);

   begin
      Set_Handler (Ev_Src.all, TE.Handler'Access);
      return Ev_Src;
   end Create_Event_Source;

   ----------------
   -- Create_QoS --
   ----------------

   function Create_QoS
     (End_Point : TLS_Endpoint)
      return PolyORB.QoS.QoS_Parameter_Access
   is
      Result : constant QoS_Transport_Context_Parameter_Access
        := new QoS_Transport_Context_Parameter;

   begin
      Result.Transport := End_Point.Transport;
      Result.Accepting_Credentials := End_Point.Credentials;
      Result.Invocation_Credentials :=
        Create_Received_Compound_Credentials
        (Accepting => End_Point.Credentials,
         Transport => Create_Peer_TLS_Credentials (End_Point.TLS_Socket));

      return QoS_Parameter_Access (Result);
   end Create_QoS;

   -----------------------------
   -- Extract_TLS_Credentials --
   -----------------------------

   function Extract_TLS_Credentials
     (Credentials : Credentials_Ref)
      return TLS_Credentials_Access
   is
      Creds : Credentials_Access
        := Credentials_Access (Entity_Of (Credentials));

   begin
      if Creds /= null then
         Creds :=
           Credentials_Access
           (Entity_Of
            (Get_Transport_Credentials
             (Compound_Credentials_Access (Creds))));

         if Creds /= null
           and then Creds.all in TLS_Credentials'Class
         then
            return TLS_Credentials_Access (Creds);
         end if;
      end if;

      return null;
   end Extract_TLS_Credentials;

   -----------------------
   -- Is_Data_Available --
   -----------------------

   overriding function Is_Data_Available
     (TE : TLS_Endpoint;
      N  : Natural) return Boolean
   is
   begin
      return Pending_Length (TE.TLS_Socket) >= N;
   end Is_Data_Available;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (TE     : in out TLS_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  :    out Errors.Error_Container)
   is
      use type Ada.Streams.Stream_Element_Count;

      Data_Received : Ada.Streams.Stream_Element_Count;

      procedure Receive_Socket (V : access PolyORB.Buffers.Iovec);
      --  Lowlevel socket receive

      procedure Receive_Socket (V : access PolyORB.Buffers.Iovec) is
         Count : Ada.Streams.Stream_Element_Count;
         Vecs  : PolyORB.Sockets.Vector_Type (1 .. 1);
         pragma Import (Ada, Vecs);
         for Vecs'Address use V.all'Address;

      begin
         PolyORB.TLS.Receive_Vector (TE.TLS_Socket, Vecs, Count);
         V.Iov_Len := System.Storage_Elements.Storage_Offset (Count);
      end Receive_Socket;

      procedure Receive_Buffer is new PolyORB.Buffers.Receive_Buffer
        (Receive_Socket);

   begin
      begin
         Receive_Buffer (Buffer, Size, Data_Received);

      exception
         when PolyORB.Sockets.Socket_Error | PolyORB.TLS.TLS_Error =>
            PolyORB.Errors.Throw
              (Error, PolyORB.Errors.Comm_Failure_E,
               PolyORB.Errors.System_Exception_Members'
               (Minor => 0, Completed => PolyORB.Errors.Completed_Maybe));
            return;

         when others =>
            PolyORB.Errors.Throw
              (Error, PolyORB.Errors.Unknown_E,
               PolyORB.Errors.System_Exception_Members'
               (Minor => 0, Completed => PolyORB.Errors.Completed_Maybe));
            return;
      end;

      pragma Assert (Data_Received <= Size);
      Size := Data_Received;
   end Read;

   -------------------------------
   -- Set_Accepting_Credentials --
   -------------------------------

   procedure Set_Accepting_Credentials
     (TAP         : in out TLS_Access_Point;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref)
   is
   begin
      TAP.Credentials := Credentials;
   end Set_Accepting_Credentials;

   -----------------------------
   -- Set_Transport_Mechanism --
   -----------------------------

   procedure Set_Transport_Mechanism
     (TAP       : in out TLS_Access_Point;
      Mechanism :
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access)
   is
   begin
      TAP.Transport := Mechanism;
   end Set_Transport_Mechanism;

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (TE     : in out TLS_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Errors.Error_Container)
   is

      procedure Socket_Send
        (V     : access PolyORB.Buffers.Iovec;
         N     :        Integer;
         Count :    out System.Storage_Elements.Storage_Offset);
      --  Send gathered data

      -----------------
      -- Socket_Send --
      -----------------

      procedure Socket_Send
        (V     : access PolyORB.Buffers.Iovec;
         N     :        Integer;
         Count :    out System.Storage_Elements.Storage_Offset)
      is
         subtype SV_T is PolyORB.Sockets.Vector_Type (1 .. N);
         SV : SV_T;
         pragma Import (Ada, SV);
         for SV'Address use V.all'Address;

         S_Count : Ada.Streams.Stream_Element_Count;

      begin
         PolyORB.TLS.Send_Vector (TE.TLS_Socket, SV, S_Count);
         Count := System.Storage_Elements.Storage_Offset (S_Count);
      end Socket_Send;

      procedure Send_Buffer is new Buffers.Send_Buffer (Socket_Send);

   begin
      pragma Debug (C, O ("Write: enter"));

      --  Send_Buffer is not atomic, needs to be protected.

      Enter (TE.Mutex);
      pragma Debug (C, O ("TE mutex acquired"));

      begin
         Send_Buffer (Buffer);

      exception
         when PolyORB.Sockets.Socket_Error | PolyORB.TLS.TLS_Error =>
            PolyORB.Errors.Throw
              (Error, PolyORB.Errors.Comm_Failure_E,
               PolyORB.Errors.System_Exception_Members'
                (Minor => 0, Completed => PolyORB.Errors.Completed_Maybe));

         when others =>
            PolyORB.Errors.Throw
              (Error, PolyORB.Errors.Unknown_E,
               PolyORB.Errors.System_Exception_Members'
                (Minor => 0, Completed => PolyORB.Errors.Completed_Maybe));
      end;
      Leave (TE.Mutex);
   end Write;

end PolyORB.Transport.Connected.Sockets.TLS;
