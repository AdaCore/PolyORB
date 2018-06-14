------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.TRANSPORT.CONNECTED.SOCKETS.SSL                  --
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
with Ada.Streams;
with System.Storage_Elements;

with PolyORB.Asynch_Ev.Sockets.SSL;
with PolyORB.Log;

package body PolyORB.Transport.Connected.Sockets.SSL is

   use PolyORB.Asynch_Ev;
   use PolyORB.Asynch_Ev.Sockets.SSL;
   use PolyORB.Log;
   use PolyORB.SSL;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.connected.sockets.ssl");
   procedure O (Message : String; Level : Log.Log_Level := Log.Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -----------------------
   -- Accept_Connection --
   -----------------------

   overriding procedure Accept_Connection
     (TAP : SSL_Access_Point;
      TE  : out Transport_Endpoint_Access)
   is
      New_Socket  : SSL_Socket_Type;
      New_Address : Sock_Addr_Type;

   begin
      TE := new SSL_Endpoint;
      Accept_Socket (TAP.Socket, TAP.Context, New_Socket, New_Address);
      Create (SSL_Endpoint (TE.all), New_Socket);

   exception
      when SSL_Error =>
         Destroy (TE);
   end Accept_Connection;

   -----------
   -- Close --
   -----------

   overriding procedure Close (TE : access SSL_Endpoint) is
   begin
      if TE.Closed then
         return;
      end if;

      Enter (TE.Mutex);
      begin
         if TE.SSL_Socket /= No_SSL_Socket then
            pragma Debug (C, O ("Closing socket"
                             & PolyORB.Sockets.Image (TE.Socket)));
            Close_Socket (TE.SSL_Socket);
            TE.SSL_Socket := No_SSL_Socket;
            TE.Socket := No_Socket;
         end if;
         Leave (TE.Mutex);
         PolyORB.Transport.Connected.Close
           (Connected_Transport_Endpoint (TE.all)'Access);
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
     (SAP     : in out SSL_Access_Point;
      Socket  : PolyORB.Sockets.Socket_Type;
      Address : in out PolyORB.Sockets.Sock_Addr_Type;
      Context : PolyORB.SSL.SSL_Context_Type)
   is
   begin
      Create (Connected_Socket_AP (SAP), Socket, Address);
      SAP.Context := Context;
   end Create;

   procedure Create
     (TE : in out SSL_Endpoint;
      S  : SSL_Socket_Type)
   is
   begin
      Create (Socket_Endpoint (TE), Socket_Of (S));
      TE.SSL_Socket := S;
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   overriding function Create_Event_Source
     (TAP : access SSL_Access_Point) return Asynch_Ev_Source_Access
   is
      Ev_Src : constant Asynch_Ev_Source_Access :=
        Create_Event_Source (TAP.Socket);
   begin
      Set_Handler (Ev_Src.all, TAP.Handler'Access);
      return Ev_Src;
   end Create_Event_Source;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   overriding function Create_Event_Source
     (TE : access SSL_Endpoint) return Asynch_Ev_Source_Access
   is
      Ev_Src : constant Asynch_Ev_Source_Access :=
        Create_Event_Source (TE.SSL_Socket);
   begin
      Set_Handler (Ev_Src.all, TE.Handler'Access);
      return Ev_Src;
   end Create_Event_Source;

   ---------------------
   -- Get_SSL_Context --
   ---------------------

   function Get_SSL_Context
     (SAP : SSL_Access_Point)
      return PolyORB.SSL.SSL_Context_Type
   is
   begin
      return SAP.Context;
   end Get_SSL_Context;

   -----------------------
   -- Is_Data_Available --
   -----------------------

   overriding function Is_Data_Available
     (TE : SSL_Endpoint;
      N  : Natural) return Boolean
   is
   begin
      return Pending_Length (TE.SSL_Socket) >= N;
   end Is_Data_Available;

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (TE     : in out SSL_Endpoint;
      Buffer : Buffers.Buffer_Access;
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
         PolyORB.SSL.Receive_Vector (TE.SSL_Socket, Vecs, Count);
         V.Iov_Len := System.Storage_Elements.Storage_Offset (Count);
      end Receive_Socket;

      procedure Receive_Buffer is new PolyORB.Buffers.Receive_Buffer
        (Receive_Socket);

   begin
      begin
         Receive_Buffer (Buffer, Size, Data_Received);
      exception
         when PolyORB.Sockets.Socket_Error =>
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

   -----------
   -- Write --
   -----------

   overriding procedure Write
     (TE     : in out SSL_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Error  :    out Errors.Error_Container)
   is

      procedure Socket_Send
        (V     : access PolyORB.Buffers.Iovec;
         N     : Integer;
         Count :    out System.Storage_Elements.Storage_Offset);
      --  Send gathered data

      -----------------
      -- Socket_Send --
      -----------------

      procedure Socket_Send
        (V     : access PolyORB.Buffers.Iovec;
         N     : Integer;
         Count :    out System.Storage_Elements.Storage_Offset)
      is
         subtype SV_T is PolyORB.Sockets.Vector_Type (1 .. N);
         SV : SV_T;
         pragma Import (Ada, SV);
         for SV'Address use V.all'Address;

         S_Count : Ada.Streams.Stream_Element_Count;
      begin
         PolyORB.SSL.Send_Vector (TE.SSL_Socket, SV, S_Count);
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
         when PolyORB.Sockets.Socket_Error =>
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

end PolyORB.Transport.Connected.Sockets.SSL;
