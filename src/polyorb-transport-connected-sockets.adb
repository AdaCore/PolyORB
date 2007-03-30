------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T R A N S P O R T . C O N N E C T E D . S O C K E T S   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Socket implementation of transport service access points
--  and communication endpoints.

with Ada.Exceptions;
with System.Storage_Elements;

with PolyORB.Asynch_Ev.Sockets;
with PolyORB.Log;
with PolyORB.Parameters;

package body PolyORB.Transport.Connected.Sockets is

   use Ada.Streams;

   use PolyORB.Asynch_Ev.Sockets;
   use PolyORB.Log;
   use PolyORB.Parameters;
   use PolyORB.Tasking.Mutexes;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.connected.sockets");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   -----------------------
   -- Accept_Connection --
   -----------------------

   procedure Accept_Connection
     (TAP :     Socket_Access_Point;
      TE  : out Transport_Endpoint_Access)
   is
      New_Socket : Socket_Type;
      New_Address : Sock_Addr_Type;
   begin
      TE := new Socket_Endpoint;
      Accept_Socket
        (Server  => TAP.Socket,
         Socket  => New_Socket,
         Address => New_Address);
      Create (Socket_Endpoint (TE.all), New_Socket);
   end Accept_Connection;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of (SAP : Socket_Access_Point)
     return Sock_Addr_Type is
   begin
      return SAP.Addr;
   end Address_Of;

   ------------
   -- Create --
   ------------

   procedure Create
     (SAP     : in out Socket_Access_Point;
      Socket  :        Socket_Type;
      Address : in out Sock_Addr_Type) is
   begin
      Bind_Socket (Socket, Address);
      Listen_Socket (Socket);

      SAP.Socket := Socket;

      if Address.Addr = Any_Inet_Addr then

         --  Address is unspecified, choose one IP for the SAP looking
         --  up hostname.

         SAP.Addr.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
         Address := SAP.Addr;

      else

         --  Use specified IP address for SAP

         SAP.Addr := Address;
      end if;

      SAP.Addr.Port := Get_Socket_Name (Socket).Port;
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TAP : access Socket_Access_Point)
     return Asynch_Ev_Source_Access
   is
      use PolyORB.Annotations;

      Ev_Src : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TAP.Socket);
   begin
      Set_Note (Notepad_Of (Ev_Src).all,
                AES_Note'(Annotations.Note with Handler =>
                            TAP.Handler'Access));
      return Ev_Src;
   end Create_Event_Source;

   ------------
   -- Create --
   ------------

   procedure Create
     (TE : in out Socket_Endpoint;
      S  :        Socket_Type) is
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

   function Create_Event_Source
     (TE : access Socket_Endpoint)
     return Asynch_Ev_Source_Access
   is
      use PolyORB.Annotations;

      Ev_Src : constant Asynch_Ev_Source_Access
        := Create_Event_Source (TE.Socket);
   begin
      Set_Note (Notepad_Of (Ev_Src).all,
                AES_Note'(Annotations.Note with Handler =>
                            TE.Handler'Access));
      return Ev_Src;
   end Create_Event_Source;

   -----------------------
   -- Is_Data_Available --
   -----------------------

   function Is_Data_Available
     (TE : Socket_Endpoint;
      N  : Natural)
     return Boolean
   is
      Request : Request_Type (N_Bytes_To_Read);

   begin
      Control_Socket (TE.Socket, Request);

      pragma Debug (O ("Found" & Request.Size'Img & " bytes waiting"));

      return Request.Size >= N;
   end Is_Data_Available;

   ----------
   -- Read --
   ----------

   procedure Read
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

      procedure Receive_Socket (V : access Iovec) is
         Count : Ada.Streams.Stream_Element_Count;
         Vecs  : Vector_Type (1 .. 1);
         pragma Import (Ada, Vecs);
         for Vecs'Address use V.all'Address;
      begin
         PolyORB.Sockets.Receive_Vector (TE.Socket, Vecs, Count);
         V.Iov_Len := System.Storage_Elements.Storage_Offset (Count);
      end Receive_Socket;

      procedure Receive_Buffer is new PolyORB.Buffers.Receive_Buffer
        (Receive_Socket);
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

   procedure Write
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

   begin
      pragma Debug (O ("Write: enter"));

      --  Send_Buffer is not atomic, needs to be protected.

      Enter (TE.Mutex);
      pragma Debug (O ("TE mutex acquired"));

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
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (TE : access Socket_Endpoint) is
   begin
      if TE.Closed then
         return;
      end if;

      Enter (TE.Mutex);
      begin
         PolyORB.Transport.Connected.Close
           (Connected_Transport_Endpoint (TE.all)'Access);
         if TE.Socket /= No_Socket then
            pragma Debug (O ("Closing socket"
                             & PolyORB.Sockets.Image (TE.Socket)));
            Close_Socket (TE.Socket);
            TE.Socket := No_Socket;
         end if;
         Leave (TE.Mutex);
      exception
         when E : others =>
            pragma Debug (O ("Close (Socket_Endpoint): got "
                             & Ada.Exceptions.Exception_Information (E)));
            null;
      end;
   end Close;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (TE : in out Socket_Endpoint) is
   begin
      Destroy (TE.Mutex);
      Connected.Destroy (Connected_Transport_Endpoint (TE));
   end Destroy;

end PolyORB.Transport.Connected.Sockets;
