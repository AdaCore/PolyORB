------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.TRANSPORT.DATAGRAM.SOCKETS_IN                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2010, Free Software Foundation, Inc.          --
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

--  Datagram Socket Access Point and End Point to recieve data from network

with Ada.Exceptions;

with System.Storage_Elements;

with PolyORB.Asynch_Ev.Sockets;
with PolyORB.Log;

package body PolyORB.Transport.Datagram.Sockets_In is

   use Ada.Streams;

   use PolyORB.Asynch_Ev.Sockets;
   use PolyORB.Log;
   use PolyORB.Utils.Sockets;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.datagram.sockets_in");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   --------------------
   -- Init_Socket_In --
   --------------------

   procedure Init_Socket_In
     (SAP          : in out Socket_In_Access_Point;
      Socket       : Socket_Type;
      Address      : in out Sock_Addr_Type;
      Bind_Address : Sock_Addr_Type := No_Sock_Addr;
      Update_Addr  : Boolean := True)
   is
   begin
      if Bind_Address /= No_Sock_Addr then
         Bind_Socket (Socket, Bind_Address);
      else
         Bind_Socket (Socket, Address);
      end if;

      SAP.Socket := Socket;

      if Update_Addr then
         SAP.Addr := Get_Socket_Name (Socket);
         if SAP.Addr.Addr = Any_Inet_Addr then
            --  ??? Should keep Host_Name unresolved here, see comments in
            --  PolyORB.Transport.Connected.Sockets.Create.

            SAP.Addr.Addr := Local_Inet_Address;
         end if;
         Address := SAP.Addr;

      else
         SAP.Addr := Address;
      end if;
   end Init_Socket_In;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TAP : access Socket_In_Access_Point) return Asynch_Ev_Source_Access
   is
      Ev_Src : constant Asynch_Ev_Source_Access :=
                 Create_Event_Source (TAP.Socket);
   begin
      Set_Handler (Ev_Src.all, TAP.Handler'Access);
      return Ev_Src;
   end Create_Event_Source;

   ----------------
   -- Address_Of --
   ----------------

   function Address_Of
     (SAP : Socket_In_Access_Point) return Utils.Sockets.Socket_Name
   is
   begin
      return Image (SAP.Addr.Addr) + SAP.Addr.Port;
   end Address_Of;

   ------------
   -- Create --
   ------------

   procedure Create
     (TE   : in out Socket_In_Endpoint;
      S    :        Socket_Type;
      Addr :        Sock_Addr_Type) is
   begin
      TE.Addr := Addr;
      TE.Socket := S;
   end Create;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   function Create_Event_Source
     (TE : access Socket_In_Endpoint) return Asynch_Ev_Source_Access
   is
      Ev_Src : constant Asynch_Ev_Source_Access :=
                 Create_Event_Source (TE.Socket);
   begin
      Set_Handler (Ev_Src.all, TE.Handler'Access);
      return Ev_Src;
   end Create_Event_Source;

   ----------
   -- Read --
   ----------

   procedure Read
     (TE     : in out Socket_In_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count;
      Error  :    out Errors.Error_Container)
   is
      use PolyORB.Buffers;
      use PolyORB.Errors;

      Data_Received : Stream_Element_Count;
      Request : Request_Type (N_Bytes_To_Read);

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

      procedure Receive_Buffer is new PolyORB.Buffers.Receive_Buffer
        (Receive_Socket);

   begin
      --  Must read all data in one call from datagram socket.
      --  Amount read is often greater than asked amount.

      Control_Socket (TE.Socket, Request);
      Size := Stream_Element_Offset (Request.Size);
      pragma Debug (C, O ("To read :" & Size'Img));
      begin
         Receive_Buffer (Buffer, Size, Data_Received);
      exception
         when E : Sockets.Socket_Error =>
            O ("receive failed: " & Ada.Exceptions.Exception_Message (E),
               Notice);
            Throw (Error, Comm_Failure_E,
                   System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));

         when others =>
            Throw (Error, Unknown_E,
                   System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));
      end;

      pragma Assert (Data_Received /= 0);
      pragma Debug (C, O (Data_Received'Img & " byte(s) received"));
      pragma Assert (Data_Received <= Size);

      Size := Data_Received;
   end Read;

   -----------
   -- Write --
   -----------

   procedure Write
     (TE     : in out Socket_In_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Errors.Error_Container)
   is
   begin
      raise Program_Error;
      --  Should never happen
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (TE : access Socket_In_Endpoint) is
   begin
      pragma Debug (C, O ("Closing UDP socket"));
      if TE.Closed then
         return;
      end if;

      PolyORB.Transport.Datagram.Close
        (Datagram_Transport_Endpoint (TE.all)'Access);
      TE.Socket := No_Socket;
   end Close;

   ---------------------
   -- Create_Endpoint --
   ---------------------

   function Create_Endpoint
     (TAP : access Socket_In_Access_Point)
     return Datagram_Transport_Endpoint_Access
   is
      TE : constant Datagram_Transport_Endpoint_Access :=
             new Socket_In_Endpoint;

   begin
      pragma Debug (C, O ("Create Endpoint for UDP socket"));

      Socket_In_Endpoint (TE.all).Addr := TAP.Addr;
      Socket_In_Endpoint (TE.all).Socket := TAP.Socket;
      return TE;
   end Create_Endpoint;

end PolyORB.Transport.Datagram.Sockets_In;
