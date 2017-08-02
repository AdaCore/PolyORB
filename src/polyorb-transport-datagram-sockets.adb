------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . T R A N S P O R T . D A T A G R A M . S O C K E T S    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2017, Free Software Foundation, Inc.          --
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

pragma Ada_2005;

--  Datagram Socket Access Point and End Point to receive data from network

with Ada.Exceptions;

with System.Storage_Elements;

with PolyORB.Asynch_Ev;
with PolyORB.Asynch_Ev.Sockets;
with PolyORB.Log;
with PolyORB.Utils.Socket_Access_Points;

package body PolyORB.Transport.Datagram.Sockets is

   use Ada.Streams;

   use PolyORB.Asynch_Ev;
   use PolyORB.Asynch_Ev.Sockets;
   use PolyORB.Log;
   use PolyORB.Utils.Socket_Access_Points;

   package L is new PolyORB.Log.Facility_Log
     ("polyorb.transport.datagram.sockets");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   -----------------
   -- Init_Socket --
   -----------------

   procedure Init_Socket
     (SAP          : in out Datagram_Socket_AP;
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
   end Init_Socket;

   -------------------------
   -- Create_Event_Source --
   -------------------------

   overriding function Create_Event_Source
     (TAP : access Datagram_Socket_AP) return Asynch_Ev_Source_Access
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
     (SAP : Datagram_Socket_AP) return Utils.Sockets.Socket_Name
   is
   begin
      return Image (SAP.Addr.Addr) + SAP.Addr.Port;
   end Address_Of;

   ------------
   -- Create --
   ------------

   procedure Create
     (TE   : in out Socket_Endpoint;
      S    : Socket_Type;
      Addr : Sock_Addr_Type)
   is
   begin
      TE.Socket := S;
      TE.Remote_Address := Addr;
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

   ----------
   -- Read --
   ----------

   overriding procedure Read
     (TE     : in out Socket_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count;
      Error  : out Errors.Error_Container)
   is
      use PolyORB.Buffers;
      use PolyORB.Errors;

      Request       : Request_Type (N_Bytes_To_Read);
      Data_Received : Ada.Streams.Stream_Element_Offset;

      procedure Lowlevel_Receive_Datagram (V : access Iovec);
      --  Receive datagram from TE into V

      -------------------------------
      -- Lowlevel_Receive_Datagram --
      -------------------------------

      procedure Lowlevel_Receive_Datagram (V : access Iovec) is
         Count : Stream_Element_Count;
         Item  : Stream_Element_Array (1 .. Stream_Element_Offset (V.Iov_Len));
         for Item'Address use V.Iov_Base;
         pragma Import (Ada, Item);
      begin
         Receive_Socket
           (TE.Socket, Item, Count, TE.Remote_Address, No_Request_Flag);
         V.Iov_Len := System.Storage_Elements.Storage_Offset (Count);
      end Lowlevel_Receive_Datagram;

      procedure Receive_Datagram is
        new Buffers.Receive_Buffer (Lowlevel_Receive_Datagram);

   --  Start of processing for Read

   begin
      begin
         Control_Socket (TE.Socket, Request);
         Size := Stream_Element_Offset (Request.Size);
         Receive_Datagram (Buffer, Size, Data_Received);
      exception
         when E : Socket_Error =>
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
      use PolyORB.Buffers;
      use PolyORB.Errors;

      Data : constant Stream_Element_Array :=
        To_Stream_Element_Array (Buffer.all);
      Last : Stream_Element_Offset;

   begin
      pragma Debug (C, O ("Write: enter"));
      pragma Debug (C, O ("Send to : " & Image (TE.Remote_Address)));
      pragma Debug (C, O ("Buffer Size : " & Data'Length'Img));
      begin
         PolyORB.Sockets.Send_Socket
           (TE.Socket, Data, Last, TE.Remote_Address);
      exception
         when E : Socket_Error =>
            O ("send failed: " & Ada.Exceptions.Exception_Information (E),
               Notice);
            Throw (Error, Comm_Failure_E,
                   System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));

         when others =>
            Throw (Error, Unknown_E,
                   System_Exception_Members'
                   (Minor => 0, Completed => Completed_Maybe));
      end;
      pragma Debug (C, O ("Write: leave"));
   end Write;

   -----------
   -- Close --
   -----------

   overriding procedure Close (TE : access Socket_Endpoint) is
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

   overriding function Create_Endpoint
     (TAP : access Datagram_Socket_AP)
     return Datagram_Transport_Endpoint_Access
   is
      TE : constant Datagram_Transport_Endpoint_Access :=
        new Socket_Endpoint;

   begin
      pragma Debug (C, O ("Create Endpoint for UDP socket"));
      Socket_Endpoint (TE.all).Socket := TAP.Socket;
      return TE;
   end Create_Endpoint;

   --------------------------------
   -- Set_Socket_AP_Publish_Name --
   --------------------------------

   overriding procedure Set_Socket_AP_Publish_Name
      (SAP  : in out Datagram_Socket_AP;
       Name : Socket_Name)
   is
   begin
      SAP.Publish := new Socket_Name'(Name);
   end Set_Socket_AP_Publish_Name;

   -----------------------
   -- Socket_AP_Address --
   -----------------------

   overriding function Socket_AP_Address
     (SAP : Datagram_Socket_AP) return Sock_Addr_Type
   is
   begin
      return SAP.Addr;
   end Socket_AP_Address;

   ----------------------------
   -- Socket_AP_Publish_Name --
   ----------------------------

   overriding function Socket_AP_Publish_Name
     (SAP : access Datagram_Socket_AP) return Socket_Name
   is
   begin
      Set_Default_Publish_Name (SAP.Publish, SAP.Addr);
      return SAP.Publish.all;
   end Socket_AP_Publish_Name;

end PolyORB.Transport.Datagram.Sockets;
