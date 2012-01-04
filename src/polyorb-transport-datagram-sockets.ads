------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--   P O L Y O R B . T R A N S P O R T . D A T A G R A M . S O C K E T S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

--  Datagram Socket Access Point and End Point to receive data from network

with PolyORB.Sockets;
with PolyORB.Utils.Sockets;

package PolyORB.Transport.Datagram.Sockets is

   pragma Elaborate_Body;

   use PolyORB.Sockets;

   ------------------
   -- Access Point --
   ------------------

   type Socket_Access_Point is
     new Datagram_Transport_Access_Point with private;
   --  Datagram Socket Access Point to receive data

   procedure Init_Socket
     (SAP          : in out Socket_Access_Point;
      Socket       : Socket_Type;
      Address      : in out Sock_Addr_Type;
      Bind_Address : Sock_Addr_Type := No_Sock_Addr;
      Update_Addr  : Boolean := True);
   --  Init datagram socket socket
   --  If Update_Addr is set, Address will be updated with the assigned socket
   --  address. If Bind_Address is not No_Sock_Addr, then that address is used
   --  to bind the access point, Address. This is used for multicast sockets
   --  on Windows, where we need to use IN_ADDR_ANY for Bind_Address, while
   --  still recording the proper group address in SAP.

   function Create_Event_Source
     (TAP : access Socket_Access_Point)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   function Address_Of
     (SAP : Socket_Access_Point) return Utils.Sockets.Socket_Name;
   --  Return a Socket_Name designating SAP

   ---------------
   -- End Point --
   ---------------

   type Socket_Endpoint
     is new Datagram_Transport_Endpoint with private;
   --  Datagram Socket Transport Endpoint for receiving data

   procedure Create
     (TE   : in out Socket_Endpoint;
      S    : Socket_Type;
      Addr : Sock_Addr_Type);
   --  Called on client side to assign remote server address

   function Create_Event_Source
     (TE : access Socket_Endpoint) return Asynch_Ev.Asynch_Ev_Source_Access;

   procedure Read
     (TE     : in out Socket_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  : out Errors.Error_Container);
   --  Read data from datagram socket

   procedure Write
     (TE     : in out Socket_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Error  : out Errors.Error_Container);
   --  Write data to datagram socket

   procedure Close (TE : access Socket_Endpoint);

   function Create_Endpoint
     (TAP : access Socket_Access_Point)
     return Datagram_Transport_Endpoint_Access;
   --  Called on server side to initialize socket

private

   type Socket_Access_Point is new Datagram_Transport_Access_Point
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
     end record;

   type Socket_Endpoint is new Datagram_Transport_Endpoint
     with record
        Handler        : aliased Datagram_TE_AES_Event_Handler;
        Socket         : Socket_Type := No_Socket;
        Remote_Address : Sock_Addr_Type;
     end record;

end PolyORB.Transport.Datagram.Sockets;
