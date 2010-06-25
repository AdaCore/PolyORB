------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.TRANSPORT.DATAGRAM.SOCKETS_IN                   --
--                                                                          --
--                                 S p e c                                  --
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

--  Datagram Socket Access Point and End Point to receive data from network

with PolyORB.Sockets;
with PolyORB.Utils.Sockets;

package PolyORB.Transport.Datagram.Sockets_In is

   pragma Elaborate_Body;

   use PolyORB.Sockets;

   ------------------
   -- Access Point --
   ------------------

   type Socket_In_Access_Point is
     new Datagram_Transport_Access_Point with private;
   --  Datagram Socket Access Point to receive data

   procedure Init_Socket_In
     (SAP          : in out Socket_In_Access_Point;
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
     (TAP : access Socket_In_Access_Point)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   function Address_Of
     (SAP : Socket_In_Access_Point) return Utils.Sockets.Socket_Name;
   --  Return a Socket_Name designating SAP

   ---------------
   -- End Point --
   ---------------

   type Socket_In_Endpoint
     is new Datagram_Transport_Endpoint with private;
   --  Datagram Socket Transport Endpoint for receiving data

   procedure Create
     (TE   : in out Socket_In_Endpoint;
      S    : Socket_Type;
      Addr : Sock_Addr_Type);
   --  Called on client side to assign remote server address

   function Create_Event_Source
     (TE : access Socket_In_Endpoint) return Asynch_Ev.Asynch_Ev_Source_Access;

   procedure Read
     (TE     : in out Socket_In_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  : out Errors.Error_Container);
   --  Read data from datagram socket

   procedure Write
     (TE     : in out Socket_In_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Error  : out Errors.Error_Container);
   --  Write data to datagram socket

   procedure Close (TE : access Socket_In_Endpoint);

   function Create_Endpoint
     (TAP : access Socket_In_Access_Point)
     return Datagram_Transport_Endpoint_Access;
   --  Called on server side to initialize socket

private

   type Socket_In_Access_Point is new Datagram_Transport_Access_Point
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
     end record;

   type Socket_In_Endpoint is new Datagram_Transport_Endpoint
     with record
        Handler        : aliased Datagram_TE_AES_Event_Handler;
        Socket         : Socket_Type := No_Socket;
        Remote_Address : Sock_Addr_Type;
     end record;

end PolyORB.Transport.Datagram.Sockets_In;
