------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                  POLYORB.TRANSPORT.DATAGRAM.SOCKETS_IN                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2005 Free Software Foundation, Inc.           --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

--  Datagram Socket Access Point and End Point to recieve data from network

with PolyORB.Sockets;

package PolyORB.Transport.Datagram.Sockets_In is

   pragma Elaborate_Body;

   use PolyORB.Sockets;

   ------------------
   -- Access Point --
   ------------------

   type Socket_In_Access_Point
   is new Datagram_Transport_Access_Point with private;
   --  Datagram Socket Access Point to receive data

   procedure Init_Socket_In
     (SAP         : in out Socket_In_Access_Point;
      Socket      : in     Socket_Type;
      Address     : in out Sock_Addr_Type;
      Update_Addr :        Boolean := True);
   --  Init datagram socket socket
   --  If Update_Addr is set, Address will be updated by the socket address
   --  For multicast sockets, it will remove multicast address

   function Create_Event_Source
     (TAP : access Socket_In_Access_Point)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   function Address_Of (SAP : Socket_In_Access_Point) return Sock_Addr_Type;

   ---------------
   -- End Point --
   ---------------

   type Socket_In_Endpoint
     is new Datagram_Transport_Endpoint with private;
   --  Datagram Socket End Point for reciving data

   procedure Create
     (TE   : in out Socket_In_Endpoint;
      S    :        Socket_Type;
      Addr :        Sock_Addr_Type);

   function Create_Event_Source
     (TE : access Socket_In_Endpoint)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   procedure Read
     (TE     : in out Socket_In_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  :    out Errors.Error_Container);
   --  Read data from datagram socket

   procedure Write
     (TE     : in out Socket_In_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Errors.Error_Container);
   pragma No_Return (Write);
   --  Write data to datagram socket. This procedure should not be
   --  used for read-only transport endpoints, Program_Error will be
   --  raised at run-time.

   procedure Close (TE : access Socket_In_Endpoint);

   function Create_Endpoint
     (TAP : access Socket_In_Access_Point)
     return Datagram_Transport_Endpoint_Access;

private

   type Socket_In_Access_Point is new Datagram_Transport_Access_Point
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
     end record;

   type Socket_In_Endpoint is new Datagram_Transport_Endpoint
     with record
        Handler : aliased Datagram_TE_AES_Event_Handler;
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
     end record;

end PolyORB.Transport.Datagram.Sockets_In;
