------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.TRANSPORT.DATAGRAM.SOCKETS_OUT                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
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

--  Datagram Socket End Point to send data to network

with PolyORB.Sockets;
with PolyORB.Tasking.Mutexes;
with PolyORB.Utils.Sockets;

package PolyORB.Transport.Datagram.Sockets_Out is

   pragma Elaborate_Body;

   use PolyORB.Sockets;

   ---------------
   -- End Point --
   ---------------

   type Socket_Out_Endpoint
     is new Datagram_Transport_Endpoint with private;
   --  Datagram Socket Access Point to send data

   procedure Create
     (TE   : in out Socket_Out_Endpoint;
      S    : Socket_Type;
      Addr : Utils.Sockets.Socket_Name);

   function Create_Event_Source
     (TE : access Socket_Out_Endpoint)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   procedure Read
     (TE     : in out Socket_Out_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  : out Errors.Error_Container);
   pragma No_Return (Read);
   --  Read data from datagram socket. This procedure should not be
   --  used for write-only transport endpoints, Program_Error will be
   --  raised at run-time.

   procedure Write
     (TE     : in out Socket_Out_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Error  : out Errors.Error_Container);
   --  Write data to datagram socket

   procedure Close (TE : access Socket_Out_Endpoint);

   procedure Destroy (TE : in out Socket_Out_Endpoint);

private

   type Socket_Out_Endpoint is new Datagram_Transport_Endpoint
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
        Mutex  : Tasking.Mutexes.Mutex_Access;
     end record;

end PolyORB.Transport.Datagram.Sockets_Out;
