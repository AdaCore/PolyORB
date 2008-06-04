------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T R A N S P O R T . C O N N E C T E D . S O C K E T S   --
--                                                                          --
--                                 S p e c                                  --
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

with PolyORB.Sockets;
with PolyORB.Tasking.Mutexes;

package PolyORB.Transport.Connected.Sockets is

   pragma Elaborate_Body;

   use PolyORB.Sockets;

   type Socket_Access_Point
      is new Connected_Transport_Access_Point with private;
   --  A listening transport service access point as
   --  a listening stream-oriented socket.

   procedure Create
     (SAP     : in out Socket_Access_Point;
      Socket  :        Socket_Type;
      Address : in out Sock_Addr_Type);
   --  Initialise SAP: bind Socket to Address, listen on it,
   --  and set up the corresponding Socket_Access_Point.
   --  On entry, Address.Port may be Any_Port, in which case the system
   --  will assign an available port number itself. On return,
   --  Address is always set to the actual address used.

   function Create_Event_Source
     (TAP : access Socket_Access_Point)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   procedure Accept_Connection
     (TAP : Socket_Access_Point;
      TE  : out Transport_Endpoint_Access);

   function Address_Of (SAP : Socket_Access_Point)
     return Sock_Addr_Type;

   type Socket_Endpoint is new Transport_Endpoint with private;
   --  An opened transport endpoint as a connected stream-oriented socket

   procedure Create
     (TE : in out Socket_Endpoint;
      S  : Socket_Type);

   function Create_Event_Source
     (TE : access Socket_Endpoint)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   function Is_Data_Available
     (TE : Socket_Endpoint;
      N  : Natural)
     return Boolean;

   procedure Read
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  :    out Errors.Error_Container);

   procedure Write
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Errors.Error_Container);

   procedure Close (TE : access Socket_Endpoint);

   procedure Destroy (TE : in out Socket_Endpoint);

private

   type Socket_Access_Point is new Connected_Transport_Access_Point
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
     end record;

   type Socket_Endpoint is new Connected_Transport_Endpoint
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
        Mutex  : Tasking.Mutexes.Mutex_Access;
     end record;

end PolyORB.Transport.Connected.Sockets;
