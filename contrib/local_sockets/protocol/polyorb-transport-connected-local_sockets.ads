------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                POLYORB.TRANSPORT.CONNECTED.LOCAL_SOCKETS                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2005 Free Software Foundation, Inc.             --
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

with PolyORB.Local_Sockets;
with PolyORB.Tasking.Mutexes;

package PolyORB.Transport.Connected.Local_Sockets is

   pragma Elaborate_Body;

   use PolyORB.Local_Sockets;

   type Local_Socket_Access_Point is new
     Connected_Transport_Access_Point with private;

   --  Local_Socket_Access; Addr : Local_Socket_Addr; end record; Type
   --  Local_Socket_Access_Point is new
   --  Connected_Transport_Access_Point with private; A listening
   --  transport service access point as a listening stream-oriented
   --  socket.

   procedure Create
     (SAP     : in out Local_Socket_Access_Point;
      Socket  : Local_Socket_Type;
      Address : in out Local_Socket_Addr);
   --  Initialise SAP: bind Socket to Address, listen on it,
   --  and set up the corresponding Socket_Access_Point.

   --  XXX
   --  On entry, Address.Port may be Any_Port, in which case the system
   --  will assign an available port number itself. On return,
   --  Address is always set to the actual address used.

   function Create_Event_Source
     (TAP  : access Local_Socket_Access_Point)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   procedure Accept_Connection
     (TAP : Local_Socket_Access_Point;
      TE  : out Transport_Endpoint_Access);

   function Address_Of
     (SAP  : Local_Socket_Access_Point)
      return Local_Socket_Addr;

   type Local_Socket_Endpoint is new Connected_Transport_Endpoint
     with private;
   --  An opened transport endpoint as a connected stream-oriented
   --  socket.

   procedure Create
     (TE : in out Local_Socket_Endpoint;
      S  : Local_Socket_Type);

   function Create_Event_Source
     (TE   : access Local_Socket_Endpoint)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   function Is_Data_Available
     (TE   : Local_Socket_Endpoint;
      N    : Natural)
      return Boolean;

   function Image (TAP : Local_Socket_Access_Point) return String;

   procedure Read
     (TE     : in out Local_Socket_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  : out Errors.Error_Container);

   procedure Write
     (TE     : in out Local_Socket_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Error  : out Errors.Error_Container);

   procedure Close (TE : access Local_Socket_Endpoint);

   procedure Destroy (TE : in out Local_Socket_Endpoint);

private

   type Local_Socket_Access_Point is new
     Connected_Transport_Access_Point
     with record
        Socket : Local_Socket_Access;
        Addr   : Local_Socket_Addr;
     end record;

   type Local_Socket_Endpoint is new Connected_Transport_Endpoint
     with record
        Socket : Local_Socket_Access;
        Addr   : Local_Socket_Addr;
        Mutex  : Tasking.Mutexes.Mutex_Access;
     end record;

end PolyORB.Transport.Connected.Local_Sockets;
