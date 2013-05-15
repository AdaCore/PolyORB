------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--  P O L Y O R B . T R A N S P O R T . C O N N E C T E D . S O C K E T S   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2001-2013, Free Software Foundation, Inc.          --
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

--  Socket implementation of transport service access points
--  and communication endpoints.

with PolyORB.Sockets;
with PolyORB.Tasking.Mutexes;
with PolyORB.Transport.Sockets;
with PolyORB.Utils.Sockets;

package PolyORB.Transport.Connected.Sockets is

   pragma Elaborate_Body;

   use PolyORB.Sockets;
   use PolyORB.Transport.Sockets;
   use PolyORB.Utils.Sockets;

   type Connected_Socket_AP is
     new Connected_Transport_Access_Point
     and Socket_Access_Point
       with private;
   --  Transport access point backed by a listening stream socket

   procedure Create
     (SAP     : in out Connected_Socket_AP;
      Socket  : Socket_Type;
      Address : Sock_Addr_Type);
   --  Initialise SAP: bind Socket to Address, listen on it, and set up the
   --  corresponding Connected_Socket_AP.
   --  On entry, Address.Port may be Any_Port, in which case the system will
   --  assign an available port number itself. On return, Address is always
   --  set to the actual address used.

   overriding function Create_Event_Source
     (TAP : access Connected_Socket_AP)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   overriding procedure Accept_Connection
     (TAP : Connected_Socket_AP;
      TE  : out Transport_Endpoint_Access);

   overriding procedure Destroy (TAP : in out Connected_Socket_AP);

   type Socket_Endpoint is new Transport_Endpoint with private;
   --  An opened transport endpoint as a connected stream-oriented socket

   procedure Create
     (TE : in out Socket_Endpoint;
      S  : Socket_Type);

   overriding function Create_Event_Source
     (TE : access Socket_Endpoint)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   function Is_Data_Available
     (TE : Socket_Endpoint;
      N  : Natural)
     return Boolean;

   overriding procedure Read
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  :    out Errors.Error_Container);

   overriding procedure Write
     (TE     : in out Socket_Endpoint;
      Buffer :        Buffers.Buffer_Access;
      Error  :    out Errors.Error_Container);

   overriding procedure Close (TE : access Socket_Endpoint);
   overriding procedure Destroy (TE : in out Socket_Endpoint);

   overriding procedure Check_Validity (TE : access Socket_Endpoint);

private

   type Connected_Socket_AP is
     new Connected_Transport_Access_Point
     and Socket_Access_Point
     with record
        Socket  : Socket_Type := No_Socket;
        Addr    : Sock_Addr_Type;
        Publish : Socket_Name_Ptr;
     end record;

   overriding procedure Set_Socket_AP_Publish_Name
      (SAP  : in out Connected_Socket_AP;
       Name : Socket_Name);
   overriding function Socket_AP_Publish_Name
      (SAP : access Connected_Socket_AP) return Socket_Name;

   overriding function Socket_AP_Address
     (SAP : Connected_Socket_AP) return Sock_Addr_Type;

   type Socket_Endpoint is new Connected_Transport_Endpoint
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
        Mutex  : Tasking.Mutexes.Mutex_Access;
     end record;

end PolyORB.Transport.Connected.Sockets;
