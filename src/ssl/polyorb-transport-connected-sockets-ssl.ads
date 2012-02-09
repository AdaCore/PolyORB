------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.TRANSPORT.CONNECTED.SOCKETS.SSL                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2012, Free Software Foundation, Inc.          --
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

--  SSL transport service access points and transport endpoints.

with PolyORB.SSL;

package PolyORB.Transport.Connected.Sockets.SSL is

   pragma Elaborate_Body;

   type SSL_Access_Point is new Socket_Access_Point with private;

   procedure Create
     (SAP     : in out SSL_Access_Point;
      Socket  : PolyORB.Sockets.Socket_Type;
      Address : in out PolyORB.Sockets.Sock_Addr_Type;
      Context : PolyORB.SSL.SSL_Context_Type);

   overriding function Create_Event_Source
     (TAP : access SSL_Access_Point)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   overriding procedure Accept_Connection
     (TAP : SSL_Access_Point;
      TE  : out Transport_Endpoint_Access);

   function Get_SSL_Context
     (SAP : SSL_Access_Point)
      return PolyORB.SSL.SSL_Context_Type;

   type SSL_Endpoint is new Socket_Endpoint with private;

   procedure Create
     (TE : in out SSL_Endpoint;
      S  : PolyORB.SSL.SSL_Socket_Type);

   overriding function Create_Event_Source
     (TE : access SSL_Endpoint)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   overriding function Is_Data_Available
     (TE : SSL_Endpoint;
      N : Natural)
     return Boolean;

   overriding procedure Read
     (TE     : in out SSL_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  : out Errors.Error_Container);

   overriding procedure Write
     (TE     : in out SSL_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Error  : out Errors.Error_Container);

   overriding procedure Close (TE : access SSL_Endpoint);

private

   type SSL_Access_Point is new Socket_Access_Point with record
      Context : PolyORB.SSL.SSL_Context_Type;
   end record;

   type SSL_Endpoint is new Socket_Endpoint with record
      SSL_Socket : PolyORB.SSL.SSL_Socket_Type;
   end record;

end PolyORB.Transport.Connected.Sockets.SSL;
