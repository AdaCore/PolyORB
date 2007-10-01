------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.TRANSPORT.CONNECTED.SOCKETS.SSL                  --
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

   function Create_Event_Source
     (TAP : access SSL_Access_Point)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   procedure Accept_Connection
     (TAP : SSL_Access_Point;
      TE  : out Transport_Endpoint_Access);

   function Get_SSL_Context
     (SAP : SSL_Access_Point)
      return PolyORB.SSL.SSL_Context_Type;

   type SSL_Endpoint is new Socket_Endpoint with private;

   procedure Create
     (TE : in out SSL_Endpoint;
      S  : PolyORB.SSL.SSL_Socket_Type);

   function Create_Event_Source
     (TE : access SSL_Endpoint)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   function Is_Data_Available (TE : SSL_Endpoint; N : Natural) return Boolean;

   procedure Read
     (TE     : in out SSL_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  : out Errors.Error_Container);

   procedure Write
     (TE     : in out SSL_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Error  : out Errors.Error_Container);

   procedure Close (TE : access SSL_Endpoint);

private

   type SSL_Access_Point is new Socket_Access_Point with record
      Context : PolyORB.SSL.SSL_Context_Type;
   end record;

   type SSL_Endpoint is new Socket_Endpoint with record
      SSL_Socket : PolyORB.SSL.SSL_Socket_Type;
   end record;

end PolyORB.Transport.Connected.Sockets.SSL;
