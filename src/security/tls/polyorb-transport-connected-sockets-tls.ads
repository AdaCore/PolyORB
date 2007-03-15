------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                 POLYORB.TRANSPORT.CONNECTED.SOCKETS.TLS                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2005-2006, Free Software Foundation, Inc.          --
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

--  TLS transport service access points and transport endpoints.

with PolyORB.QoS;
with PolyORB.Security.Credentials;
with PolyORB.Security.Transport_Mechanisms;
with PolyORB.TLS;

package PolyORB.Transport.Connected.Sockets.TLS is

   pragma Elaborate_Body;

   type TLS_Access_Point is new Socket_Access_Point with private;

   function Create_Event_Source
     (TAP : access TLS_Access_Point)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   procedure Set_Accepting_Credentials
     (TAP         : in out TLS_Access_Point;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref);

   procedure Set_Transport_Mechanism
     (TAP       : in out TLS_Access_Point;
      Mechanism :
       PolyORB.Security.Transport_Mechanisms.
         Target_Transport_Mechanism_Access);

   procedure Accept_Connection
     (TAP : TLS_Access_Point;
      TE  : out Transport_Endpoint_Access);

   type TLS_Endpoint is new Socket_Endpoint with private;

   procedure Create
     (TE : in out TLS_Endpoint;
      S  :        PolyORB.TLS.TLS_Socket_Type);

   procedure Create
     (TE          : in out TLS_Endpoint;
      S           :        PolyORB.TLS.TLS_Socket_Type;
      Mechanism   :
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access;
      Credentials :        PolyORB.Security.Credentials.Credentials_Ref);

   function Create_Event_Source
     (TE : access TLS_Endpoint)
      return Asynch_Ev.Asynch_Ev_Source_Access;

   function Is_Data_Available (TE : TLS_Endpoint; N : Natural) return Boolean;

   procedure Read
     (TE     : in out TLS_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Ada.Streams.Stream_Element_Count;
      Error  : out Errors.Error_Container);

   procedure Write
     (TE     : in out TLS_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Error  : out Errors.Error_Container);

   procedure Close (TE : access TLS_Endpoint);

   function Create_QoS
     (End_Point : TLS_Endpoint) return PolyORB.QoS.QoS_Parameter_Access;

private

   type TLS_Access_Point is new Socket_Access_Point with record
      Transport   :
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access;
      Credentials : PolyORB.Security.Credentials.Credentials_Ref;
   end record;

   type TLS_Endpoint is new Socket_Endpoint with record
      Transport   :
       PolyORB.Security.Transport_Mechanisms.Target_Transport_Mechanism_Access;
      Credentials : PolyORB.Security.Credentials.Credentials_Ref;
      TLS_Socket  : PolyORB.TLS.TLS_Socket_Type;
   end record;

end PolyORB.Transport.Connected.Sockets.TLS;
