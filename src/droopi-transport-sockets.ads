--  Socket implementation of transport service access points
--  and communication endpoints.

--  $Id$

with Droopi.Sockets; use Droopi.Sockets;

package Droopi.Transport.Sockets is

   pragma Elaborate_Body;

   type Socket_Access_Point
      is new Transport_Access_Point with private;
   --  A listening transport service access point as
   --  a listening stream-oriented socket.

   function Create_Transport_Access_Point
     (Socket : Socket_Type)
     return Transport_Access_Point_Access;

   function Create_Event_Source
     (TAP : Socket_Access_Point)
      return Asynch_Ev_Source_Access;

   procedure Accept_Connection
     (TAP : Socket_Access_Point;
      TE  : out Transport_Endpoint_Access);

   type Socket_Endpoint
     is new Transport_Endpoint with private;
   --  An opened transport endpoint as a connected
   --  stream-oriented socket.

   function Create_Event_Source
     (TE : Socket_Endpoint)
      return Asynch_Ev_Source_Access;

   procedure Read
     (TE     : in out Socket_Endpoint;
      Buffer : Buffer_Access;
      Size   : in out Stream_Element_Count);

   procedure Write
     (TE     : in out Socket_Endpoint;
      Buffer : Buffer_Access);

   procedure Close (TE : in out Socket_Endpoint);

private

   type Socket_Access_Point is new Transport_Access_Point
     with record
        Socket : Socket_Type := No_Socket;
     end record;

   type Socket_Endpoint is new Transport_Endpoint
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
     end record;

end Droopi.Transport.Sockets;
