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

   function Create_Event_Source
     (TAP : Socket_Access_Point)
      return Asynchronous_Event_Source_Access;

   procedure Accept_Connection
     (TAP : Socket_Access_Point;
      TE  : out Transport_Endpoint_Access);

   type Socket_Endpoint
     is new Transport_Endpoint with private;
   --  An opened transport endpoint as a connected
   --  stream-oriented socket.

   function Create_Event_Source
     (TE : Socket_Endpoint)
      return Asynchronous_Event_Source_Access;

   procedure Read
     (TE     : Socket_Endpoint;
      Buffer : Buffer_Access;
      Size   : in out Stream_Element_Count);

   procedure Write
     (TE     : Socket_Endpoint;
      Buffer : Buffer_Access);

   procedure Close (TE : Socket_Endpoint);

private

   type Socket_Access_Point is new Transport_Access_Point
     with record
        Socket : Socket_Type;
     end record;

   type Socket_Endpoint is new Transport_Endpoint
     with record
        Socket : Socket_Type;
        Addr   : Sock_Addr_Type;
     end record;

end Droopi.Transport.Sockets;
