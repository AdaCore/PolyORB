--  Socket implementation of transport service access points
--  and communication endpoints.

--  $Id$

with PolyORB.Sockets; use PolyORB.Sockets;

package PolyORB.Transport.Sockets is

   pragma Elaborate_Body;

   type Socket_Access_Point
      is new Transport_Access_Point with private;
   --  A listening transport service access point as
   --  a listening stream-oriented socket.

   procedure Create
     (SAP     : in out Socket_Access_Point;
      Socket  :        Socket_Type;
      Address :        Sock_Addr_Type);
   --  Initialise SAP: bind Socket to Address, listen on it,
   --  and set up the corresponding Socket_Access_Point.

   function Create_Event_Source
     (TAP : Socket_Access_Point)
      return Asynch_Ev_Source_Access;

   procedure Accept_Connection
     (TAP : Socket_Access_Point;
      TE  : out Transport_Endpoint_Access);

   function Address_Of (SAP : Socket_Access_Point)
     return Sock_Addr_Type;

   type Socket_Endpoint
     is new Transport_Endpoint with private;
   --  An opened transport endpoint as a connected
   --  stream-oriented socket.

   procedure Create
     (TE : in out Socket_Endpoint;
      S  : Socket_Type);

   function Create_Event_Source
     (TE : Socket_Endpoint)
      return Asynch_Ev_Source_Access;

   procedure Read
     (TE     : in out Socket_Endpoint;
      Buffer : Buffers.Buffer_Access;
      Size   : in out Stream_Element_Count);

   procedure Write
     (TE     : in out Socket_Endpoint;
      Buffer : Buffers.Buffer_Access);

   procedure Close (TE : in out Socket_Endpoint);

private

   type Socket_Access_Point is new Transport_Access_Point
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
     end record;

   type Socket_Endpoint is new Transport_Endpoint
     with record
        Socket : Socket_Type := No_Socket;
        Addr   : Sock_Addr_Type;
     end record;

end PolyORB.Transport.Sockets;
