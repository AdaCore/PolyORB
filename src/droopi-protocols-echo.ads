--  A dummy protocol, just for testing.

--  $Id$

with Droopi.Buffers;

package Droopi.Protocols.Echo is

   --  Echo_Protocol:
   --  A very simple protocol that echoes text lines received
   --  from the user.

   type Echo_Protocol is new Protocol with private;

   procedure Create_Session
     (Proto   : access Echo_Protocol;
      Sock    : Sockets.Socket_Type;
      Session : out Session_Access;
      Channel : out Channels.Channel_Access);

   --  Echo_Session

   type Echo_Session is new Session with private;

   procedure Invoke_Request (S : access Echo_Session; R : Request);
   procedure Abort_Request (S : access Echo_Session; R : Request);
   --  These are just for show and do nothing.

   procedure Handle_Connect (S : access Echo_Session);
   --  Send a greeting banner to user.

   procedure Handle_Data (S : access Echo_Session);
   --  Handle data received from user.

   procedure Handle_Connection_Closed (S : access Echo_Session);
   --  Handle disconnection from user.

private

   type Echo_Protocol is new Protocol with null record;
   type Echo_Session is new Session with record
      Buffer : Buffers.Buffer_Access;
   end record;

end Droopi.Protocols.Echo;
