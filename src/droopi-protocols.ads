--  Support for object method invocation protocols.

--  $Id$

with Droopi.Channels;
with Droopi.Requests; use Droopi.Requests;
with Droopi.Sockets;

package Droopi.Protocols is

   pragma Elaborate_Body;

   --  A protocol is a factory of sessions. Each session corresponds
   --  to a connection to a remote protocol entity.

   type Session is abstract tagged limited private;
   type Session_Access is access all Session'Class;

   type Protocol is abstract tagged limited private;
   type Protocol_Access is access all Protocol'Class;

   procedure Create_Session
     (Proto   : access Protocol;
      Sock    : Sockets.Socket_Type;
      Session : out Session_Access;
      Channel : out Channels.Channel_Access)
     is abstract;
   --  Create a session for protocol Proto using socket Sock.
   --  The newly-created Session and the corresponding Channel
   --  are returned.

   procedure Destroy_Session (S : in out Session_Access);
   --  Destroy the session associated with S, return any associated
   --  resources to the system, and assign null to S.

   -----------------------------------------------------
   -- Protocol primitives (interface to upper layers) --
   -----------------------------------------------------

   procedure Invoke_Request (S : access Session; R : Request)
      is abstract;
   procedure Abort_Request (S : access Session; R : Request)
      is abstract;

   ------------------------------------------------
   -- Callback point (interface to lower layers) --
   ------------------------------------------------

   procedure Handle_Connect (S : access Session) is abstract;
   --  Invoked when a new incoming connection has been accepted
   --  as session S.

   procedure Handle_Data (S : access Session) is abstract;
   --  Invoked when some data arrives for session S.

   procedure Handle_Connection_Closed (S : access Session) is abstract;
   --  Invoked when the underlying connection is closed.

private

   type Protocol is abstract tagged limited null record;

   type Session is abstract tagged limited record
      Channel : Channels.Channel_Access;
   end record;

   type Session_Channel is new Channels.Channel with record
      Session : Session_Access;
      --  Uplink to the associated session.
   end record;

   procedure Signal_Data (SC : access Session_Channel);
   procedure Signal_Connection_Closed (SC : access Session_Channel);

end Droopi.Protocols;
