--  Support for object method invocation protocols.

--  $Id$

with Droopi.Requests; use Droopi.Requests;

package Droopi.Protocols is

   pragma Preelaborate;

   --  A protocol is a factory of sessions. Each session corresponds
   --  to a connection to a remote protocol entity, through a transport
   --  channel.

   type Session is abstract tagged limited private;
   type Session_Access is access all Session'Class;

   type Protocol is abstract tagged limited private;
   type Protocol_Access is access all Protocol'Class;

   function Create_Session
     (P : access Protocol)
     return Session_Access is abstract;

   -----------------------------------------------------
   -- Protocol primitives (interface to upper layers) --
   -----------------------------------------------------

   procedure Invoke_Request (S : Session; R : Request) is abstract;
   procedure Abort_Request (S : Session; R : Request) is abstract;

   ------------------------------------------------
   -- Callback point (interface to lower layers) --
   ------------------------------------------------

   procedure Handle_Data (S : Session) is abstract;
   --  Invoked when some data arrives for session S.

   -------------------------
   -- Resource management --
   -------------------------

   procedure Destroy_Session (S : in out Session_Access);
   --  Destroy the session associated with S, return any associated
   --  resources to the system, and assign null to S.

private

   type Protocol is abstract tagged limited null record;
   type Session is abstract tagged limited null record;

end Droopi.Protocols;
