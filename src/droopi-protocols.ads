--  Support for object method invocation protocols.

--  $Id$

package Droopi.Protocols is

   pragma Preelaborate;

   --  A protocol is a factory of sessions. Each session corresponds
   --  to a connection to a remote protocol entity, through a transport
   --  channel.

   type Session is abstract tagged limited private;
   type Session_Access is access all Session'Class;

   type Protocol is abstract tagged limited private;

   function Create_Session
     (P : Protocol)
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

private

   type Protocol is abstract tagged limited null record;
   type Session is abstract tagged limited null record;

end Droopi.Protocols;
