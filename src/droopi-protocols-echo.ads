--  A dummy protocol, just for testing.

--  $Id$

with Droopi.Binding_Data;
with Droopi.Buffers;

package Droopi.Protocols.Echo is

   --  Echo_Protocol:
   --  A very simple protocol that echoes text lines received
   --  from the user.

   type Echo_Protocol is new Protocol with private;

   procedure Create
     (Proto   : access Echo_Protocol;
      Session : out Filter_Access);

   type Echo_Session is new Session with private;

   procedure Connect
     (S : access Echo_Session; P : Binding_Data.Profile_Access);
   procedure Invoke_Request (S : access Echo_Session; R : Request);
   procedure Abort_Request (S : access Echo_Session; R : Request);
   --  These are just for show and do nothing.

   procedure Handle_Connect (S : access Echo_Session);
   --  Send a greeting banner to user.

   procedure Handle_Data_Indication (S : access Echo_Session);
   --  Handle data received from user.

   procedure Handle_Disconnect (S : access Echo_Session);
   --  Handle disconnection from user.

private

   type Echo_Protocol is new Protocol with null record;

   type Echo_Session is new Session with record
      Buffer : Buffers.Buffer_Access;
   end record;

end Droopi.Protocols.Echo;
