--  A dummy protocol, just for testing.

--  $Id$

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

   procedure Connect (S : access Echo_Session);
   procedure Invoke_Request (S : access Echo_Session; R : Request);
   procedure Abort_Request (S : access Echo_Session; R : Request);
   --  These are just for show and do nothing.

   procedure Send_Reply (S : access Echo_Session; R : Request);
   --  Send a reply to the user.

   procedure Handle_Connect_Indication (S : access Echo_Session);
   --  Send a greeting banner to user.

   procedure Handle_Connect_Confirmation (S : access Echo_Session);
   --  Setup client dialog.

   procedure Handle_Data_Indication (S : access Echo_Session);
   --  Handle data received from user.

   procedure Handle_Disconnect (S : access Echo_Session);
   --  Handle disconnection from user.

private

   type Echo_Protocol is new Protocol with null record;

   type Echo_Session is new Session with record
      Buffer : Buffers.Buffer_Access;
      Out_Buffer : Buffers.Buffer_Access;
   end record;

end Droopi.Protocols.Echo;
