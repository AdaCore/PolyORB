--  A protocol similar to the HTTP protocol
--  SRP : Simple Request Protocol

with Droopi.Buffers;

package Droopi.Protocols.SRP is

   pragma Elaborate_Body;

   type SRP_Protocol is new Protocol with private;
   --   type String_Ptr is access all String;

   procedure Create
     (Proto   : access SRP_Protocol;
      Session : out Filter_Access);

   type SRP_Session is new Session with private;

   procedure Connect (S : access SRP_Session);
   procedure Invoke_Request (S : access SRP_Session; R : Request);
   procedure Abort_Request (S : access SRP_Session; R : Request);
   --  Do nothing.

   procedure Send_Reply (S : access SRP_Session; R : Request);
   --  Send a reply to the user.

   procedure Handle_Connect_Indication (S : access SRP_Session);
   --  Send a greeting banner to user.

   procedure Handle_Connect_Confirmation (S : access SRP_Session);
   --  Setup client dialog.

   procedure Handle_Data_Indication (S : access SRP_Session);
   --  Handle data received from user.

   procedure Handle_Disconnect (S : access SRP_Session);
   --  Handle disconnection from user.

private

   type SRP_Protocol is new Protocol with null record;

   type SRP_Session is new Session with record
      Buffer : Buffers.Buffer_Access;
      Out_Buffer : Buffers.Buffer_Access;
   end record;

end Droopi.Protocols.SRP;
