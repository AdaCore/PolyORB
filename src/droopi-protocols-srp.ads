--  A protocol similar to the HTTP protocol
--  SRP : Simple Request Protocol

with Ada.Unchecked_Deallocation;

with Droopi.Buffers;
with Droopi.Objects; use Droopi.Objects;

package Droopi.Protocols.SRP is

   pragma Elaborate_Body;

   type SRP_Protocol is new Protocol with private;
   --   type String_Ptr is access all String;

   type Arg_Info;
   type Arg_Info_Ptr is access Arg_Info;
   type Arg_Info is record
      Name  : String_Ptr;
      Value : String_Ptr;
      Next  : Arg_Info_Ptr := null;
   end record;

   type Split_SRP is record
      Method : String_Ptr;
      Oid    : Object_Id_Access;
      Args   : Arg_Info_Ptr;
   end record;


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

   procedure Free_Arg_Info is new Ada.Unchecked_Deallocation
     (Arg_Info, Arg_Info_Ptr);

end Droopi.Protocols.SRP;
