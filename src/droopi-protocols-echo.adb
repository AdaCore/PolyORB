--  A dummy protocol, just for testing.

--  $Id$

with Droopi.Buffers;
with Droopi.Log;

with Droopi.Representations.Test; use Droopi.Representations.Test;

package body Droopi.Protocols.Echo is

   use Droopi.Log;

   package L is new Droopi.Log.Facility_Log ("droopi.protocols.echo");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   Rep : constant Rep_Test_Access := new Rep_Test;

   procedure Create_Session
     (Proto   : access Echo_Protocol;
      Sock    : Sockets.Socket_Type;
      Session : out Session_Access;
      Channel : out Channels.Channel_Access)
   is
   begin

      --  This should be factored in Droopi.Protocols.

      Session := new Echo_Session;
      Channel := new Session_Channel;
      Channels.Create (Channel, Sock);
      Session_Channel (Channel.all).Session := Session;
      Session.Channel := Channel;

      --  That is Echo-specific. Or is it?

      Echo_Session (Session.all).Buffer := new Buffers.Buffer_Type;

   end Create_Session;

   procedure Invoke_Request (S : access Echo_Session; R : Request) is
   begin
      null;
   end Invoke_Request;

   procedure Abort_Request (S : access Echo_Session; R : Request) is
   begin
      null;
   end Abort_Request;

   procedure Handle_Connect (S : access Echo_Session) is
   begin
      --  Send_String ("Hello, please type data." & ASCII.LF);
      pragma Debug (O ("Received new connection to echo service..."));
      Channels.Expect_Data (S.Channel, S.Buffer, 1024, False);
   end Handle_Connect;

   procedure Handle_Data (S : access Echo_Session) is
   begin
      pragma Debug (O ("Received data on echo service..."));
      pragma Debug (Buffers.Show (S.Buffer.all));

      declare
         Str : constant String
           := Unmarshall_String (Rep, S.Buffer);
      begin
         pragma Debug (O ("Remote said: " & Str));
         Buffers.Release_Contents (S.Buffer.all);
         Marshall_String
           (Rep, S.Buffer, "You said « " & Str & " »"
            & ASCII.CR & ASCII.LF);

         Channels.Send_Data (S.Channel, S.Buffer);
         Buffers.Release_Contents (S.Buffer.all);
      end;

      Channels.Expect_Data (S.Channel, S.Buffer, 1024, False);
      --  Clear buffer and prepare to receive next message.

   end Handle_Data;

   procedure Handle_Connection_Closed (S : access Echo_Session) is
   begin
      pragma Debug (O ("Received disconnect."));

      --  Cleanup protocol.

      Buffers.Release (S.Buffer);

      --  Destroy channel, remove it from the ORB.

      --  Destroy session.

      null;
   end Handle_Connection_Closed;

end Droopi.Protocols.Echo;

