--  A communication channel (a transport service protocol entity).

-- $Id$

with Ada.Unchecked_Deallocation;

with Droopi.Log;

package body Droopi.Channels is

   use Droopi.Log;
   use Droopi.Sockets;

   package L is new Droopi.Log.Facility_Log ("droopi.channels");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   function Create
     (Socket : Sockets.Socket_Type;
      Session : Protocols.Session_Access)
     return Channel_Access
   is
      Result : constant Channel_Access
        := new Channel;
   begin
      pragma Assert (Result /= null);

      Result.Socket := Socket;
      Result.Session := Session;
      return Result;
   end Create;

   procedure Free is new Ada.Unchecked_Deallocation
     (Channel'Class, Channel_Access);
   procedure Destroy (C : in out Channel_Access) is
   begin
      Free (C);
   end Destroy;

   procedure Send_Data
     (C : access Channel;
      B : access Buffers.Buffer_Type) is
   begin
      Droopi.Buffers.Send_Buffer (B, C.Socket);
   end Send_Data;

   procedure Expect_Data (C : access Channel; Size : Natural) is
   begin
      raise Not_Implemented;
   end Expect_Data;

   procedure Handle_Data
     (C : access Channel;
      S : Sockets.Socket_Type) is
   begin
      O ("Data received on socket " & Image (C.Socket));
   end Handle_Data;

end Droopi.Channels;


