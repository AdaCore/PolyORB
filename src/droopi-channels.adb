--  A communication channel (a transport service protocol entity).

--  $Id$

with Ada.Unchecked_Deallocation;

with Droopi.Log;

package body Droopi.Channels is

   use Droopi.Buffers;
   use Droopi.Log;
   use Droopi.Servers;
   use Droopi.Sockets;

   package L is new Droopi.Log.Facility_Log ("droopi.channels");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   function Create
     (Socket  : Sockets.Socket_Type;
      Session : Protocols.Session_Access;
      Server  : Server_Access)
     return Channel_Access
   is
      Result : constant Channel_Access
        := new Channel;
   begin
      pragma Assert (Result /= null);

      Result.Socket  := Socket;
      Result.Session := Session;
      Result.Server  := Server;
      return Result;
   end Create;

   procedure Free is new Ada.Unchecked_Deallocation
     (Channel'Class, Channel_Access);
   procedure Destroy (C : in out Channel_Access) is
   begin
      Free (C);
   end Destroy;

   ----------------------------
   -- Upper layers interface --
   ----------------------------

   procedure Send_Data
     (C : access Channel;
      B : access Buffers.Buffer_Type) is
   begin
      Droopi.Buffers.Send_Buffer (B, C.Socket);
   end Send_Data;

   procedure Receive_Data
     (C     : access Channel;
      B     : access Buffers.Buffer_Type;
      Size  : Stream_Element_Count;
      Exact : Boolean := True)
   is
   begin
      C.Data_Expected := Size;
      C.Data_Exact    := Exact;
      C.Data_Buffer   := Buffer_Access (B);
      C.Data_Arrived  := False;
      Run (C.Server, Exit_Condition_Access'(C.Data_Arrived'Access));
   end Receive_Data;

   -------------------------------
   -- Callback from lower layer --
   -------------------------------

   procedure Handle_Data
     (C : access Channel;
      S : Sockets.Socket_Type)
   is
      Data_Received : Stream_Element_Count;
   begin
      pragma Debug (O ("Data received on socket"
                       & Image (C.Socket)));

      if C.Data_Buffer = null or else C.Data_Expected = 0 then
         O ("Unexpected data received on fd" & Image (C.Socket));

         --  XXX Should signal error and close channel altogether!
         return;
      end if;

      Droopi.Buffers.Receive_Buffer
        (C.Data_Buffer, C.Socket, C.Data_Expected, Data_Received);
      pragma Assert (Data_Received <= C.Data_Expected);
      C.Data_Expected := C.Data_Expected - Data_Received;

      if C.Data_Expected = 0 or else not C.Data_Exact then
         C.Data_Expected := 0;
         C.Data_Buffer   := null;
         C.Data_Arrived  := True;
      end if;
   end Handle_Data;

end Droopi.Channels;
