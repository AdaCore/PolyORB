--  A communication channel (a transport service protocol entity).

--  $Id$

with Ada.Unchecked_Deallocation;

with Droopi.Log;

package body Droopi.Channels is

   use Droopi.Buffers;
   use Droopi.Log;
   use Droopi.Sockets;

   package L is new Droopi.Log.Facility_Log ("droopi.channels");
   procedure O (Message : in String; Level : Log_Level := Debug)
     renames L.Output;

   procedure Create
     (C      : access Channel;
      Socket : Sockets.Socket_Type) is
   begin
      C.Socket := Socket;

      C.Data_Expected := 0;
      C.Data_Exact    := False;
      C.Data_Buffer   := null;
      C.Data_Arrived  := False;
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

   procedure Expect_Data
     (C     : access Channel;
      B     : Buffers.Buffer_Access;
      Size  : Stream_Element_Count;
      Exact : Boolean := True)
   is
   begin
      C.Data_Expected := Size;
      C.Data_Exact    := Exact;
      C.Data_Buffer   := B;
      C.Data_Arrived  := False;
   end Expect_Data;

   ------------------------------
   -- Interface to lower layer --
   ------------------------------

   procedure Handle_Data
     (C : access Channel;
      Connection_Closed : out Boolean)
   is
      Data_Received : Stream_Element_Count;
   begin
      Connection_Closed := False;

      pragma Debug (O ("Data received on socket"
                       & Image (C.Socket)));

      if C.Data_Buffer = null or else C.Data_Expected = 0 then
         O ("Unexpected data received on fd" & Image (C.Socket));

         Connection_Closed := True;
         Signal_Connection_Closed (Channel_Access (C));
         --  Redispatch.
         return;
      end if;

      Droopi.Buffers.Receive_Buffer
        (C.Data_Buffer, C.Socket, C.Data_Expected, Data_Received);

      if Data_Received = 0 then
         Connection_Closed := True;

         Signal_Connection_Closed (Channel_Access (C));
         --  Redispatch.
         return;
      end if;

      pragma Assert (Data_Received <= C.Data_Expected);
      C.Data_Expected := C.Data_Expected - Data_Received;

      if C.Data_Expected = 0 or else not C.Data_Exact then
         C.Data_Expected := 0;
            C.Data_Buffer   := null;
            C.Data_Arrived  := True;

            Signal_Data (Channel_Access (C));
            --  Redispatch.
      end if;

   end Handle_Data;

end Droopi.Channels;
