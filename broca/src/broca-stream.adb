with Sockets.Thin;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Stream is

   Flag : constant Natural := Broca.Debug.Is_Active ("broca.stream");
   procedure O is new Broca.Debug.Output (Flag);

   procedure Lock_Send (Stream : access Stream_Type) is
   begin
      Stream.Lock_S.Lock;
   end Lock_Send;

   procedure Unlock_Send (Stream : access Stream_Type) is
   begin
      Stream.Lock_S.Unlock;
   end Unlock_Send;

   procedure Lock_Receive (Stream : access Stream_Type) is
   begin
      Stream.Lock_R.Lock;
   end Lock_Receive;

   procedure Unlock_Receive (Stream : access Stream_Type) is
   begin
      Stream.Lock_R.Unlock;
   end Unlock_Receive;

   procedure Send
     (Stream : access Fd_Stream_Type;
      Buffer : in out Buffer_Descriptor)
   is
      use Sockets.Thin;
      use Interfaces.C;
      Length : Buffer_Index_Type := Full_Size (Buffer);
      Bytes  : Buffer_Type (0 .. Length - 1);
      Result : Interfaces.C.int;
   begin
      Stream.Lock_S.Check_Owner;
      Read (Buffer, Bytes);
      pragma Debug (O ("Dump outgoing buffer of length" & Length'Img));
      Broca.Buffers.Dump (Bytes);
      Result := C_Send
        (Stream.Fd,
         Bytes'Address,
         Interfaces.C.int (Length), 0);
      if Result /= Interfaces.C.int (Length) then
         raise Connection_Closed;
      end if;
   end Send;

   procedure Receive
     (Stream : access Fd_Stream_Type;
      Buffer : in out Buffer_Descriptor)
   is
      use Sockets.Thin;
      use Interfaces.C;
      Length : Buffer_Index_Type := Size_Left (Buffer);
      Bytes  : Buffer_Type (0 .. Length - 1);
      Result : Interfaces.C.int;
   begin
      Stream.Lock_R.Check_Owner;
      Result := C_Recv
        (Stream.Fd,
         Bytes'Address,
         Interfaces.C.int (Length), 0);

      if Result /=  Interfaces.C.int (Length) then
         raise Connection_Closed;
      end if;
      Write (Buffer, Bytes);

      pragma Debug (O ("Dump incoming buffer of length" & Length'Img));
      Broca.Buffers.Dump (Bytes);
   end Receive;

   function Create_Fd_Stream (Fd : Interfaces.C.int) return Stream_Ptr is
      Res : Stream_Ptr;
   begin
      Res := new Fd_Stream_Type;
      Fd_Stream_Type (Res.all).Fd := Fd;
      return Res;
   end Create_Fd_Stream;

end Broca.Stream;

