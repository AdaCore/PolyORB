with Sockets.Thin;
--  with Broca.Locks; use Broca.Locks;

package body Broca.Stream is
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

   procedure Send (Stream : access Fd_Stream_Type;
                   Buffer : in Buffer_Descriptor)
   is
      use Sockets.Thin;
      use Interfaces.C;
      Len : Interfaces.C.int;
   begin
      Stream.Lock_S.Check_Owner;
      Len := Interfaces.C.int (Buffer.Pos);
      if C_Send (Stream.Fd, Buffer.Buffer.all'Address, Len, 0) /= Len then
         raise Connection_Closed;
      end if;
   end Send;

   procedure Receive
     (Stream : access Fd_Stream_Type; Buffer : in out Buffer_Descriptor)
   is
      use Sockets.Thin;
      use Interfaces.C;
      Len : Interfaces.C.int;
   begin
      Stream.Lock_R.Check_Owner;
      Len := Interfaces.C.int (Buffer.Pos);
      Len := C_Recv (Stream.Fd, Buffer.Buffer.all'Address, Len, 0);
      if Len < 0 then
         raise Connection_Closed;
      else
         Buffer.Pos := Buffer_Index_Type (Len);
      end if;
   end Receive;

   function Create_Fd_Stream (Fd : Interfaces.C.int) return Stream_Acc is
      Res : Stream_Acc;
   begin
      Res := new Fd_Stream_Type;
      Fd_Stream_Type (Res.all).Fd := Fd;
      return Res;
   end Create_Fd_Stream;

end Broca.Stream;

