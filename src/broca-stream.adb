------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                         B R O C A . S T R E A M                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2000 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
-- not, write to the Free Software Foundation, 59 Temple Place - Suite 330, --
-- Boston, MA 02111-1307, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Sockets.Thin;

with Broca.Buffers.IO_Operations;
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
      Buffer : access Buffer_Type) is
   begin
      Stream.Lock_S.Check_Owner;
      --  Read (Buffer, Bytes);
      pragma Debug (O ("Dump outgoing buffer"));
      Broca.Buffers.Show (Buffer.all);
      begin
         Broca.Buffers.IO_Operations.Write_To_FD
           (Stream.Fd,
            Buffer);
      exception
         when others =>
            raise Connection_Closed;
      end;
   end Send;

   function Receive
     (Stream : access Fd_Stream_Type;
      Length : Index_Type)
     return Octet_Array
   is
      use Sockets.Thin;
      use Interfaces.C;

      Bytes : Octet_Array (1 .. Length);
      Result : Interfaces.C.int;
   begin
      Stream.Lock_R.Check_Owner;
      Result := C_Recv
        (Stream.Fd,
         Bytes (1)'Address,
         Interfaces.C.int (Length), 0);

      if Result /=  Interfaces.C.int (Length) then
         raise Connection_Closed;
      end if;

      --  Write (Buffer, Bytes);

      --  pragma Debug (O ("Receive: got " & Length'Img & " bytes"));
      --  Broca.Buffers.Dump (Bytes);
      return Bytes;
   end Receive;

   function Create_Fd_Stream (Fd : Interfaces.C.int) return Stream_Ptr is
      Res : Stream_Ptr;
   begin
      Res := new Fd_Stream_Type;
      Fd_Stream_Type (Res.all).Fd := Fd;
      return Res;
   end Create_Fd_Stream;

end Broca.Stream;

