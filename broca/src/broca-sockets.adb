with Broca.Debug;
with Sockets.Thin; use Sockets.Thin;
with System.Storage_Elements;        use System, System.Storage_Elements;

package body Broca.Sockets is

   procedure O is
     new Broca.Debug.Output (Broca.Debug.Is_Active ("broca.sockets"));

   use Broca.Opaque, Interfaces.C;

   -------------
   -- Receive --
   -------------

   function Receive
     (Socket : int;
      Buffer : Address;
      Length : Index_Type)
     return Index_Type
   is
      Offset : Storage_Offset := 0;
      Rest   : Index_Type     := Length;
      RP     : int;
   begin
      pragma Debug
        (O ("Waiting for" & Length'Img & " bytes on FD" & Socket'Img));
      while Rest > 0 loop
         pragma Debug (O ("Still" & Rest'Img & " bytes to read"));
         RP := C_Recv (Socket, Buffer + Offset, int (Rest), 0);
         exit when RP <= 0;
         Offset := Offset + Storage_Offset (RP);
         Rest   := Rest - Index_Type (RP);
      end loop;
      pragma Debug (O ("Read" & Index_Type'Image (Length - Rest) &
                       " bytes of" & Length'Img));
      return Length - Rest;
   end Receive;

   -------------
   -- Receive --
   -------------

   function Receive
     (Socket : int;
      Buffer : access Octet_Array;
      Length : Index_Type)
     return Index_Type
   is
   begin
      return Receive (Socket, Buffer.all'Address, Length);
   end Receive;

end Broca.Sockets;
