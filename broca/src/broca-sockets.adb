------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                        B R O C A . S O C K E T S                         --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
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

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);
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
