------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S O C K E T S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
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
--                PolyORB is maintained by ACT Europe.                      --
--                    (email: sales@act-europe.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

--  $Id$

with PolyORB.Types;
with PolyORB.Representations.CDR;

package body PolyORB.Utils.Sockets is

   use PolyORB.Buffers;
   use PolyORB.Sockets;
   use PolyORB.Representations.CDR;

   --------------------
   -- String_To_Addr --
   --------------------

   function String_To_Addr
     (Str : Types.String)
     return Inet_Addr_Type
   is
      use PolyORB.Types;
      use PolyORB.Sockets;

      Addr_Image : constant String := To_Standard_String (Str);
      Hostname_Seen : Boolean := False;
   begin
      for J in Addr_Image'Range loop
         if Addr_Image (J) not in '0' .. '9'
           and then Addr_Image (J) /= '.'
         then
            Hostname_Seen := True;
            exit;
         end if;
      end loop;

      if Hostname_Seen then
         return Addresses (Get_Host_By_Name (Addr_Image), 1);
      else
         return Inet_Addr (Addr_Image);
      end if;
   end String_To_Addr;

   ---------------------
   -- Marshall_Socket --
   ---------------------

   procedure Marshall_Socket
     (Buffer : access Buffer_Type;
      Sock   : in     Sock_Addr_Type)
   is
      Str : constant Types.String :=
        PolyORB.Types.To_PolyORB_String (Image (Sock.Addr));
   begin

      --  Marshalling the host name as a string

      Marshall (Buffer, Str);

      --  Marshalling the port

      Marshall (Buffer, Types.Unsigned_Short (Sock.Port));

   end Marshall_Socket;

   -----------------------
   -- Unmarshall_Socket --
   -----------------------

   procedure Unmarshall_Socket
    (Buffer : access Buffer_Type;
     Sock   :    out Sock_Addr_Type)
   is
      Addr_Image : constant Types.String := Unmarshall (Buffer);
      Port : constant Types.Unsigned_Short := Unmarshall (Buffer);
   begin
      Sock.Addr := String_To_Addr (Addr_Image);
      Sock.Port := Port_Type (Port);
   end Unmarshall_Socket;

end PolyORB.Utils.Sockets;
