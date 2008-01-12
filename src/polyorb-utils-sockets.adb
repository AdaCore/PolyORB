------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S O C K E T S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2008, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.Strings.Fixed;

with PolyORB.Log;
with PolyORB.Representations.CDR.Common;
with PolyORB.Types;

package body PolyORB.Utils.Sockets is

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use PolyORB.Buffers;
   use PolyORB.Log;
   use PolyORB.Sockets;
   use PolyORB.Representations.CDR.Common;

   package L is new PolyORB.Log.Facility_Log ("polyorb.utils.sockets");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   ---------
   -- "+" --
   ---------

   function "+"
     (Host_Name : String;
      Port      : PolyORB.Sockets.Port_Type) return Socket_Name
   is
   begin
      return Socket_Name'(Name_Len  => Host_Name'Length,
                          Host_Name => Host_Name,
                          Port      => Port);
   end "+";

   --------------------
   -- Connect_Socket --
   --------------------

   procedure Connect_Socket
     (Sock        : PolyORB.Sockets.Socket_Type;
      Remote_Name : Socket_Name)
   is

      function Try_One_Address
        (Remote_Addr : Sock_Addr_Type; Last : Boolean) return Boolean;
      --  Try one of Remote_Name's aliases. Return True for a successful
      --  connection. For a failed connection, if Last is False, there are
      --  other addresses to try, so we return False to indicate non-fatal
      --  failure. Otherwise an exception is propagated.

      ---------------------
      -- Try_One_Address --
      ---------------------

      function Try_One_Address
        (Remote_Addr : Sock_Addr_Type; Last : Boolean) return Boolean
      is
         Remote_Addr_Var : Sock_Addr_Type := Remote_Addr;
         --  WAG:62
         --  Connect_Socket should take a parameter of mode IN, not IN OUT
      begin
         pragma Debug
           (O ("... trying " & Image (Remote_Addr)));
         PolyORB.Sockets.Connect_Socket (Sock, Remote_Addr_Var);
         return True;
      exception
         when Socket_Error =>
            if Last then
               raise;
            else
               return False;
            end if;
      end Try_One_Address;

      Host_Name        : String renames Remote_Name.Host_Name;

   begin
      pragma Debug
        (O ("connect socket" & Image (Sock)
            & " to " & Image (Remote_Name)));

      if Is_IP_Address (Host_Name) then
         if not Try_One_Address
           ((Family => Family_Inet,
             Addr   => Inet_Addr (Host_Name),
             Port   => Remote_Name.Port), Last => True)
         then
            --  Should never happen, Last = True so in case of error,
            --  an exception is expected.

            raise Program_Error;
         end if;
      else
         declare
            Host_Entry       : constant Host_Entry_Type :=
                                 Get_Host_By_Name (Host_Name);
            Addresses_Len : constant Natural :=
                              PolyORB.Sockets.Addresses_Length (Host_Entry);
         begin
            --  Iterate over all addresses associated with name

            for J in 1 .. Addresses_Len loop
               if Try_One_Address
                 ((Family => Family_Inet,
                   Addr   => Addresses (Host_Entry, J),
                   Port   => Remote_Name.Port),
                  Last => J = Addresses_Len)
               then
                  --  Success

                  return;
               end if;
            end loop;

            --  Never reached, last iteration above must either exit
            --  succesfully or raise Socket_Error.

            raise Program_Error;
         end;
      end if;

   exception
      when E : PolyORB.Sockets.Socket_Error =>
         O ("connect to " & Host_Name & " failed: "
            & Ada.Exceptions.Exception_Message (E), Notice);
         raise;
   end Connect_Socket;

   -----------
   -- Image --
   -----------

   function Image (SN : Socket_Name) return String is
   begin
      return SN.Host_Name & ":" & Trim (SN.Port'Img, Left);
   end Image;

   -------------------
   -- Is_IP_Address --
   -------------------

   function Is_IP_Address (Name : String) return Boolean is
   begin
      for J in Name'Range loop
         if Name (J) /= '.'
           and then Name (J) not in '0' .. '9'
         then
            return False;
         end if;
      end loop;

      return True;
   end Is_IP_Address;

   ---------------------
   -- Marshall_Socket --
   ---------------------

   procedure Marshall_Socket
     (Buffer : access Buffer_Type;
      Sock   : Socket_Name)
   is
   begin
      --  Marshall the host name as a string

      Marshall_Latin_1_String (Buffer, Sock.Host_Name);

      --  Marshall the port

      Marshall (Buffer, Types.Unsigned_Short (Sock.Port));
   end Marshall_Socket;

   -----------------------
   -- Unmarshall_Socket --
   -----------------------

   function Unmarshall_Socket
     (Buffer : access Buffer_Type) return Socket_Name
   is
      Host_Name : constant String := Unmarshall_Latin_1_String (Buffer);
      Port      : constant Types.Unsigned_Short := Unmarshall (Buffer);
   begin
      return Host_Name + Port_Type (Port);
   end Unmarshall_Socket;

   --------------------
   -- String_To_Addr --
   --------------------

   function String_To_Addr (Str : Standard.String) return Inet_Addr_Type is
      use PolyORB.Types;

      Hostname_Seen : Boolean := False;
   begin
      for J in Str'Range loop
         if Str (J) not in '0' .. '9'
           and then Str (J) /= '.'
         then
            Hostname_Seen := True;
            exit;
         end if;
      end loop;

      if Hostname_Seen then
         return Addresses (Get_Host_By_Name (Str), 1);
      else
         return Inet_Addr (Str);
      end if;
   end String_To_Addr;

end PolyORB.Utils.Sockets;
