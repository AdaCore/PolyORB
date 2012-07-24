------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . U T I L S . S O C K E T S                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
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
     (Sock        : in out PolyORB.Sockets.Socket_Type;
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
         pragma Warnings (Off, Remote_Addr_Var); --  WAG:61
         --  Connect_Socket should take a parameter of mode IN, not IN OUT.
         --  This is fixed in GNAT 6.2, and we can do away with this variable
         --  when earlier versions aren't supported anymore. In the meantime,
         --  we need to keep the variable, and for 6.2 and later compilers we
         --  kill the "variable not modified" warning.
      begin
         pragma Debug
           (C, O ("... trying " & Image (Remote_Addr)));
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
        (C, O ("connect socket" & Image (Sock)
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
         pragma Debug
           (C, O ("connect to " & Host_Name & " failed: "
                  & Ada.Exceptions.Exception_Message (E)));
         PolyORB.Sockets.Close_Socket (Sock);
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
         if Name (J) /= '.' and then Name (J) not in '0' .. '9' then
            return False;
         end if;
      end loop;

      return True;
   end Is_IP_Address;

   ------------------------
   -- Local_Inet_Address --
   ------------------------

   function Local_Inet_Address return Inet_Addr_Type is
      Host_Entry : constant Host_Entry_Type := Get_Host_By_Name (Host_Name);
      Candidate  : Inet_Addr_Type := No_Inet_Addr;

   begin
      for J in 1 .. Host_Entry.Addresses_Length loop
         Candidate := Addresses (Host_Entry, J);
         exit when not Has_Prefix (Image (Candidate), Prefix => "127.");

         --  Should use netmask manipulation on Candidate instead of string
         --  image???

      end loop;
      return Candidate;
   end Local_Inet_Address;

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

   ----------------
   -- To_Address --
   ----------------

   function To_Address (SN : Socket_Name) return Sock_Addr_Type is
   begin
      return Result : Sock_Addr_Type do
         if Is_IP_Address (SN.Host_Name) then
            Result.Addr := Inet_Addr (SN.Host_Name);
         else
            Result.Addr := Addresses (Get_Host_By_Name (SN.Host_Name), 1);
         end if;

         Result.Port := SN.Port;
      end return;
   end To_Address;

end PolyORB.Utils.Sockets;
