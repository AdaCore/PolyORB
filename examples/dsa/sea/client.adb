------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2011-2012, Free Software Foundation, Inc.          --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.                                               --
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
with Ada.Text_IO; use Ada.Text_IO;
with Server;

procedure Client is
begin
   Put_Line ("The client has started!");
   Put ("Thus spake my server upon me:");
   declare
      Str : constant String := "Hi SEA!";
      SEA : Server.SEA (1 .. Str'Length);
      for SEA'Address use Str'Address;
      pragma Import (Ada, SEA);

      SEA2 : constant Server.SEA := Server.Echo_SEA (SEA);
      Str2 : String (1 .. SEA2'Length);
      for Str2'Address use SEA2'Address;
      pragma Import (Ada, Str2);
   begin
      Put_Line (Str2);
   end;
exception
   when E : others =>
      Put_Line ("Got " & Ada.Exceptions.Exception_Information (E));
end Client;
