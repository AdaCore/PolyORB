------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                   "   &   S G P   &   " . C O N F I G                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision$
--                                                                          --
--         Copyright (C) 1996-2001 Free Software Foundation, Inc.           --
--                                                                          --
-- GARLIC is free software;  you can redistribute it and/or modify it under --
-- terms of the  GNU General Public License  as published by the Free Soft- --
-- ware Foundation;  either version 2,  or (at your option)  any later ver- --
-- sion.  GARLIC is distributed  in the hope that  it will be  useful,  but --
-- WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABI- --
-- LITY or  FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public  --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License  distributed with GARLIC;  see file COPYING.  If  --
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
--               GLADE  is maintained by ACT Europe.                        --
--               (email: glade-report@act-europe.fr)                        --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;

procedure B_GaPrCo is
   Up_To_Low : constant Natural := Character'Pos ('a') - Character'Pos ('A');
   Protocol  : String := Argument (1);
   SGP       : constant String := "System.Garlic.Protocols";

begin
   if Protocol (1) in 'a' .. 'z' then
      Protocol (1) := Character'Val (Character'Pos (Protocol (1)) - Up_To_Low);
   end if;
   for I in 2 .. Protocol'Last loop
      if Protocol (I) in 'A' .. 'Z' then
         Protocol (I)
           := Character'Val (Character'Pos (Protocol (I)) + Up_To_Low);
      end if;
   end loop;
   declare
      P : constant String := SGP & "." & Protocol;
      S : constant String := P & ".Server";
   begin
      Ada.Text_IO.Put_Line ("with " & P & ";");
      Ada.Text_IO.Put_Line ("with " & S & ";");
      Ada.Text_IO.Put_Line ("pragma Elaborate_All (" & S & ");");
      Ada.Text_IO.Put_Line ("pragma Warnings (Off, " & S & ");");
      Ada.Text_IO.Put_Line ("package body " & SGP & ".Config is");
      Ada.Text_IO.Put_Line ("   procedure Initialize is");
      Ada.Text_IO.Put_Line ("   begin");
      Ada.Text_IO.Put_Line ("      Register (" & P & ".Create);");
      Ada.Text_IO.Put_Line ("   end Initialize;");
      Ada.Text_IO.Put_Line ("end " & SGP & ".Config;");
   end;

end B_GaPrCo;
