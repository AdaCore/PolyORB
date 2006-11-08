------------------------------------------------------------------------------
--                                                                          --
--                            GLADE COMPONENTS                              --
--                                                                          --
--                             B _ G A P R C O                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 1996-2006 Free Software Foundation, Inc.           --
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
with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

procedure B_GaPrCo is
   Up_To_Low : constant Natural := Character'Pos ('a') - Character'Pos ('A');
   Protocol  : String := Argument (1);
   SGP       : constant String := "System.Garlic.Protocols";
   Outfile   : File_Type;

   procedure Put_Line (S : String);
   --  Local version of Put_Line ensures Unix style line endings

   procedure Put_Line (S : String) is
   begin
      String'Write (Stream (Outfile), S);
      Character'Write (Stream (Outfile), ASCII.LF);
   end Put_Line;

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

   Create (File => Outfile, Name => "s-gaprco.adb");

   declare
      P : constant String := SGP & "." & Protocol;
      S : constant String := P & ".Server";
   begin
      Put_Line ("with " & P & ";");
      Put_Line ("with " & S & ";");
      Put_Line ("pragma Elaborate_All (" & S & ");");
      Put_Line ("pragma Warnings (Off, " & S & ");");
      Put_Line ("package body " & SGP & ".Config is");
      Put_Line ("   procedure Initialize is");
      Put_Line ("   begin");
      Put_Line ("      Register (" & P & ".Create);");
      Put_Line ("   end Initialize;");
      Put_Line ("end " & SGP & ".Config;");
   end;

   Close (Outfile);
end B_GaPrCo;
