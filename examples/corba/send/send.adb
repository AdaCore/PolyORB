------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                                 S E N D                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2003 Free Software Foundation, Inc.           --
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


with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with CORBA.ORB;

with Test.Printer;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Send is
   Howmany : Integer := 1;
   type Send_Type is (Long, String, EchoString, EchoLong);
   T : Send_Type := String;
   K : Integer;
   Tempo : Integer;
begin
   CORBA.ORB.Initialize ("ORB");
   if Argument_Count < 1 then
      Put_Line ("usage : ./send <IOR> [number of calls] "
                & "[time between calls] [type]");
      Put_Line ("type = s | l | tws | twl ");
      Put_Line ("       s   : one way call, send a string (default)");
      Put_Line ("       l   : one way call, send a long");
      Put_Line ("       tws : two way call, echo a string");
      Put_Line ("       twl : two way call, echo a long");
      return;
   end if;

   if Argument_Count >= 2 then
      Howmany := Integer'Value (Argument (2));
   end if;

   if Argument_Count >= 3 then
      Tempo := Integer'Value (Argument (3));
   else
      Tempo := 1;
   end if;

   if Argument_Count >= 4 then
      if Argument (4) = "l" then
         T := Long;
      elsif Argument (4) = "tws" then
         T := EchoString;
      elsif Argument (4) = "twl" then
         T := EchoLong;
      end if;
   end if;

   declare
      Sent_Msg : constant Standard.String
        := "Hello Multicast world !";
--          & "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"
--          & "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"
--          & "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"
--          & "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"
--          & "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggg"
--          & "gggggggggggggggggggggggggggggggggggggggggggggggggggggggggggga";
      myprint : Test.Printer.Ref;
   begin
      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), myprint);

      --  checking if it worked
      if Test.Printer.Is_Nil (myprint) then
         Put_Line ("main : cannot invoke on a nil reference");
         return;
      end if;

      --  sending message
      Put_Line ("Mode : " & T'Img);
      while Howmany > 0 loop
         case T is
            when String =>
               Test.Printer.printString
                 (myprint,
                  CORBA.To_CORBA_String (Howmany'Img & " " & Sent_Msg));
            when EchoString =>
               declare
                  use CORBA;
                  Str : constant CORBA.String
                    := To_CORBA_String
                    (Howmany'Img & " " & Sent_Msg);
               begin
                  if Str /= Test.Printer.echoString (myprint, Str) then
                     Put_Line ("Bad return value");
                  end if;
               end;
            when EchoLong =>
               declare
                  use CORBA;

                  L : constant CORBA.Long := CORBA.Long (Howmany);
               begin
                  if L /= Test.Printer.echoLong (myprint, L) then
                     Put_Line ("Bad return value");
                  end if;
               end;
            when Long =>
               Test.Printer.printLong (myprint, CORBA.Long (Howmany));
         end case;
         Howmany := Howmany - 1;
         K := 0;
         for I in 1 .. Tempo loop
            K := K + 1;
         end loop;
         pragma Assert (K = Tempo);
      end loop;
   exception
      when E : CORBA.Transient =>
         declare
            Memb : CORBA.System_Exception_Members;
         begin
            CORBA.Get_Members (E, Memb);
            Put ("received exception transient, minor");
            Put (CORBA.Unsigned_Long'Image (Memb.Minor));
            Put (", completion status: ");
            Put_Line (CORBA.Completion_Status'Image (Memb.Completed));
         end;
   end;
end Send;
