------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             E V O L U T E D                              --
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

--  Evoluted BBS client
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Calendar;     use Ada.Calendar;
with Exceptions;       use Exceptions;
with Evoluted_Pkg;     use Evoluted_Pkg;
with Server;           use Server;
with Common;
with Utils;

with PolyORB.Dynamic_Dict;

with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;

procedure Evoluted is

   --  This program is launched using: evoluted "pseudo"

   procedure Usage;
   --  Print usage

   package Penpals_Cache is new PolyORB.Dynamic_Dict (Penpal_Pointer);

   function Cache_Get_Penpal (P : String) return Penpal_Pointer;
   function Cache_Get_Penpal (P : String) return Penpal_Pointer is
      PP : Penpal_Pointer := Penpals_Cache.Lookup (P, null);
   begin
      if PP = null then
         PP := Get_Penpal (P);
         Penpals_Cache.Register (P, PP);
      end if;
      return PP;
   end Cache_Get_Penpal;

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage: evoluted ""nickname""");
      Set_Exit_Status (1);
   end Usage;

   Is_Test : Boolean := False;
   Test_Broadcast : Boolean := False;
   Message_Count : Integer := 100;
   Message_Size : Integer  := 100;
   Nmax : Integer;
   Expected_Messages : Integer;
   type String_Ptr is access String;
   Payload : String_Ptr;
   Start : Time;

   function Image (I : Integer) return String;
   function Image (I : Integer) return String
   is
      S : constant String := Integer'Image (I);
      First : Integer := S'First;
   begin
      if S (First) = ' ' then
         First := First + 1;
      end if;
      return S (First .. S'Last);
   end Image;

begin
   loop
      case Getopt ("b c: n: s:") is
         when ASCII.NUL => exit;

         when 'b' =>
            Test_Broadcast := True;

         when 'c' =>
            Message_Count := Integer'Value (Parameter);

         when 'n' =>
            Nmax := Integer'Value (Parameter);
            Is_Test := True;

         when 's' =>
            Message_Size := Integer'Value (Parameter);

         when others =>
            raise Program_Error;
      end case;
   end loop;

   declare
      Penpal_Name : constant String := Get_Argument;
   begin
      if Penpal_Name'Length = 0 then
         Usage;
         GNAT.OS_Lib.OS_Exit (1);
      end if;

      Put ("Initializing local penpal...");
      Initialize (Penpal, Penpal_Name);
      Put (" registering...");
      Register (Penpal'Access);
      Put_Line (" done.");

      if not Is_Test then
         Mainloop;
         GNAT.OS_Lib.OS_Exit (0);
      end if;

      ----------------------------
      -- Automated test section --
      ----------------------------

      Expected_Messages := Nmax * Message_Count;
      if Test_Broadcast then
         Expected_Messages := Expected_Messages * 2;
      end if;
      Received_Counter.Set_Expected (Expected_Messages);
      Payload := new String'(1 .. Message_Size => 'X');

      Put_Line ("Expecting"
                  & Expected_Messages'Img & " messages.");
      declare
         Dummy : constant String := Utils.Get_Line ("Ready>");
         pragma Unreferenced (Dummy);
      begin
         null;
      end;

      Start := Clock;
      for K in 0 .. Message_Count - 1 loop
         declare
            Iter : constant String := Integer'Image (K);
         begin
            if Test_Broadcast then
               Post_Message
                 (Sender  => Name_Of (Penpal'Access),
                  Message => "B" & Iter & ":" & Penpal_Name
                    & ":" & Payload.all);
            end if;

            for J in 0 .. Nmax - 1 loop
               declare
                  To : constant String := "TEST_" & Image (J);
               begin
                  Common.New_Message
                    (Sender    => Name_Of (Penpal'Access),
                     Recipient => Cache_Get_Penpal (To),
                     Message   => "P" & Iter & ":"
                       & Penpal_Name & ":" & To
                       & ":" & Payload.all);
               end;
            end loop;
         exception
            when E : others =>
               Put_Line ("Raised exception in iteration"
                 & Integer'Image (K) & ": "
                 & Ada.Exceptions.Exception_Information (E));
               GNAT.OS_Lib.OS_Exit (1);
         end;
      end loop;

      Ada.Text_IO.Put_Line ("Send done.");
      Received_Counter.Wait_For_Completion;
      declare
         Elapsed : constant Duration := Clock - Start;
      begin
         Put_Line ("Elapsed :" & Duration'Image (Elapsed));
      end;
   exception
      when E : others =>
         Put_Line (Ada.Exceptions.Exception_Information (E));
   end;
   delay 5.0;
   GNAT.OS_Lib.OS_Exit (0);

exception
   when Sender_Error =>
      Put_Line ("Invalid nickname");
      Set_Exit_Status (2);
end Evoluted;
