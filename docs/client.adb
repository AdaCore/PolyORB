------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2012, Free Software Foundation, Inc.          --
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

--  Echo client

with Ada.Command_Line;
with Ada.Text_IO;
with CORBA.ORB;

with Echo;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Utils.Report;

procedure Client is
   use Ada.Command_Line;
   use Ada.Text_IO;
   use PolyORB.Utils.Report;
   use type CORBA.String;

   Sent_Msg, Rcvd_Msg : CORBA.String;
   myecho : Echo.Ref;

   Length : Natural := 0;
   Calls  : Natural := 1;
begin
   New_Test ("Echo client");

   CORBA.ORB.Initialize ("ORB");
   if Argument_Count not in 1 .. 3 then
      Put_Line
        ("usage: client <IOR_string_from_server>|-i [strlen [num of calls]]");
      return;
   end if;

   --  Getting the CORBA.Object

   CORBA.ORB.String_To_Object
     (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), myecho);

   --  Get optional arguments Length and Calls

   if Argument_Count >= 2 then
      Length := Natural'Value (Ada.Command_Line.Argument (2));
   end if;

   if Argument_Count >= 3 then
      Calls := Natural'Value (Ada.Command_Line.Argument (3));
   end if;

   --  Checking if it worked

   if Echo.Is_Nil (myecho) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  Sending message

   if Length = 0 then
      Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
   else
      Sent_Msg := CORBA.To_CORBA_String
                    (Standard.String'(1 .. Length => 'X'));
   end if;

   for J in 1 .. Calls loop
      Rcvd_Msg := Echo.echoString (myecho, Sent_Msg);
   end loop;

   if Rcvd_Msg /= Sent_Msg then
      raise Program_Error with "incorrect string returned by server";
   end if;

   --  Printing result

   if Length = 0 then
      Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));
      Put_Line
        ("The object answered : " & CORBA.To_Standard_String (Rcvd_Msg));

   else
      Put_Line ("I said : 'X' *" & Length'Img);
      Put_Line ("The object answered the same.");
   end if;

   End_Report;

exception
   when E : CORBA.Transient =>
      Output ("echo test", False);
      declare
         Memb : CORBA.System_Exception_Members;
      begin
         CORBA.Get_Members (E, Memb);
         Put ("received exception transient, minor");
         Put (CORBA.Unsigned_Long'Image (Memb.Minor));
         Put (", completion status: ");
         Put_Line (CORBA.Completion_Status'Image (Memb.Completed));

         End_Report;
      end;
end Client;
