------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               C L I E N T                                --
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

--   echo client.

--  $Id: //droopi/main/examples/corba/echo/client.adb#5 $

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with CORBA.ORB;

with Echo;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

procedure Client is
   Sent_Msg, Rcvd_Msg : CORBA.String;
   myecho : Echo.Ref;

begin
   CORBA.ORB.Initialize ("ORB");
   if Argument_Count < 1 then
      Put_Line ("usage : client <IOR_string_from_server>|-i");
      return;
   end if;

   --  getting the CORBA.Object
   --  if Argument (1) = "-i" then
   --     myecho := Echo.Helper.To_Ref (Locate ("echo"));
   --  else
   --     myecho := Echo.Helper.To_Ref (Locate (Argument (1)));
   --  end if;
   CORBA.ORB.String_To_Object
     (CORBA.To_CORBA_String (Ada.Command_Line.Argument (1)), myecho);

   --  checking if it worked
   if Echo.Is_Nil (myecho) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  sending message
   Sent_Msg := CORBA.To_CORBA_String (Standard.String'("Hello Ada !"));
   Rcvd_Msg := Echo.echoString (myecho, Sent_Msg);

   --  printing result
   Put_Line ("I said : " & CORBA.To_Standard_String (Sent_Msg));
   Put_Line ("The object answered : " & CORBA.To_Standard_String (Rcvd_Msg));
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
end Client;
