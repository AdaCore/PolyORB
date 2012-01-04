------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 0 _ C L I E N T                        --
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

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Text_IO;

with CORBA.ORB;

with PolyORB.Setup.Client;
pragma Warnings (Off, PolyORB.Setup.Client);

with PolyORB.Utils.Report; use PolyORB.Utils.Report;
with Test_Interface;

procedure Test000_Client is
   use Ada.Command_Line;
   use Ada.Text_IO;

   IOR : constant String := Ada.Command_Line.Argument (1);
   Ref, Invalid_Ref : Test_Interface.Ref;
   Got_Comm_Failure : Boolean;

begin
   New_Test ("Test000_Client");

   CORBA.ORB.Initialize ("ORB");

   if Argument_Count /= 1 then
      Put_Line ("usage : client <IOR_string_from_server>|-i");
      return;
   end if;

   --  Getting the CORBA.Object

   CORBA.ORB.String_To_Object (CORBA.To_CORBA_String (IOR), Ref);

   --  Checking if it worked

   if Test_Interface.Is_Nil (Ref) then
      Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   --  Sending message

   Output ("Non_Existent on valid server ref",
     not Test_Interface.Non_Existent (Ref));

   Invalid_Ref := Test_Interface.Ref (Test_Interface.Get_Invalid_Ref (Ref));
   Output ("Non_Existent on invalid server ref",
     Test_Interface.Non_Existent (Invalid_Ref));

   Test_Interface.Terminate_Server (Ref);

   Got_Comm_Failure := False;
   declare
      B : Boolean;
      pragma Unreferenced (B);
   begin
      B := Test_Interface.Non_Existent (Ref);
   exception
      when CORBA.Comm_Failure =>
         Got_Comm_Failure := True;
   end;
   Output ("Non_Existent on dead server gets COMM_FAILURE", Got_Comm_Failure);

   End_Report;

exception
   when E : others =>
      declare
         Memb : CORBA.System_Exception_Members;
      begin
         CORBA.Get_Members (E, Memb);
         Put_Line ("received exception "
           & Ada.Exceptions.Exception_Name (E) & ", minor"
           & Memb.Minor'Img & ", completion status: "
           & Memb.Completed'Img);

         Output ("Unexpected exception", False);

         End_Report;
      end;
end Test000_Client;
