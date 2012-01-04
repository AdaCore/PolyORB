------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          T E S T _ C L I E N T                           --
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

with Ada.Exceptions;

with CORBA.ORB;

with PolyORB.Utils.Report; use PolyORB.Utils.Report;

with PolyORB.CORBA_P.Server_Tools; use PolyORB.CORBA_P.Server_Tools;
with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with Test_Interface;
with Test_Interface.Impl; use Test_Interface.Impl;

procedure Test_Client (Wait : Boolean) is
   Ref : Test_Interface.Ref;

begin
   New_Test ("Test_Client: Wait = " & Wait'Img);

   CORBA.ORB.Initialize ("ORB");

   --  Set up local server in separate task

   Initiate_Servant (new Test_Interface.Impl.Object, Ref);
   Initiate_Server (Start_New_Task => True);

   --  Checking if it worked

   if Test_Interface.Is_Nil (Ref) then
      Output ("cannot invoke on a nil reference", False);
      return;
   end if;

   Test_Interface.Trigger_Server_Shutdown (Ref, Wait);
   Client_Terminated := True;

exception
   when E : others =>
      Output ("Unexpected exception "
              & Ada.Exceptions.Exception_Information (E), False);
      End_Report;
end Test_Client;
