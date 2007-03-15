------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 0 _ C L I E N T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
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
