------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      E V O L U T E D P _ C O R B A                       --
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

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with Exceptions;       use Exceptions;
with Evoluted_CORBA;     use Evoluted_CORBA;
with DSA_Server;
with DSA_Server.Helper;
with DSA_Common.Penpal_Type.Impl;

with PolyORB.CORBA_P.Naming_Tools;
with PolyORB.CORBA_P.Server_Tools;

with CORBA; use CORBA;
with CORBA.ORB;

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

procedure EvolutedP_CORBA is

   --  This program is launched using: evoluted "pseudo"

   procedure Usage;
   --  Print usage

   -----------
   -- Usage --
   -----------

   procedure Usage is
   begin
      Put_Line ("Usage: evoluted ""nickname""");
      Set_Exit_Status (1);
   end Usage;

begin
   CORBA.ORB.Initialize ("ORB");
   PolyORB.CORBA_P.Server_Tools.Initiate_Server (Start_New_Task => True);
   My_Server := DSA_Server.Helper.To_Ref
     (PolyORB.CORBA_P.Naming_Tools.Locate ("server.RCI"));
   if DSA_Server.Is_Nil (My_Server) then
      Ada.Text_IO.Put_Line ("main : cannot invoke on a nil reference");
      return;
   end if;

   if Argument_Count = 1 then
      declare
         use DSA_Common.Penpal_Type.Impl;
         use DSA_Server;
         Penpal_Ref : Penpal_Pointer;
      begin
         Ada.Text_IO.Put_Line ("Initializing local penpal...");
         Initialize (Penpal'Access, To_CORBA_String (Argument (1)));
         PolyORB.CORBA_P.Server_Tools.Initiate_Servant
           (Penpal'Access, Penpal_Ref);
         Ada.Text_IO.Put_Line (" registering...");
         Register (My_Server, Penpal_Ref);
         Ada.Text_IO.Put_Line (" done.");
         Mainloop;
      end;
   else
      Usage;
      Set_Exit_Status (1);
   end if;
exception
   when Sender_Error =>
      Put_Line ("Invalid nickname");
      Set_Exit_Status (2);
end EvolutedP_CORBA;
