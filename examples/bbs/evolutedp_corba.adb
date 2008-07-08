------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                      E V O L U T E D P _ C O R B A                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2002 Free Software Foundation, Inc.             --
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
