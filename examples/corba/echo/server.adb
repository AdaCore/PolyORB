------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
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

with Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;

with PortableServer.POA.Helper;
with PortableServer.POAManager;

with Echo.Impl;

with PolyORB.CORBA_P.CORBALOC;

--  Setup server node: use no tasking default configuration

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

procedure Server is
begin

   declare
      Argv : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;

   begin
      CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);

      declare
         Root_POA : PortableServer.POA.Local_Ref;

         Ref : CORBA.Object.Ref;

         Obj : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      begin

         --  Retrieve Root POA

         Root_POA := PortableServer.POA.Helper.To_Local_Ref
           (CORBA.ORB.Resolve_Initial_References
            (CORBA.ORB.To_CORBA_String ("RootPOA")));

         PortableServer.POAManager.Activate
           (PortableServer.POA.Get_The_POAManager (Root_POA));

         --  Set up new object

         Ref := PortableServer.POA.Servant_To_Reference
           (Root_POA, PortableServer.Servant (Obj));

         --  Output IOR

         Put_Line
           ("'"
            & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref))
            & "'");
         New_Line;

         --  Output corbaloc

         Put_Line
           ("'"
            & CORBA.To_Standard_String
            (PolyORB.CORBA_P.CORBALOC.Object_To_Corbaloc (Ref))
            & "'");

         --  Launch the server. CORBA.ORB.Run is supposed to never return,
         --  print a message if it does.

         CORBA.ORB.Run;

         Put_Line ("ORB main loop terminated!");
      end;
   end;
exception
   when E : others =>
      Put_Line
        ("Echo server raised " & Ada.Exceptions.Exception_Information (E));
      raise;
end Server;
