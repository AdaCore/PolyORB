------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                               S E R V E R                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2002-2007, Free Software Foundation, Inc.          --
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
with Ada.Text_IO;

with CORBA.Impl;
with CORBA.Object;
with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA.Helper;
with PortableServer.POAManager;

with Echo.Impl;

--  Setup server node: use no tasking default configuration

with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);

with PolyORB.Setup.Secure_Client;
pragma Warnings (Off, PolyORB.Setup.Secure_Client);
with PolyORB.Setup.Secure_Server;
pragma Warnings (Off, PolyORB.Setup.Secure_Server);

procedure Server is
begin

   declare
      Argv : CORBA.ORB.Arg_List := CORBA.ORB.Command_Line_Arguments;

   begin
      CORBA.ORB.Init (CORBA.ORB.To_CORBA_String ("ORB"), Argv);

      declare
         Root_POA : PortableServer.POA.Local_Ref;
         My_POA   : PortableServer.POA.Local_Ref;

         Ref : CORBA.Object.Ref;

         Obj : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      begin

         --  Retrieve Root POA

         Root_POA := PortableServer.POA.Helper.To_Ref
           (CORBA.ORB.Resolve_Initial_References
            (CORBA.ORB.To_CORBA_String ("RootPOA")));

         PortableServer.POAManager.Activate
           (PortableServer.POA.Get_The_POAManager (Root_POA));

         --  Create My POA

         declare
            Policies : CORBA.Policy.PolicyList;

         begin
            My_POA :=
               PortableServer.POA.Local_Ref
               (PortableServer.POA.Create_POA
                (Root_POA,
                 CORBA.To_CORBA_String ("My_POA"),
                 PortableServer.POA.Get_The_POAManager (Root_POA),
                 Policies));
         end;

         --  Set up new object

         Ref := PortableServer.POA.Servant_To_Reference
           (My_POA, PortableServer.Servant (Obj));

         --  Output IOR

         Ada.Text_IO.Put_Line
           ("'"
            & CORBA.To_Standard_String (CORBA.Object.Object_To_String (Ref))
            & "'");
         Ada.Text_IO.New_Line;

         --  Launch the server

         CORBA.ORB.Run;
      end;
   end;

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
      CORBA.ORB.Shutdown (False);

      raise;
end Server;
