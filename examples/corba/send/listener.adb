------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                            L I S T E N E R                               --
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

--  $Id$

with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;

with PolyORB.Setup.No_Tasking_Server;
pragma Elaborate_All (PolyORB.Setup.No_Tasking_Server);
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with CORBA;
with CORBA.Object;
with CORBA.ORB;
with CORBA.Impl;
with CORBA.Policy;
with PortableServer;
with PortableServer.POA;
with PortableServer.POA.GOA;
with PolyORB.CORBA_P.Server_Tools;
with PolyORB.Setup;

with Test.Printer.Impl;

with PolyORB.Objects;

procedure Listener is
   use CORBA.ORB;
   use PolyORB.CORBA_P.Server_Tools;
   use PortableServer.POA.GOA;

   procedure Print_List (List : IDs);
   pragma Warnings (Off, Print_List);

   procedure Print_List (List : IDs)
   is
      use Sequence_IDs;
      use PolyORB.Objects;

   begin
      Ada.Text_IO.Put_Line ("Group length :" & Length (List)'Img);
      for J in 1 .. Length (List) loop
         Ada.Text_IO.Put_Line
           (J'Img
            & " - "
            & Image (Object_Id (Element_Of (List, J).all)));
      end loop;
   end Print_List;

   Print_IOR : Boolean := False;
begin
   if Argument_Count >= 1 then
      if Argument (1) = "-v" then
         Print_IOR := True;
      end if;
      if Argument_Count > 1 then
         Ada.Text_IO.Put_Line ("usage : ./listner [-v]");
         return;
      end if;
   end if;
   CORBA.ORB.Initialize ("ORB");

   declare
      use CORBA.Impl;

      Ref1, Ref2, Ref3 : CORBA.Object.Ref;
      Group : CORBA.Object.Ref;

      Policies : CORBA.Policy.PolicyList;

      GOA : constant PortableServer.POA.GOA.Ref
        := PortableServer.POA.GOA.To_Ref
        (PortableServer.POA.Create_POA
         (Get_Root_POA,
          CORBA.To_CORBA_String ("RootGOA"),
          PortableServer.POA.Get_The_POAManager (Get_Root_POA),
          Policies));

      Obj1 : constant CORBA.Impl.Object_Ptr
        := new Test.Printer.Impl.Object;
      Obj2 : constant CORBA.Impl.Object_Ptr
        := new Test.Printer.Impl.Object;
      Obj3 : constant CORBA.Impl.Object_Ptr
        := new Test.Printer.Impl.Object;
      Oid1 : constant PortableServer.ObjectId
        := Servant_To_Id (GOA, PortableServer.Servant (Obj1));
      Oid2 : constant PortableServer.ObjectId
        := Servant_To_Id (GOA, PortableServer.Servant (Obj2));
      Oid3 : constant PortableServer.ObjectId
        := Servant_To_Id (GOA, PortableServer.Servant (Obj3));
   begin
      Initiate_Servant (PortableServer.Servant (Obj1), Ref1);
      Initiate_Servant (PortableServer.Servant (Obj2), Ref2);
      Initiate_Servant (PortableServer.Servant (Obj3), Ref3);

      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String
         ("corbaloc:miop:1.0@1.0-TestDomain-5506/239.239.239.18:5678"),
         Group);

      Associate_Reference_With_Id (GOA, Group, Oid1);
      Associate_Reference_With_Id (GOA, Group, Oid2);
      Associate_Reference_With_Id (GOA, Group, Oid3);

      Print_List (Reference_To_Ids (GOA, Group));

      Disassociate_Reference_With_Id (GOA, Group, Oid1);
      Disassociate_Reference_With_Id (GOA, Group, Oid2);
      Disassociate_Reference_With_Id (GOA, Group, Oid3);

      Print_List (Reference_To_Ids (GOA, Group));

      Associate_Reference_With_Id (GOA, Group, Oid1);
      Associate_Reference_With_Id (GOA, Group, Oid2);

      declare
         Obj4 : constant CORBA.Impl.Object_Ptr
           := new Test.Printer.Impl.Object;
         Oid : constant PortableServer.ObjectId
           := Create_Id_For_Reference (GOA, Group);
      begin
         Activate_Object_With_Id
           (GOA,
            Oid,
            PortableServer.Servant (Obj4));
      end;

      Print_List (Reference_To_Ids (GOA, Group));

      Ada.Text_IO.Put_Line
        ("Group IOR: '"
         & CORBA.To_Standard_String (Object_To_String (Group))
         & "'");

      Ada.Text_IO.Put_Line
        ("Group IOR: '"
         & CORBA.To_Standard_String (Object_To_Corbaloc (Group))
         & "'");

      if Print_IOR then
         Ada.Text_IO.Put_Line
           ("Object IOR: '"
            & CORBA.To_Standard_String (Object_To_String (Group))
            & "'");

         Ada.Text_IO.Put_Line
           ("Object IOR: '"
            & CORBA.To_Standard_String (Object_To_Corbaloc (Ref1))
            & "'");
      end if;
      --  Launch the server
      Initiate_Server;
   end;
end Listener;
