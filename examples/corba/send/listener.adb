------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             L I S T E N E R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2012, Free Software Foundation, Inc.          --
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

with Ada.Text_IO;

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);
--  Note: this test relies on the fact that the server is mono
--  tasking, see Test.Printer.Impl for more details.

with CORBA.Object;
with CORBA.ORB;
with CORBA.Impl;
with CORBA.Policy;

with PortableServer.POA.GOA;

with PolyORB.CORBA_P.CORBALOC;
with PolyORB.CORBA_P.Server_Tools;

with Test.Controller.Impl;
with Test.Printer.Impl;
with Test.Printer.Helper;

procedure Listener is

   use CORBA.ORB;
   use PortableServer;
   use PortableServer.POA.GOA;

   use PolyORB.CORBA_P.Server_Tools;

   procedure Print_List (List : IDs);
   --  Output each elements of List

   ----------------
   -- Print_List --
   ----------------

   procedure Print_List (List : IDs) is
      use Sequence_IDs;

   begin
      Ada.Text_IO.Put_Line ("Group length :" & Integer'Image (Length (List)));
      Ada.Text_IO.Put_Line ("Objects in group :");

      for J in 1 .. Length (List) loop
         Ada.Text_IO.Put_Line
           (Integer'Image (J)
            & " - "
            & PortableServer.ObjectId_To_String (Get_Element (List, J)));
      end loop;
      Ada.Text_IO.New_Line;

   end Print_List;

   Group_Id : constant Standard.String
     := "corbaloc:miop:1.0@1.0-TestDomain-5506/239.239.239.18:5678";

begin
   CORBA.ORB.Initialize ("ORB");

   declare
      use CORBA.Impl;

      Ref1, Ref2, Ref3, Ref4 : CORBA.Object.Ref;
      Group : CORBA.Object.Ref;

      Policies : CORBA.Policy.PolicyList;

      GOA : constant PortableServer.POA.GOA.Ref
        := PortableServer.POA.GOA.To_Ref
        (PortableServer.POA.Create_POA
         (Get_Root_POA,
          CORBA.To_CORBA_String ("RootGOA"),
          PortableServer.POA.Get_The_POAManager (Get_Root_POA),
          Policies));

      Obj1 : constant CORBA.Impl.Object_Ptr := new Test.Printer.Impl.Object;
      Obj2 : constant CORBA.Impl.Object_Ptr := new Test.Printer.Impl.Object;
      Obj3 : constant CORBA.Impl.Object_Ptr := new Test.Printer.Impl.Object;

      Oid1 : constant PortableServer.ObjectId
        := Servant_To_Id (GOA, PortableServer.Servant (Obj1));
      Oid2 : constant PortableServer.ObjectId
        := Servant_To_Id (GOA, PortableServer.Servant (Obj2));
      Oid3 : constant PortableServer.ObjectId
        := Servant_To_Id (GOA, PortableServer.Servant (Obj3));

      Controller_Obj : constant CORBA.Impl.Object_Ptr
        := new Test.Controller.Impl.Object;

   begin
      Initiate_Servant (PortableServer.Servant (Obj1), Ref1);
      Initiate_Servant (PortableServer.Servant (Obj2), Ref2);
      Initiate_Servant (PortableServer.Servant (Obj3), Ref3);
      Initiate_Servant (PortableServer.Servant (Controller_Obj), Ref4);

      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String (Group_Id),
         Group);

      Associate_Reference_With_Id (GOA, Group, Oid1);
      Associate_Reference_With_Id (GOA, Group, Oid2);
      Associate_Reference_With_Id (GOA, Group, Oid3);

      Print_List (Reference_To_Ids (GOA, Group));

      Ada.Text_IO.Put_Line
        ("IOR of the test controller '"
           & CORBA.To_Standard_String (Object_To_String (Ref4))
           & "'");
      Ada.Text_IO.New_Line;

      Test.Controller.Impl.Set_Printer
        (Test.Controller.Impl.Object (Controller_Obj.all)'Access,
         Test.Printer.Helper.Unchecked_To_Ref (Group));

      Test.Controller.Impl.Set_Group_Size
        (Test.Controller.Impl.Object (Controller_Obj.all)'Access,
         Length (Reference_To_Ids (GOA, Group)));

      Ada.Text_IO.Put_Line
        ("Group IOR: '"
         & CORBA.To_Standard_String (Object_To_String (Group))
         & "'");
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line
        ("Group corbaloc: '"
         & CORBA.To_Standard_String
         (PolyORB.CORBA_P.CORBALOC.Object_To_Corbaloc (Group))
         & "'");
      Ada.Text_IO.New_Line;

      --  Launch the server

      Initiate_Server;
   end;
end Listener;
