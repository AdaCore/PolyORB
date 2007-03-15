------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                             L I S T E N E R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2003-2006, Free Software Foundation, Inc.          --
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

with Test.Printer.Impl;

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

   procedure Print_List (List : IDs)
   is
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
        (CORBA.To_CORBA_String (Group_Id),
         Group);

      Associate_Reference_With_Id (GOA, Group, Oid1);
      Associate_Reference_With_Id (GOA, Group, Oid2);
      Associate_Reference_With_Id (GOA, Group, Oid3);

      Print_List (Reference_To_Ids (GOA, Group));

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

      Ada.Text_IO.Put_Line
        ("IOR of one object in group: '"
         & CORBA.To_Standard_String (Object_To_String (Ref1))
         & "'");
      Ada.Text_IO.New_Line;

      Ada.Text_IO.Put_Line
        ("corbaloc of one object in group: '"
         & CORBA.To_Standard_String
         (PolyORB.CORBA_P.CORBALOC.Object_To_Corbaloc (Ref1))
         & "'");
      Ada.Text_IO.New_Line;

      --  Launch the server

      Initiate_Server;
   end;
end Listener;
