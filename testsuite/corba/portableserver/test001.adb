------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                              T E S T 0 0 1                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2012, Free Software Foundation, Inc.          --
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

with PolyORB.Setup.No_Tasking_Server;
pragma Warnings (Off, PolyORB.Setup.No_Tasking_Server);

with CORBA.Object;
with CORBA.ORB;
with CORBA.Impl;
with CORBA.Policy;

with PortableServer.POA.GOA;

with PolyORB.CORBA_P.Server_Tools;

with PolyORB.Utils.Report;

with Echo.Impl;

procedure Test001 is

   use PortableServer;
   use PortableServer.POA.GOA;

   use PolyORB.CORBA_P.Server_Tools;

   use PolyORB.Utils.Report;

begin

   New_Test ("GOA");

   CORBA.ORB.Initialize ("ORB");

   declare
      use CORBA.Impl;

      Ignored_Ref : CORBA.Object.Ref;
      pragma Warnings (Off, Ignored_Ref);
      --  WAGCC4.2: kill warning on use of Ignored_Ref

      pragma Unreferenced (Ignored_Ref);  --  Just passed to Initiate_Servant.
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
        := new Echo.Impl.Object;
      Obj2 : constant CORBA.Impl.Object_Ptr
        := new Echo.Impl.Object;
      Obj3 : constant CORBA.Impl.Object_Ptr
        := new Echo.Impl.Object;

      Oid1 : constant PortableServer.ObjectId
        := Servant_To_Id (GOA, PortableServer.Servant (Obj1));
      Oid2 : constant PortableServer.ObjectId
        := Servant_To_Id (GOA, PortableServer.Servant (Obj2));
      Oid3 : constant PortableServer.ObjectId
        := Servant_To_Id (GOA, PortableServer.Servant (Obj3));

   begin
      Initiate_Servant (PortableServer.Servant (Obj1), Ignored_Ref);
      Initiate_Servant (PortableServer.Servant (Obj2), Ignored_Ref);
      Initiate_Servant (PortableServer.Servant (Obj3), Ignored_Ref);

      CORBA.ORB.String_To_Object
        (CORBA.To_CORBA_String
         ("corbaloc:miop:1.0@1.0-TestDomain-5506/239.239.239.18:5678"),
         Group);

      Associate_Reference_With_Id (GOA, Group, Oid1);
      Associate_Reference_With_Id (GOA, Group, Oid2);
      Associate_Reference_With_Id (GOA, Group, Oid3);

      Output ("Added 3 servants to Group", True);

      Output ("Group'Length is correct",
              Length (Reference_To_Ids (GOA, Group)) = 3);

      Disassociate_Reference_With_Id (GOA, Group, Oid1);

      Output ("Removed 1 servant from Group", True);

      Disassociate_Reference_With_Id (GOA, Group, Oid2);
      Disassociate_Reference_With_Id (GOA, Group, Oid3);

      Output ("Group'Length is correct",
              Length (Reference_To_Ids (GOA, Group)) = 0);

      Associate_Reference_With_Id (GOA, Group, Oid1);
      Associate_Reference_With_Id (GOA, Group, Oid2);

      Output ("Added 2 servants to Group", True);

      Output ("Group'Length is correct",
              Length (Reference_To_Ids (GOA, Group)) = 2);

      declare
         Obj4 : constant CORBA.Impl.Object_Ptr
           := new Echo.Impl.Object;
         Oid : constant PortableServer.ObjectId
           := Create_Id_For_Reference (GOA, Group);
      begin
         Activate_Object_With_Id
           (GOA,
            Oid,
            PortableServer.Servant (Obj4));

         Output ("Activate one Object with Id", True);
      end;

      Output ("Group'Length is correct",
              Length (Reference_To_Ids (GOA, Group)) = 3);

      End_Report;
   end;
end Test001;
