------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                       T E S T 0 0 1 _ S E R V E R                        --
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

with Ada.Text_IO;

with CORBA.Object;
with CORBA.ORB;
with CORBA.Policy;
with PolyORB.Setup.Thread_Pool_Server;
pragma Warnings (Off, PolyORB.Setup.Thread_Pool_Server);
with PortableServer.POA.Helper;
with PortableServer.POAManager;
with PolyORB.Smart_Pointers;

with Test_Globals;
with Test_Interface.Helper;
with Test_Interface.Impl;
with Test_ServantActivator.Impl;

procedure Test001_Server is
begin
   CORBA.ORB.Initialize ("ORB");

   declare
      Root_POA : PortableServer.POA.Local_Ref;
      My_POA   : PortableServer.POA.Local_Ref;

      use CORBA.Policy.IDL_SEQUENCE_Policy;

      Implicit_Activation_Policy : constant CORBA.Policy.Ref
        := CORBA.Policy.Ref
        (PortableServer.POA.Create_Implicit_Activation_Policy
         (PortableServer.NO_IMPLICIT_ACTIVATION));

      Id_Assignment_Policy : constant CORBA.Policy.Ref
        := CORBA.Policy.Ref
        (PortableServer.POA.Create_Id_Assignment_Policy
         (PortableServer.USER_ID));

      Request_Processing_Policy : constant CORBA.Policy.Ref
        := CORBA.Policy.Ref
        (PortableServer.POA.Create_Request_Processing_Policy
         (PortableServer.USE_SERVANT_MANAGER));

      Policies : CORBA.Policy.PolicyList;

      Obj : constant Test_ServantActivator.Impl.Object_Ptr
        := new Test_ServantActivator.Impl.Object;
      Ref : Test_ServantActivator.Local_Ref;

   begin
      Root_POA :=
        PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));
      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (Root_POA));

      Append (Policies, Implicit_Activation_Policy);
      Append (Policies, Id_Assignment_Policy);
      Append (Policies, Request_Processing_Policy);
      My_POA :=
        PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("My_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));
      Test_ServantActivator.Set (Ref, PolyORB.Smart_Pointers.Entity_Ptr (Obj));
      PortableServer.POA.Set_Servant_Manager (My_POA, Ref);
      PortableServer.POAManager.Activate
        (PortableServer.POA.Get_The_POAManager (My_POA));

      declare
         Ptr : Test_Interface.Impl.Object_Ptr;
      begin
         Ptr := new Test_Interface.Impl.Object;
         Test_Interface.Impl.Init (Ptr, "Hello, world!");
         declare
            Id : constant PortableServer.ObjectId
              := PortableServer.POA.Activate_Object
              (Root_POA, PortableServer.Servant (Ptr));
            pragma Warnings (Off, Id);
         begin
            Test_Globals.Object_1 :=
              Test_Interface.Helper.To_Ref
              (PortableServer.POA.Servant_To_Reference
               (Root_POA, PortableServer.Servant (Ptr)));
         end;
      end;

      declare
         Ref : constant CORBA.Object.Ref
           := PortableServer.POA.Create_Reference_With_Id
           (My_POA,
            PortableServer.String_To_ObjectId ("dead"),
            CORBA.To_CORBA_String (Test_Interface.Repository_Id));
      begin
         Test_Globals.Object_2 := Test_Interface.Helper.To_Ref (Ref);

         Ada.Text_IO.Put_Line
           ("'" & CORBA.To_Standard_String (CORBA.ORB.Object_To_String (Ref))
            & "'");

         CORBA.ORB.Run;
      end;
   end;
end Test001_Server;
