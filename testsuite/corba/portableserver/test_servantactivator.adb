------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                T E S T _ S E R V A N T A C T I V A T O R                 --
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

with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA.Helper;

with PolyORB.Smart_Pointers;
with PolyORB.Utils.Report;

with Echo.Helper;
with Test_NullActivator.Impl;
with Test_SimpleActivator.Impl;

package body Test_ServantActivator is

   use CORBA;
   use PolyORB.Utils.Report;

   -------------------------------
   -- Run_Test_ServantActivator --
   -------------------------------

   procedure Run_Test_ServantActivator is
      use CORBA.Policy.IDL_SEQUENCE_Policy;
      use PortableServer.POA;

      Null_Activator_Obj : constant Test_NullActivator.Impl.Object_Ptr
        := new Test_NullActivator.Impl.Object;
      Null_Activator : Test_NullActivator.Local_Ref;

      Simple_Activator_Obj : constant Test_SimpleActivator.Impl.Object_Ptr
        := new Test_SimpleActivator.Impl.Object;
      Simple_Activator : Test_SimpleActivator.Local_Ref;

      Root_POA : constant PortableServer.POA.Local_Ref :=
        PortableServer.POA.Helper.To_Local_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Policies : CORBA.Policy.PolicyList;

      Implicit_Activation_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Implicit_Activation_Policy
         (PortableServer.NO_IMPLICIT_ACTIVATION));

      Id_Assignment_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Id_Assignment_Policy (PortableServer.USER_ID));

      Request_Processing_Policy : constant CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Request_Processing_Policy
         (PortableServer.USE_SERVANT_MANAGER));

      Child_POA : PortableServer.POA.Local_Ref;
      Child_POA2 : PortableServer.POA.Local_Ref;

   begin
      New_Test ("Servant Activator");

      --  Create POA policies list

      Append (Policies, Implicit_Activation_Policy);
      Append (Policies, Id_Assignment_Policy);
      Append (Policies, Request_Processing_Policy);

      --  Register a Child POA

      Child_POA := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("Child_POA"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Output ("Created child POA", True);

      --  Look for a non existent servant whitout ServantActivator;

      declare
         Obj_Ref : constant Echo.Ref :=
           Echo.Helper.To_Ref
           (Create_Reference_With_Id
            (Child_POA,
             PortableServer.String_To_ObjectId ("dead"),
             To_CORBA_String (Echo.Repository_Id)));

         Result : CORBA.String;
         pragma Unreferenced (Result);
         --  To kill "variable "Result" is assigned but never read" warning
      begin
         pragma Warnings (Off); --  WAG:GCC3.4.3
         Result := Echo.echoString
           (Obj_Ref,
            To_CORBA_String ("Hello Ada World !"));
         pragma Warnings (On); --  WAG:GCC3.4.3
         --  XXX This is to kill warning "pragma Unreferenced given
         --  for "Result""

         Output ("Non existant object found !", False);
      exception
         when CORBA.Object_Not_Exist =>
            Output ("Non existant object not found", True);
      end;

      --  Set Null Servant Activator

      Test_NullActivator.Set
       (Null_Activator,
        PolyORB.Smart_Pointers.Entity_Ptr (Null_Activator_Obj));
      PortableServer.POA.Set_Servant_Manager (Child_POA, Null_Activator);

      --  Test Null Servant Activator Incarnate primitive is called

      declare
         Obj_Ref : constant Echo.Ref :=
           Echo.Helper.To_Ref
           (Create_Reference_With_Id
            (Child_POA,
             PortableServer.String_To_ObjectId ("dead"),
             To_CORBA_String (Echo.Repository_Id)));

         Result : CORBA.String;
         pragma Unreferenced (Result);
         --  To kill "variable "Result" is assigned but never read" warning
      begin
         pragma Warnings (Off); --  WAG:GCC3.4.3
         Result := Echo.echoString
           (Obj_Ref,
            To_CORBA_String ("Hello Ada World !"));
         pragma Warnings (On); --  WAG:GCC3.4.3
         --  XXX This is to kill warning "pragma Unreferenced given
         --  for "Result""

      exception
         when CORBA.Object_Not_Exist =>
            Output ("Null Activator called", Null_Activator_Incarnate_Called);
      end;

      --  Register a second Child POA

      Child_POA2 := PortableServer.POA.Local_Ref
        (PortableServer.POA.Create_POA
         (Root_POA,
          CORBA.To_CORBA_String ("Child_POA2"),
          PortableServer.POA.Get_The_POAManager (Root_POA),
          Policies));

      Output ("Created child POA", True);

      --  Look for a non existent servant whitout ServantActivator;

      declare
         Obj_Ref : constant Echo.Ref :=
           Echo.Helper.To_Ref
           (Create_Reference_With_Id
            (Child_POA2,
             PortableServer.String_To_ObjectId ("dead"),
             To_CORBA_String (Echo.Repository_Id)));

         Result : CORBA.String;
         pragma Unreferenced (Result);
         --  To kill "variable "Result" is assigned but never read" warning
      begin
         pragma Warnings (Off); --  WAG:GCC3.4.3
         Result := Echo.echoString
           (Obj_Ref,
            To_CORBA_String ("Hello Ada World !"));
         pragma Warnings (On); --  WAG:GCC3.4.3
         --  XXX This is to kill warning "pragma Unreferenced given
         --  for "Result""

         Output ("Non existant object found !", False);
      exception
         when CORBA.Object_Not_Exist =>
            Output ("Non existant object not found", True);
      end;

      --  Set Simple Servant Activator

      Test_SimpleActivator.Set
       (Simple_Activator,
        PolyORB.Smart_Pointers.Entity_Ptr (Simple_Activator_Obj));
      PortableServer.POA.Set_Servant_Manager (Child_POA2, Simple_Activator);

      --  Test Simple Servant Activator Incarnate primitive is called

      declare
         Obj_Ref : constant Echo.Ref :=
           Echo.Helper.To_Ref
           (Create_Reference_With_Id
            (Child_POA2,
             PortableServer.String_To_ObjectId ("dead"),
             To_CORBA_String (Echo.Repository_Id)));

         Result : CORBA.String;
         pragma Unreferenced (Result);
         --  To kill "variable "Result" is assigned but never read" warning
      begin
         pragma Warnings (Off); --  WAG:GCC3.4.3
         Result := Echo.echoString
           (Obj_Ref,
            To_CORBA_String ("Hello Ada World !"));
         pragma Warnings (On); --  WAG:GCC3.4.3
         --  XXX This is to kill warning "pragma Unreferenced given
         --  for "Result""

         Output ("Simple Activator called",
                 Simple_Activator_Incarnate_Called);

         Deactivate_Object (Child_POA2,
                            Reference_To_Id (Child_POA2, Obj_Ref));
         Output ("Etherealize called", Simple_Activator_Etherealize_Called);
      exception
         when CORBA.Object_Not_Exist =>
            Output ("No servant created !", False);
      end;
   end Run_Test_ServantActivator;

end Test_ServantActivator;
