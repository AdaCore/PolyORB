------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                T E S T _ S E R V A N T A C T I V A T O R                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2003 Free Software Foundation, Inc.             --
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

with CORBA.Impl;
with CORBA.ORB;
with CORBA.Policy;

with PortableServer.POA;

with PolyORB.Utils.Report;

with Echo.Helper;
with Echo.Impl;

package body Test_ServantActivator is

   use CORBA;
   use PolyORB.Utils.Report;

   ---------------
   -- Incarnate --
   ---------------

   Null_Activator_Incarnate_Called : Boolean := False;

   function Incarnate
     (Self    : in Null_Activator_Ref;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref)
     return PortableServer.Servant
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self, Oid, Adapter);
      pragma Warninfs (On); --  WAG:3.15

   begin
      Null_Activator_Incarnate_Called := True;

      return null;
   end Incarnate;

   Simple_Activator_Incarnate_Called : Boolean := False;

   function Incarnate
     (Self    : in Simple_Activator_Ref;
      Oid     : in PortableServer.ObjectId;
      Adapter : in PortableServer.POA_Forward.Ref)
     return PortableServer.Servant
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self);
      pragma Warninfs (On); --  WAG:3.15

      Obj : constant CORBA.Impl.Object_Ptr := new Echo.Impl.Object;

      package Convert is new
        PortableServer.POA_Forward.Convert (PortableServer.POA.Ref);

      POA : constant PortableServer.POA.Ref := Convert.To_Ref (Adapter);

   begin
      Simple_Activator_Incarnate_Called := True;

      PortableServer.POA.Activate_Object_With_Id
        (POA,
         Oid,
         PortableServer.Servant (Obj));

      return PortableServer.Servant (Obj);
   end Incarnate;

   -----------------
   -- Etherealize --
   -----------------

   procedure Etherealize
     (Self                  : in Null_Activator_Ref;
      Oid                   : in PortableServer.ObjectId;
      Adapter               : in PortableServer.POA_Forward.Ref;
      Serv                  : in PortableServer.Servant;
      Cleanup_In_Progress   : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self, Oid, Adapter, Serv);
      pragma Unreferenced (Cleanup_In_Progress, Remaining_Activations);
      pragma Warninfs (On); --  WAG:3.15

   begin
      null;
   end Etherealize;

   Simple_Activator_Etherealize_Called : Boolean := False;

   procedure Etherealize
     (Self                  : in Simple_Activator_Ref;
      Oid                   : in PortableServer.ObjectId;
      Adapter               : in PortableServer.POA_Forward.Ref;
      Serv                  : in PortableServer.Servant;
      Cleanup_In_Progress   : in CORBA.Boolean;
      Remaining_Activations : in CORBA.Boolean)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Self, Oid, Adapter, Serv);
      pragma Unreferenced (Cleanup_In_Progress, Remaining_Activations);
      pragma Warninfs (On); --  WAG:3.15

   begin
      Simple_Activator_Etherealize_Called := True;
   end Etherealize;

   -------------------------------
   -- Run_Test_ServantActivator --
   -------------------------------

   procedure Run_Test_ServantActivator
   is
      use CORBA.Policy.IDL_Sequence_Policy;
      use PortableServer.POA;

      Null_Activator : constant Null_Activator_Access :=
        new Null_Activator_Ref;

      Simple_Activator : constant Simple_Activator_Access :=
        new Simple_Activator_Ref;

      Root_POA : constant PortableServer.POA.Ref :=
        PortableServer.POA.To_Ref
        (CORBA.ORB.Resolve_Initial_References
         (CORBA.ORB.To_CORBA_String ("RootPOA")));

      Policies : CORBA.Policy.PolicyList;

      Implicit_Activation_Policy : CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Implicit_Activation_Policy
         (PortableServer.NO_IMPLICIT_ACTIVATION));

      Id_Assignment_Policy : CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Id_Assignment_Policy (PortableServer.USER_ID));

      Request_Processing_Policy : CORBA.Policy.Ref :=
        CORBA.Policy.Ref
        (Create_Request_Processing_Policy
         (PortableServer.USE_SERVANT_MANAGER));

      Child_POA : PortableServer.POA.Ref;
      Child_POA2 : PortableServer.POA.Ref;

   begin
      New_Test ("Servant Activator");

      --  Create POA policies list

      Append (Policies, Implicit_Activation_Policy);
      Append (Policies, Id_Assignment_Policy);
      Append (Policies, Request_Processing_Policy);

      --  Register a Child POA

      Child_POA := PortableServer.POA.Ref
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
      begin
         Result := Echo.echoString
           (Obj_Ref,
            To_CORBA_String ("Hello Ada World !"));

         Output ("Non existant object found !", False);
      exception
         when CORBA.Object_Not_Exist =>
            Output ("Non existant object not found", True);
      end;

      --  Set Null Servant Activator

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
      begin
         Result := Echo.echoString
           (Obj_Ref,
            To_CORBA_String ("Hello Ada World !"));

      exception
         when CORBA.Object_Not_Exist =>
            Output ("Null Activator called", Null_Activator_Incarnate_Called);
      end;

      --  Register a second Child POA

      Child_POA2 := PortableServer.POA.Ref
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
      begin
         Result := Echo.echoString
           (Obj_Ref,
            To_CORBA_String ("Hello Ada World !"));

         Output ("Non existant object found !", False);
      exception
         when CORBA.Object_Not_Exist =>
            Output ("Non existant object not found", True);
      end;

      --  Set Simple Servant Activator

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
      begin
         Result := Echo.echoString
           (Obj_Ref,
            To_CORBA_String ("Hello Ada World !"));

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
