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
