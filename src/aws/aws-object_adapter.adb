with PolyORB.Errors;                use PolyORB.Errors;
with PolyORB.Log;
with PolyORB.POA;
with PolyORB.POA.Basic_POA;
with PolyORB.POA_Manager;               use PolyORB.POA_Manager;
with PolyORB.POA_Manager.Basic_Manager; use PolyORB.POA_Manager.Basic_Manager;
with PolyORB.POA_Policies;              use PolyORB.POA_Policies;
with PolyORB.POA_Policies.Request_Processing_Policy.Use_Default_Servant;
with PolyORB.POA_Policies.Id_Uniqueness_Policy.Multiple;
with PolyORB.POA_Policies.Lifespan_Policy.Persistent;
with PolyORB.POA_Policies.Implicit_Activation_Policy.No_Activation;
with PolyORB.POA_Policies.Servant_Retention_Policy.Non_Retain;
with PolyORB.Servants;

package body AWS.Object_Adapter is

   use PolyORB.Log;
   package L is
     new PolyORB.Log.Facility_Log ("aws.object_adapter");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug
   --  the polyorb logging facility

   ---------------------
   -- Unknown_Adapter --
   ---------------------

   procedure Unknown_Adapter
     (Self   : access AWS_AdapterActivator;
      Parent : access Obj_Adapter'Class;
      Name   : in     String;
      Result :    out Boolean;
      Error  : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (Self);
      pragma Warnings (On);

      The_Poa : PolyORB.POA.Obj_Adapter_Access;
      The_Servant : PolyORB.Servants.Servant_Access;
      Policies : PolicyList;
      The_Poa_Manager : constant Basic_POA_Manager_Access :=
        new Basic_POA_Manager;
   begin
      pragma Debug (O ("Unknown_Adapter: asked for <" & Name & ">"));

      PolyORB.POA_Policies.Policy_Lists.Append
        (Policies, PolyORB.POA_Policies.Request_Processing_Policy.
         Use_Default_Servant.Create.all'Access);

      --  This is what we need

      PolyORB.POA_Policies.Policy_Lists.Append
        (Policies, PolyORB.POA_Policies.Id_Uniqueness_Policy.
         Multiple.Create.all'Access);

      --  This is required by Use_Default_Servant

      PolyORB.POA_Policies.Policy_Lists.Append
        (Policies, PolyORB.POA_Policies.Lifespan_Policy.
         Persistent.Create.all'Access);

      --  To get rid of the ";pf=..." in URIs

      PolyORB.POA_Policies.Policy_Lists.Append
        (Policies, PolyORB.POA_Policies.Servant_Retention_Policy.
         Non_Retain.Create.all'Access);

      --  To get rid of the ";sys" in URIs

      PolyORB.POA_Policies.Policy_Lists.Append
        (Policies, PolyORB.POA_Policies.Implicit_Activation_Policy.
         No_Activation.Create.all'Access);

      --  Activation policy is incompatible with Non_Retain, so we
      --  use No_Activation.

      pragma Debug (O ("Unknown_Adapter: set POA policies"));

      Create (The_Poa_Manager);

      pragma Debug (O ("Unknown_Adapter: creating a new sub-POA"));
      PolyORB.POA.Basic_POA.Create_POA
        (PolyORB.POA.Basic_POA.Basic_Obj_Adapter
         (Parent.all)'Access,
         Name,
         POAManager_Access (The_Poa_Manager),
         Policies,
         The_Poa,
         Error);

      if Found (Error) then
         pragma Debug (O ("Error when creating the POA"));
         null;
      end if;

      The_Poa.Adapter_Activator := new Object_Adapter.AWS_AdapterActivator;

      PolyORB.POA_Manager.Basic_Manager.Activate
        (The_Poa_Manager,
         Error);

      if Found (Error) then
         pragma Debug (O ("AWS_Init: "
                          & "unable to activate the POA Manager",
                          Critical));
         null;
      end if;

      pragma Debug (O ("Unknown_Adapter: "
                       & "retrieving the servant from the parent POA"));
      PolyORB.POA.Basic_POA.Get_Servant
        (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (Parent.all)'Access,
         The_Servant,
         Error);

      if Found (Error) then
         pragma Debug (O ("Error when getting the servant"));
         null;
      end if;

      pragma Debug (O ("Unknown_Adapter: "
                       & "setting the servant for the new POA"));
      PolyORB.POA.Basic_POA.Set_Servant
        (PolyORB.POA.Basic_POA.Basic_Obj_Adapter (The_Poa.all)'Access,
         The_Servant,
         Error);

      if Found (Error) then
         pragma Debug (O ("Error when setting the servant"));
         null;
      end if;

      Result := True;

      --  We always return 'true', as it is up to the AWS servant to
      --  tell wether an object exists or not, whatever the path to it
      --  may be.
   end Unknown_Adapter;

end AWS.Object_Adapter;
