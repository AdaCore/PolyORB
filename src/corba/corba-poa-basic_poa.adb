with Ada.Real_Time;

with Droopi.Objects;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);
with Droopi.CORBA_P.Exceptions; use Droopi.CORBA_P.Exceptions;

with CORBA.Policy_Types;
with CORBA.Policy_Values;
with CORBA.POA_Types;
with CORBA.POA_Manager.Basic_Manager;

with POA_Configuration.Minimum;
with POA_Configuration;

package body CORBA.POA.Basic_POA is

   use POA_Types;
   use Droopi.Log;
   use Droopi.Locks;
   use CORBA.Policy;
   use CORBA.Policy_Types;
   use POA_Configuration;
   use POA_Configuration.Minimum;
   use CORBA.POA_Manager;
   use CORBA.POA_Manager.Basic_Manager;

   package L is new Droopi.Log.Facility_Log ("corba.poa.basic_poa");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   ----------------------------------------------------------
   --  Declaration of additional procedures and functions  --
   ----------------------------------------------------------

   function Get_Boot_Time return Time_Stamp;

   function Get_Child (Adapter : access Basic_Obj_Adapter;
                       Name    : in     String)
                      return POA_Types.Obj_Adapter_Access;
   --  Look in the list of children of the Adapter if an OA with
   --  the given name already exists.
   --  The function doesn't take care of locking the list of children!

   procedure Init_With_User_Policies (OA       : Basic_Obj_Adapter_Access;
                                      Policies : Policy.PolicyList_Access);

   procedure Init_With_Default_Policies (OA : Basic_Obj_Adapter_Access);

   procedure Check_Policies_Compatibility (OA : Basic_Obj_Adapter_Access);

   function Register_Child (Self  : access Basic_Obj_Adapter;
                            Child :        Basic_Obj_Adapter_Access)
                           return Positive;
   --  Add a child to the current POA
   --  The procedure doesn't take care of locking the list of children!

   procedure Destroy_Policies (OA : in out Basic_Obj_Adapter);
   --  Destroys OA's policies

   procedure Destroy_Locks (OA : in out Basic_Obj_Adapter);
   --  Destroys OA's locks

   procedure Destroy_OA (OA : access Basic_Obj_Adapter);
   --  Destroy OA's components, and frees OA

   function Find_Servant
     (OA       : access Basic_Obj_Adapter;
      Id       :        Droopi.Objects.Object_Id;
      Do_Check :        Check_State)
     return Droopi.Objects.Servant_Access;
   --  The Find_Servant from Droopi, plus a parameter.
   --  If check is NO_CHECK, the POA doesn't check its state.

   ------------------------------------
   --  Code of additional functions  --
   ------------------------------------

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (Adapter : access Basic_Obj_Adapter;
      Name    : in     String)
     return POA_Types.Obj_Adapter_Access
   is
      use POA_Sequences;
      A_Child : POA_Types.Obj_Adapter_Access;
   begin
      pragma Debug (O ("Get_Child : " & To_Standard_String (Name)));
      if Adapter.Children /= null then
         for I in 1 .. Length (Adapter.Children.all) loop
            A_Child := Element_Of (Adapter.Children.all, I);
            if CORBA.POA.Obj_Adapter_Access (A_Child).Name = Name then
               return A_Child;
            end if;
         end loop;
      end if;
      return null;
   end Get_Child;

   -------------------
   -- Get_Boot_Time --
   -------------------

   function Get_Boot_Time
     return Time_Stamp
   is
      use Ada.Real_Time;
      T  : Time;
      SC : Seconds_Count;
      TS : Time_Span;
   begin
      T := Clock;
      Split (T, SC, TS);

      return Time_Stamp (Unsigned_Long (SC));
   end Get_Boot_Time;

   -----------------------------
   -- Init_With_User_Policies --
   -----------------------------

   procedure Init_With_User_Policies
     (OA       : Basic_Obj_Adapter_Access;
      Policies : Policy.PolicyList_Access)
   is
      A_Policy : Policy_Access;
   begin
      pragma Debug (O ("Init Basic_POA with user provided policies"));
      for I in 1 .. Policy_Sequences.Length (Policies.all) loop
         A_Policy := Policy_Sequences.Element_Of (Policies.all, I);
         case A_Policy.Policy_Type is
            when THREAD_POLICY_ID =>
               if OA.Thread_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in ThreadPolicy: using last one"));
                  null;
               end if;
               OA.Thread_Policy
                 := Create_Thread_Policy (OA,  A_Policy.Value);

            when LIFESPAN_POLICY_ID =>
               if OA.Lifespan_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in LifespanPolicy: using last one"));
                  null;
               end if;
               OA.Lifespan_Policy
                 := Create_Lifespan_Policy (OA,  A_Policy.Value);

            when ID_UNIQUENESS_POLICY_ID =>
               if OA.Id_Uniqueness_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in IdUniquenessPolicy: using last one"));
                  null;
               end if;
               OA.Id_Uniqueness_Policy
                 := Create_Id_Uniqueness_Policy (OA,  A_Policy.Value);

            when ID_ASSIGNEMENT_POLICY_ID =>
               if OA.Id_Assignement_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in IdAssignementPolicy: using last one"));
                  null;
               end if;
               OA.Id_Assignement_Policy
                 := Create_Id_Assignement_Policy (OA,  A_Policy.Value);

            when SERVANT_RETENTION_POLICY_ID =>
               if OA.Servant_Retention_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in ServantRetentionPolicy:"
                        & " using last one"));
                  null;
               end if;
               OA.Servant_Retention_Policy
                 := Create_Servant_Retention_Policy (OA,  A_Policy.Value);

            when REQUEST_PROCESSING_POLICY_ID =>
               if OA.Request_Processing_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in RequestProcessingPolicy:"
                        & " using last one"));
                  null;
               end if;
               OA.Request_Processing_Policy
                 := Create_Request_Processing_Policy (OA,  A_Policy.Value);

            when IMPLICIT_ACTIVATION_POLICY_ID =>
               if OA.Implicit_Activation_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in ImplicitActivationPolicy:"
                        & "using last one"));
                  null;
               end if;
               OA.Implicit_Activation_Policy
                 := Create_Implicit_Activation_Policy (OA,  A_Policy.Value);

            when others =>
               null;
               pragma Debug (O ("Unknown policy ignored"));
         end case;
      end loop;
   end Init_With_User_Policies;

   --------------------------------
   -- Init_With_Default_Policies --
   --------------------------------

   procedure Init_With_Default_Policies
     (OA : Basic_Obj_Adapter_Access)
   is
      use CORBA.Policy_Values;
   begin
      pragma Debug (O ("Init Basic_POA with default policies"));
      if OA.Thread_Policy = null then
         OA.Thread_Policy := Create_Thread_Policy (OA, ORB_CTRL_MODEL);
      end if;

      if OA.Lifespan_Policy = null then
         OA.Lifespan_Policy
           := Create_Lifespan_Policy (OA, CORBA.Policy_Values.TRANSIENT);
      end if;

      if OA.Id_Uniqueness_Policy = null then
         OA.Id_Uniqueness_Policy :=
           Create_Id_Uniqueness_Policy (OA, UNIQUE_ID);
      end if;

      if OA.Id_Assignement_Policy = null then
         OA.Id_Assignement_Policy :=
           Create_Id_Assignement_Policy (OA, SYSTEM_ID);
      end if;

      if OA.Servant_Retention_Policy = null then
         OA.Servant_Retention_Policy :=
           Create_Servant_Retention_Policy (OA, RETAIN);
      end if;

      if OA.Request_Processing_Policy = null then
         OA.Request_Processing_Policy :=
           Create_Request_Processing_Policy (OA,
                                             USE_ACTIVE_OBJECT_MAP_ONLY);
      end if;

      if OA.Implicit_Activation_Policy = null then
         OA.Implicit_Activation_Policy :=
           Create_Implicit_Activation_Policy (OA,
                                              NO_IMPLICIT_ACTIVATION);
      end if;
   end Init_With_Default_Policies;

   ----------------------------------
   -- Check_Policies_Compatibility --
   ----------------------------------

   procedure Check_Policies_Compatibility
     (OA : Basic_Obj_Adapter_Access)
   is
   begin
      pragma Debug (O ("Check compatibilities between policies"));
      Check_Compatibility
        (OA.Thread_Policy.all,
         CORBA.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Lifespan_Policy.all,
         CORBA.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Id_Uniqueness_Policy.all,
         CORBA.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Id_Assignement_Policy.all,
         CORBA.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Servant_Retention_Policy.all,
         CORBA.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Request_Processing_Policy.all,
         CORBA.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Implicit_Activation_Policy.all,
         CORBA.POA_Types.Obj_Adapter_Access (OA));
   end Check_Policies_Compatibility;

   --------------------
   -- Register_Child --
   --------------------

   function Register_Child
     (Self  : access Basic_Obj_Adapter;
      Child :        Basic_Obj_Adapter_Access)
     return Positive
   is
      use CORBA.POA_Types.POA_Sequences;
   begin
      pragma Debug (O (To_Standard_String (Self.Name)
                       & "registers child "
                       & To_Standard_String (Child.Name)));
      if (Self.Children = null) then
         Self.Children := new POAList;
      end if;
      for I in 1 .. Length (Sequence (Self.Children.all)) loop
         if Element_Of (Sequence (Self.Children.all), I) = null then
            Replace_Element (Sequence (Self.Children.all),
                             I,
                             CORBA.POA_Types.Obj_Adapter_Access (Child));
            return I;
         end if;
      end loop;
      Append (Sequence (Self.Children.all),
              CORBA.POA_Types.Obj_Adapter_Access (Child));
      return Length (Sequence (Self.Children.all));
   end Register_Child;

   ----------------------
   -- Destroy_Policies --
   ----------------------

   procedure Destroy_Policies
     (OA : in out Basic_Obj_Adapter)
   is
   begin
      if OA.Thread_Policy /= null then
         Free (OA.Thread_Policy.all,
               Policy_Access (OA.Thread_Policy));
      end if;
      if OA.Id_Uniqueness_Policy /= null then
         Free (OA.Id_Uniqueness_Policy.all,
               Policy_Access (OA.Id_Uniqueness_Policy));
      end if;
      if OA.Id_Assignement_Policy /= null then
         Free (OA.Id_Assignement_Policy.all,
               Policy_Access (OA.Id_Assignement_Policy));
      end if;
      if OA.Implicit_Activation_Policy /= null then
         Free (OA.Implicit_Activation_Policy.all,
               Policy_Access (OA.Implicit_Activation_Policy));
      end if;
      if OA.Lifespan_Policy /= null then
         Free (OA.Lifespan_Policy.all,
               Policy_Access (OA.Lifespan_Policy));
      end if;
      if OA.Request_Processing_Policy /= null then
         Free (OA.Request_Processing_Policy.all,
               Policy_Access (OA.Request_Processing_Policy));
      end if;
      if OA.Servant_Retention_Policy /= null then
         Free (OA.Servant_Retention_Policy.all,
               Policy_Access (OA.Servant_Retention_Policy));
      end if;
   end Destroy_Policies;

   -------------------
   -- Destroy_Locks --
   -------------------

   procedure Destroy_Locks
     (OA : in out Basic_Obj_Adapter)
   is
      use Droopi.Locks;
   begin
      if OA.Children_Lock /= null then
         Destroy (OA.Children_Lock);
      end if;
      if OA.Map_Lock /= null then
         Destroy (OA.Map_Lock);
      end if;
   end Destroy_Locks;

   ----------------
   -- Destroy_OA --
   ----------------

   procedure Destroy_OA
     (OA : access Basic_Obj_Adapter)
   is
   begin
      if OA.POA_Manager /= null then
         Remove_POA (OA.POA_Manager,
                     CORBA.POA_Types.Obj_Adapter_Access (OA));
      end if;
      Destroy_Policies (OA.all);
      Destroy_Locks    (OA.all);
      declare
         OA_Access : Basic_Obj_Adapter_Access
           := Basic_Obj_Adapter_Access (OA);
      begin
         Free (OA_Access);
      end;
   end Destroy_OA;

   ---------------------
   -- Create_Root_POA --
   ---------------------

   function Create_Root_POA
     return Obj_Adapter_Access
   is
      New_Obj_Adapter : Basic_Obj_Adapter_Access;
      Conf            : POA_Configuration.Minimum.Minimum_Configuration;
   begin
      pragma Debug (O ("Create a new Root_POA"));

      --  Create new Obj Adapter
      New_Obj_Adapter                  := new Basic_Obj_Adapter;
      New_Obj_Adapter.Boot_Time        := Get_Boot_Time;
      New_Obj_Adapter.Name             := To_CORBA_String ("RootPOA");
      New_Obj_Adapter.Absolute_Address := To_CORBA_String ("");
      Create (New_Obj_Adapter.Children_Lock);
      Create (New_Obj_Adapter.Map_Lock);

      New_Obj_Adapter.POA_Manager      := new Basic_POA_Manager;
      Create (New_Obj_Adapter.POA_Manager);
      Register_POA (New_Obj_Adapter.POA_Manager,
                    CORBA.POA_Types.Obj_Adapter_Access (New_Obj_Adapter));

      --  Create and initialize policies factory
      New_Obj_Adapter.P_Factory
        := CORBA.Policy.Policies_Factory_Pkg.New_Factory;
      Initialize (Conf, New_Obj_Adapter.P_Factory);

      --  Use default policies
      Init_With_Default_Policies (New_Obj_Adapter);

      return Obj_Adapter_Access (New_Obj_Adapter);
   end Create_Root_POA;

   -------------------------------------------------
   -- Procedures and functions required by Corba  --
   -------------------------------------------------

   ----------------
   -- Create_POA --
   ----------------

   function Create_POA
     (Self         : access Basic_Obj_Adapter;
      Adapter_Name :        String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        Policy.PolicyList_Access)
     return Obj_Adapter_Access
   is
      New_Obj_Adapter : Basic_Obj_Adapter_Access;
      Children_Locked : Boolean := False;
      Conf            : POA_Configuration.Minimum.Minimum_Configuration;
      Index           : Positive;
   begin
      pragma Debug (O ("Create a new POA with name "
                      & To_Standard_String (Adapter_Name)));

      --  Adapter_Name should be not empty
      pragma Assert (Adapter_Name /= "");

      --  Look if there is already a child with this name
      if Self.Children /= null then
         Lock_W (Self.Children_Lock);
         --  Write Lock here: content of children has to be the same when
         --  we add the new child.
         Children_Locked := True;
         if Get_Child (Self, Adapter_Name) /= null then
            Droopi.CORBA_P.Exceptions.Raise_Adapter_Already_Exists;
         end if;
      end if;

      --  Create new object adapter
      New_Obj_Adapter           := new Basic_Obj_Adapter;
      Create (New_Obj_Adapter.Children_Lock);
      Create (New_Obj_Adapter.Map_Lock);
      New_Obj_Adapter.Boot_Time := Get_Boot_Time;
      New_Obj_Adapter.Father    := POA_Types.Obj_Adapter_Access (Self);
      New_Obj_Adapter.Name      := Adapter_Name;

      if A_POAManager = null then
         pragma Debug (O ("Create new POAManager"));
         New_Obj_Adapter.POA_Manager := new Basic_POA_Manager;
         Create (New_Obj_Adapter.POA_Manager);
         Register_POA (New_Obj_Adapter.POA_Manager,
                       CORBA.POA_Types.Obj_Adapter_Access (New_Obj_Adapter));
      else
         New_Obj_Adapter.POA_Manager := A_POAManager;
         Register_POA (A_POAManager,
                       CORBA.POA_Types.Obj_Adapter_Access (New_Obj_Adapter));
      end if;

      --  Create and initialize policies factory
      New_Obj_Adapter.P_Factory
        := CORBA.Policy.Policies_Factory_Pkg.New_Factory;
      Initialize (Conf, New_Obj_Adapter.P_Factory);

      --  Init policies with those given by the user
      if Policies /= null then
         Init_With_User_Policies (New_Obj_Adapter, Policies);
      end if;

      --  Use default policies if not provided by the user
      Init_With_Default_Policies (New_Obj_Adapter);

      --  Check compatibilities between policies
      Check_Policies_Compatibility (New_Obj_Adapter);

      --  Register new obj_adapter as a sibling of the current POA
      if not Children_Locked then
         Lock_W (Self.Children_Lock);
      end if;
      Index := Register_Child (Self, New_Obj_Adapter);
      if Length (Self.Absolute_Address) > 0 then
         New_Obj_Adapter.Absolute_Address := Self.Absolute_Address
           & To_CORBA_String (".") & Adapter_Name;
      else
         New_Obj_Adapter.Absolute_Address
           := Self.Absolute_Address & Adapter_Name;
      end if;
      pragma Debug (O
                    ("Absolute name of created POA is "
                     & To_Standard_String (New_Obj_Adapter.Absolute_Address)));
      Unlock_W (Self.Children_Lock);

      return Obj_Adapter_Access (New_Obj_Adapter);

   exception
      when CORBA.Adapter_Already_Exists =>
         --  Reraise exception
         Droopi.CORBA_P.Exceptions.Raise_Adapter_Already_Exists;
         return null;
      when others =>
         Destroy_OA (New_Obj_Adapter);
         raise;
   end Create_POA;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self                : access Basic_Obj_Adapter;
      Etherealize_Objects : in     Boolean;
      Wait_For_Completion : in     Boolean)
   is
      use CORBA.POA_Types.POA_Sequences;
      A_Child : CORBA.POA.Obj_Adapter_Access;
      Name    : String := Self.Name;
   begin
      pragma Debug (O ("Start destroying POA "
                       & To_Standard_String (Name)));

      --  Destroy all children
      Lock_W (Self.Children_Lock);
      if Self.Children /= null then
         for I in 1 .. Length (Sequence (Self.Children.all)) loop
            A_Child := CORBA.POA.Obj_Adapter_Access
              (Element_Of (Sequence (Self.Children.all), I));
            Destroy (A_Child.all'Access,
                     Etherealize_Objects,
                     Wait_For_Completion);
            Replace_Element (Sequence (Self.Children.all), I, null);
         end loop;
      end if;
      Unlock_W (Self.Children_Lock);

      --  Tell father to remove current POA from its list of children
      if Self.Father /= null then
         Remove_POA_By_Name
           (CORBA.POA.Obj_Adapter_Access (Self.Father).all'Access,
            Self.Name);
      end if;

      --  Destroy self (also unregister from the POAManager)
      --  ??? Add code for Etherealize_Objects and Wait_For_Completion
      Destroy_OA (Self);
      pragma Debug (O ("POA "
                       & To_Standard_String (Name)
                       & " destroyed"));
   exception
      when others =>
         Unlock_W (Self.Children_Lock);
         raise;
   end Destroy;

   --------------------------
   -- Create_Thread_Policy --
   --------------------------

   function Create_Thread_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        ThreadPolicyValue)
     return ThreadPolicy_Access
   is
   begin
      return ThreadPolicy_Access
        (CORBA.Policy.Policies_Factory_Pkg.Create (Self.P_Factory.all, Value));
   end Create_Thread_Policy;

   ----------------------------
   -- Create_Lifespan_Policy --
   ----------------------------

   function Create_Lifespan_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        LifespanPolicyValue)
     return LifespanPolicy_Access
   is
   begin
      return LifespanPolicy_Access
        (CORBA.Policy.Policies_Factory_Pkg.Create (Self.P_Factory.all, Value));
   end Create_Lifespan_Policy;

   ---------------------------------
   -- Create_Id_Uniqueness_Policy --
   ---------------------------------

   function Create_Id_Uniqueness_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        IdUniquenessPolicyValue)
     return IdUniquenessPolicy_Access
   is
   begin
      return IdUniquenessPolicy_Access
        (CORBA.Policy.Policies_Factory_Pkg.Create (Self.P_Factory.all, Value));
   end Create_Id_Uniqueness_Policy;

   ----------------------------------
   -- Create_Id_Assignement_Policy --
   ----------------------------------

   function Create_Id_Assignement_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        IdAssignementPolicyValue)
     return IdAssignementPolicy_Access
   is
   begin
      return IdAssignementPolicy_Access
        (CORBA.Policy.Policies_Factory_Pkg.Create (Self.P_Factory.all, Value));
   end Create_Id_Assignement_Policy;

   -------------------------------------
   -- Create_Servent_Retention_Policy --
   -------------------------------------

   function Create_Servant_Retention_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        ServantRetentionPolicyValue)
     return ServantRetentionPolicy_Access
   is
   begin
      return ServantRetentionPolicy_Access
        (CORBA.Policy.Policies_Factory_Pkg.Create (Self.P_Factory.all, Value));
   end Create_Servant_Retention_Policy;

   --------------------------------------
   -- Create_Request_Processing_Policy --
   --------------------------------------

   function Create_Request_Processing_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        RequestProcessingPolicyValue)
     return RequestProcessingPolicy_Access
   is
   begin
      return RequestProcessingPolicy_Access
        (CORBA.Policy.Policies_Factory_Pkg.Create (Self.P_Factory.all, Value));
   end Create_Request_Processing_Policy;

   ---------------------------------------
   -- Create_Implicit_Activation_Policy --
   ---------------------------------------

   function Create_Implicit_Activation_Policy
     (Self  : access Basic_Obj_Adapter;
      Value :        ImplicitActivationPolicyValue)
     return ImplicitActivationPolicy_Access
   is
   begin
      return ImplicitActivationPolicy_Access
        (CORBA.Policy.Policies_Factory_Pkg.Create (Self.P_Factory.all, Value));
   end Create_Implicit_Activation_Policy;

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id
   is
      Oid : Object_Id_Access
        := Activate_Object (Self.Servant_Retention_Policy.all,
                            POA_Types.Obj_Adapter_Access (Self),
                            P_Servant);
   begin
      return Oid.all;
   end Activate_Object;

   -----------------------------
   -- Activate_Object_With_Id --
   -----------------------------

   procedure Activate_Object_With_Id
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access;
      Oid       : in     Object_Id)
   is
   begin
      --      CORBA.Policy.Servant_Retention_Policy.Activate_Object_With_Id
      Activate_Object_With_Id (Self.Servant_Retention_Policy.all,
                               POA_Types.Obj_Adapter_Access (Self),
                               P_Servant,
                               Oid);
   end Activate_Object_With_Id;

   -----------------------
   -- Deactivate_Object --
   -----------------------

   procedure Deactivate_Object
     (Self      : access Basic_Obj_Adapter;
      Oid       : in Object_Id)
   is
   begin
      Deactivate (Self.Servant_Retention_Policy.all,
                  CORBA.POA_Types.Obj_Adapter_Access (Self),
                  Oid);
      --  ??? Wait for completion?
   end Deactivate_Object;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access)
     return Object_Id
   is
      Oid : Object_Id_Access
        := Servant_To_Id (Self.Request_Processing_Policy.all,
                          POA_Types.Obj_Adapter_Access (Self),
                          P_Servant);
   begin
      if Oid = null then
         Raise_Servant_Not_Active;
      end if;
      return Oid.all;
   end Servant_To_Id;

   ---------------------------------------------------------------
   --  Procedures and functions neither in Corba nor in Droopi  --
   ---------------------------------------------------------------

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant
     (Self : access Basic_Obj_Adapter;
      Oid  :        Object_Id)
     return Servant_Access
   is
      Servant : Servant_Access;
   begin
      Servant := Id_To_Servant (Self.Request_Processing_Policy.all,
                                POA_Types.Obj_Adapter_Access (Self),
                                Oid);
      return Servant;
   end Id_To_Servant;

   --------------------------
   -- Find_POA_Recursively --
   --------------------------

   function Find_POA_Recursively
     (Self : access Basic_Obj_Adapter;
      Name : String)
     return Basic_Obj_Adapter_Access
   is
      use CORBA.POA_Types.POA_Sequences;
      Split_Point      : Natural := Index (Name, ".");
      Remaining_Name   : String;
      A_Child_Name     : String;
      A_Child          : Obj_Adapter_Access;
   begin
      if Name = "" then
         return Basic_Obj_Adapter_Access (Self);
      end if;
      if Split_Point /= 0 then
         A_Child_Name := Head (Name, Split_Point - 1);
         Remaining_Name := Tail (Name, Length (Name) - Split_Point);
      else
         A_Child_Name := Name;
      end if;
      for I in 1 .. Length (Sequence (Self.Children.all)) loop
         A_Child := Obj_Adapter_Access (Element_Of
                                        (Sequence (Self.Children.all),
                                         I));
         if A_Child.Name = A_Child_Name then
            if Remaining_Name /= "" then
               return Find_POA_Recursively
                 (Basic_Obj_Adapter (A_Child.all)'Access,
                  Remaining_Name);
            else
               return Basic_Obj_Adapter_Access (A_Child);
            end if;
         end if;
      end loop;
      return null;
   end Find_POA_Recursively;

   ----------------------
   -- Copy_Obj_Adapter --
   ----------------------

   procedure Copy_Obj_Adapter
     (From : in     Basic_Obj_Adapter;
      To   : access Basic_Obj_Adapter)
   is
   begin
      To.Name              := From.Name;
      To.POA_Manager       := From.POA_Manager;
      To.Boot_Time         := From.Boot_Time;
      To.Absolute_Address  := From.Absolute_Address;
      To.Active_Object_Map := From.Active_Object_Map;
      To.Thread_Policy              := From.Thread_Policy;
      To.Request_Processing_Policy  := From.Request_Processing_Policy;
      To.Id_Assignement_Policy      := From.Id_Assignement_Policy;
      To.Id_Uniqueness_Policy       := From.Id_Uniqueness_Policy;
      To.Servant_Retention_Policy   := From.Servant_Retention_Policy;
      To.Lifespan_Policy            := From.Lifespan_Policy;
      To.Implicit_Activation_Policy := From.Implicit_Activation_Policy;
      To.Father            := From.Father;
      To.Children          := From.Children;
      To.Children_Lock     := From.Children_Lock;
      To.Map_Lock          := From.Map_Lock;
   end Copy_Obj_Adapter;

   ------------------------
   -- Remove_POA_By_Name --
   ------------------------

   procedure Remove_POA_By_Name
     (Self       : access Basic_Obj_Adapter;
      Child_Name :        String)
   is
      use POA_Sequences;
      A_Child : POA_Types.Obj_Adapter_Access;
   begin
      pragma Debug (O (To_Standard_String (Self.Name)
                       & ": removing POA with name "
                       & To_Standard_String (Child_Name)
                       & " from my children."));
      for I in 1 .. Length (Self.Children.all) loop
         A_Child := Element_Of (Self.Children.all, I);
         if A_Child /= null
           and then CORBA.POA.Obj_Adapter_Access (A_Child).Name = Child_Name
         then
            Replace_Element (Sequence (Self.Children.all), I, null);
            return;
         end if;
      end loop;
   end Remove_POA_By_Name;

   ---------------------------------------------------
   --  Procedures and functions required by Droopi  --
   ---------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (OA : access Basic_Obj_Adapter)
   is
      Result : Basic_Obj_Adapter_Access
        := Basic_Obj_Adapter_Access (Create_Root_POA);
   begin
      Copy_Obj_Adapter (Result.all, OA);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (OA : access Basic_Obj_Adapter)
   is
   begin
      Destroy (OA, False, False);
      --  ??? False or True
   end Destroy;

   ------------
   -- Export --
   ------------

   function Export
     (OA  : access Basic_Obj_Adapter;
      Obj :        Droopi.Objects.Servant_Access)
     return Droopi.Objects.Object_Id
   is
      Id : Droopi.Objects.Object_Id
        := Droopi.Objects.Object_Id (Activate_Object (OA,
                                                      Servant_Access (Obj)));
   begin
      pragma Debug (O ("Exporting Servant, resulting Id is "
                       & Droopi.Objects.To_String (Id)));
      return Id;
   end Export;

   --------------
   -- Unexport --
   --------------

   procedure Unexport
     (OA : access Basic_Obj_Adapter;
      Id :        Droopi.Objects.Object_Id)
   is
   begin
      Deactivate_Object (OA, Object_Id (Id));
   end Unexport;

   ------------------------
   -- Get_Empty_Arg_List --
   ------------------------

   function Get_Empty_Arg_List
     (OA     : access Basic_Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.NVList.Ref
   is
      S : Servant_Access;
   begin
      pragma Debug (O ("Get_Empty_Arg_List for Id "
                       & Droopi.Objects.To_String (Oid)));
      S := Servant_Access (Find_Servant (OA, Oid, NO_CHECK));
      return S.If_Desc.PP_Desc (Method);
   end Get_Empty_Arg_List;

   ----------------------
   -- Get_Empty_Result --
   ----------------------

   function Get_Empty_Result
     (OA     : access Basic_Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return Droopi.Any.Any
   is
      S : Servant_Access;
   begin
      pragma Debug (O ("Get_Empty_Result for Id "
                       & Droopi.Objects.To_String (Oid)));
      S := Servant_Access (Find_Servant (OA, Oid, NO_CHECK));
      return S.If_Desc.RP_Desc (Method);
   end Get_Empty_Result;

   ------------------
   -- Find_Servant --
   ------------------

   function Find_Servant
     (OA : access Basic_Obj_Adapter;
      Id :        Droopi.Objects.Object_Id)
     return Droopi.Objects.Servant_Access
   is
   begin
      return Find_Servant (OA, Id, CHECK);
   end Find_Servant;

   ------------------
   -- Find_Servant --
   ------------------

   function Find_Servant
     (OA       : access Basic_Obj_Adapter;
      Id       :        Droopi.Objects.Object_Id;
      Do_Check :        Check_State)
     return Droopi.Objects.Servant_Access
   is
      U_Oid  : Unmarshalled_Oid_Access
        := Oid_To_U_Oid (Object_Id (Id));
      The_OA : Basic_Obj_Adapter_Access;
   begin
      if Do_Check = CHECK then
         case Get_State (OA.POA_Manager.all) is
            when DISCARDING | INACTIVE =>
               Raise_Transient (1);
               --  ??? Do we have to do something special for INACTIVE
            when HOLDING =>
               declare
                  S : Droopi.Objects.Servant_Access;
               begin
                  S := Droopi.Objects.Servant_Access
                    (Get_Hold_Servant
                     (OA.POA_Manager.all'Access,
                      CORBA.POA_Types.Obj_Adapter_Access (OA)));
                  return S;
               end;
            when others =>
               null;
         end case;
      end if;
      pragma Debug (O ("Look for OA with name #"
                       & To_Standard_String (U_Oid.Creator)
                       & "# starting from RootPOA"));
      The_OA := Find_POA_Recursively (OA, U_Oid.Creator);
      pragma Debug (O ("OA : "
                       & To_Standard_String (The_OA.Name)
                       & " looks for servant associated with Id "
                       & Droopi.Objects.To_String (Id)));
      if The_OA /= null then
         return Droopi.Objects.Servant_Access (Id_To_Servant (The_OA,
                                                              Id));
      else
         raise Invalid_Object_Id;
         --  This is an exception from Droopi
      end if;
   end Find_Servant;

   ---------------------
   -- Release_Servant --
   ---------------------

   procedure Release_Servant
     (OA      : access Basic_Obj_Adapter;
      Id      :        Droopi.Objects.Object_Id;
      Servant : in out Droopi.Objects.Servant_Access)
   is
   begin
      null;
   end Release_Servant;

end CORBA.POA.Basic_POA;
