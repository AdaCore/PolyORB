with Ada.Text_IO; use Ada.Text_IO;

with Ada.Real_Time;

with Sequences.Unbounded;
with Sequences.Unbounded.Search;
with Droopi.Log;
pragma Elaborate_All (Droopi.Log);

with Droopi.CORBA_P.Exceptions;
with CORBA.Policy_Types;
with CORBA.Policy_Values;


package body CORBA.POA.Basic_POA is

   use POA_Types;
   use Droopi.Log;
   use CORBA.POA_Manager;
   use CORBA.Policy;
   use CORBA.Policy_Types;

   use CORBA.Policy.Thread_Policy;

   package L is new Droopi.Log.Facility_Log ("CORBA.POA.Root_POA");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   function Get_Boot_Time return Time_Stamp;

   function Get_Child (Adapter :    Obj_Adapter_Access;
                       Name    : in String)
                      return POA_Types.Obj_Adapter_Access;

   procedure Init_With_User_Policies (OA       : Obj_Adapter_Access;
                                      Policies : Policy.PolicyList_Access);
   procedure Init_With_Default_Policies (OA : Obj_Adapter_Access);
   procedure Check_Policies_Compatibility (OA : Obj_Adapter_Access);
   procedure Register_Child (Self  : Obj_Adapter_Access;
                             Child : Obj_Adapter_Access);

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child (Adapter :    Obj_Adapter_Access;
                       Name    : in String)
                      return POA_Types.Obj_Adapter_Access
   is
      use POA_Sequences;
      Result  : POAList;
      A_Child : POA_Types.Obj_Adapter_Access;
   begin
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

   procedure Init_With_User_Policies (OA       : Obj_Adapter_Access;
                                      Policies : Policy.PolicyList_Access)
   is
      A_Policy : Policy_Access;
   begin
      for I in 1 .. Policy_Sequences.Length (Policies.all) loop
         A_Policy := Policy_Sequences.Element_Of (Policies.all, I);
         case A_Policy.Policy_Type is
            when THREAD_POLICY_ID =>
               if OA.Thread_Policy /= null then
                  O ("Duplicate in ThreadPolicy: using last one");
               end if;
               OA.Thread_Policy
                 := Create (ThreadPolicy (A_Policy.all));

            when LIFESPAN_POLICY_ID =>
               if OA.Lifespan_Policy /= null then
                  O ("Duplicate in LifespanPolicy: using last one");
               end if;
               OA.Lifespan_Policy
                 := Create (LifespanPolicy (A_Policy.all));

            when ID_UNIQUENESS_POLICY_ID =>
               if OA.Id_Uniqueness_Policy /= null then
                  O ("Duplicate in IdUniquenessPolicy: using last one");
               end if;
               OA.Id_Uniqueness_Policy
                 := Create (IdUniquenessPolicy (A_Policy.all));

            when ID_ASSIGNEMENT_POLICY_ID =>
               if OA.Id_Assignement_Policy /= null then
                  O ("Duplicate in IdAssignementPolicy: using last one");
               end if;
               OA.Id_Assignement_Policy
                 := Create (IdAssignementPolicy (A_Policy.all));

            when SERVANT_RETENTION_POLICY_ID =>
               if OA.Servant_Retention_Policy /= null then
                  O ("Duplicate in ServantRetentionPolicy: using last one");
               end if;
               OA.Servant_Retention_Policy
                 := Create (ServantRetentionPolicy (A_Policy.all));

            when REQUEST_PROCESSING_POLICY_ID =>
               if OA.Request_Processing_Policy /= null then
                  O ("Duplicate in RequestProcessingPolicy: using last one");
               end if;
               OA.Request_Processing_Policy
                 := Create (RequestProcessingPolicy (A_Policy.all));

            when IMPLICIT_ACTIVATION_POLICY_ID =>
               if OA.Implicit_Activation_Policy /= null then
                  O ("Duplicate in ImplicitActivationPolicy: using last one");
               end if;
               OA.Implicit_Activation_Policy
                 := Create (ImplicitActivationPolicy (A_Policy.all));

            when others =>
               null;
               O ("Unknown policy ignored");
         end case;
      end loop;
   end Init_With_User_Policies;

   --------------------------------
   -- Init_With_Default_Policies --
   --------------------------------

   procedure Init_With_Default_Policies (OA : Obj_Adapter_Access)
   is
      use CORBA.Policy_Values;
   begin
      if OA.Thread_Policy = null then
         OA.Thread_Policy := Create_Thread_Policy (ORB_CTRL_MODEL);
      end if;

      if OA.Lifespan_Policy = null then
         OA.Lifespan_Policy
           := Create_Lifespan_Policy (Policy_Values.TRANSIENT);
      end if;

      if OA.Id_Uniqueness_Policy = null then
         OA.Id_Uniqueness_Policy :=
           Create_Id_Uniqueness_Policy (UNIQUE_ID);
      end if;

      if OA.Id_Assignement_Policy = null then
         OA.Id_Assignement_Policy :=
           Create_Id_Assignement_Policy (SYSTEM_ID);
      end if;

      if OA.Servant_Retention_Policy = null then
         OA.Servant_Retention_Policy :=
           Create_Servant_Retention_Policy (RETAIN);
      end if;

      if OA.Request_Processing_Policy = null then
         OA.Request_Processing_Policy :=
           Create_Request_Processing_Policy (USE_ACTIVE_OBJECT_MAP_ONLY);
      end if;

      if OA.Implicit_Activation_Policy = null then
         OA.Implicit_Activation_Policy :=
           Create_Implicit_Activation_Policy (NO_IMPLICIT_ACTIVATION);
      end if;
   end Init_With_Default_Policies;

   ----------------------------------
   -- Check_Policies_Compatibility --
   ----------------------------------

   procedure Check_Policies_Compatibility (OA : Obj_Adapter_Access)
   is
   begin
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

   procedure Register_Child (Self  : Obj_Adapter_Access;
                             Child : Obj_Adapter_Access)
   is
      use CORBA.POA_Types.POA_Sequences;
      A : Natural;
   begin
      if Self /= null then
         if (Self.Children = null) then
            Self.Children := new POAList;
         end if;
         Append (Sequence (Self.Children.all),
                 CORBA.POA_Types.Obj_Adapter_Access (Child));
         A := Length (Sequence (Self.Children.all));
         A := A + 1;
         null;
         null;
      end if;
   end Register_Child;

   ----------------
   -- Create_POA --
   ----------------

   function Create_POA
     (Self         : Obj_Adapter_Access;
      Adapter_Name : String;
      A_POAManager : POA_Manager.POAManager_Access;
      Policies     : Policy.PolicyList_Access)
     return Obj_Adapter_Access
   is
      New_Obj_Adapter : Obj_Adapter_Access;
   begin
      O ("Enter Basic_POA.Create_POA");
      --  ??? Add check code here

      --  If self is null, that means that the poa to create is the RootPOA

      --  Look if there is already a child with this name
      if Self /= null
        and then Self.Children /= null
        and then  Get_Child (Self, Adapter_Name) /= null
      then
         Droopi.CORBA_P.Exceptions.Raise_Adapter_Already_Exists;
      end if;

      --  Create new object adapter
      New_Obj_Adapter           := new Obj_Adapter;
      New_Obj_Adapter.Boot_Time := Get_Boot_Time;
      if Self /= null then
         New_Obj_Adapter.Father := POA_Types.Obj_Adapter_Access (Self);
         New_Obj_Adapter.Name   := Adapter_Name;
      else
         New_Obj_Adapter.Name   := To_CORBA_String ("RootPOA");
      end if;

      if A_POAManager = null then
         --  ??? Use POAManager factory
         null;
      else
         New_Obj_Adapter.POA_Manager := A_POAManager;
      end if;

      --  Init policies with those given by the user
      if Self /= null
        and then Policies /= null then
         Init_With_User_Policies (New_Obj_Adapter, Policies);
      end if;

      --  Use default policies if not provided by the user
      Init_With_Default_Policies (New_Obj_Adapter);

      --  Check compatibilities between policies
      Check_Policies_Compatibility (New_Obj_Adapter);

      --  ??? If error, clean memory
      --  --> An exception is raised: catch it, free the memoy, raise exception

      --  Register new obj_adapter as a sibling of the current POA
      if Self /= null then
         Register_Child (Self, New_Obj_Adapter);
      end if;

      return New_Obj_Adapter;
   end Create_POA;


   ------------
   -- Create --
   ------------

   procedure Create (OA : out Obj_Adapter)
   is
   begin
      null;
   end Create;

   --------------------------
   -- Create_Thread_Policy --
   --------------------------

   function Create_Thread_Policy (Value : ThreadPolicyValue)
                                 return ThreadPolicy_Access
   is
      use CORBA.Policy.Thread_Policy;
   begin
      return Create (Value);
   end Create_Thread_Policy;

   ----------------------------
   -- Create_Lifespan_Policy --
   ----------------------------

   function Create_Lifespan_Policy (Value : LifespanPolicyValue)
                                 return LifespanPolicy_Access
   is
      use CORBA.Policy.Lifespan_Policy;
   begin
      return Create (Value);
   end Create_Lifespan_Policy;

   ---------------------------------
   -- Create_Id_Uniqueness_Policy --
   ---------------------------------

   function Create_Id_Uniqueness_Policy
     (Value : IdUniquenessPolicyValue)
     return IdUniquenessPolicy_Access
   is
      use CORBA.Policy.Id_Uniqueness_Policy;
   begin
      return Create (Value);
   end Create_Id_Uniqueness_Policy;

   ----------------------------------
   -- Create_Id_Assignement_Policy --
   ----------------------------------

   function Create_Id_Assignement_Policy
     (Value : IdAssignementPolicyValue)
     return IdAssignementPolicy_Access
   is
      use CORBA.Policy.Id_Assignement_Policy;
   begin
      return Create (Value);
   end Create_Id_Assignement_Policy;

   -------------------------------------
   -- Create_Servent_Retention_Policy --
   -------------------------------------

   function Create_Servant_Retention_Policy
     (Value : ServantRetentionPolicyValue)
     return ServantRetentionPolicy_Access
   is
      use CORBA.Policy.Servant_Retention_Policy;
   begin
      return Create (Value);
   end Create_Servant_Retention_Policy;

   --------------------------------------
   -- Create_Request_Processing_Policy --
   --------------------------------------

   function Create_Request_Processing_Policy
     (Value : RequestProcessingPolicyValue)
     return RequestProcessingPolicy_Access
   is
      use CORBA.Policy.Request_Processing_Policy;
   begin
      return Create (Value);
   end Create_Request_Processing_Policy;

   ---------------------------------------
   -- Create_Implicit_Activation_Policy --
   ---------------------------------------

   function Create_Implicit_Activation_Policy
     (Value : ImplicitActivationPolicyValue)
     return ImplicitActivationPolicy_Access
   is
      use CORBA.Policy.Implicit_Activation_Policy;
   begin
      return Create (Value);
   end Create_Implicit_Activation_Policy;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (OA : in out Obj_Adapter)
   is
   begin
      null;
   end Destroy;

   ------------
   -- Export --
   ------------

   function Export
     (OA  : access Obj_Adapter;
      Obj :        Droopi.Objects.Servant_Access)
     return Droopi.Objects.Object_Id
   is
   begin
      return Export (OA, Obj);
   end Export;

   --------------
   -- Unexport --
   --------------

   procedure Unexport
     (OA : access Obj_Adapter;
      Id :        Droopi.Objects.Object_Id)
   is
   begin
      null;
   end Unexport;

   ------------------------
   -- Get_Empty_Arg_List --
   ------------------------

   function Get_Empty_Arg_List
     (OA     : Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return CORBA.NVList.Ref
   is
   begin
      return Get_Empty_Arg_List (OA, Oid, Method);
   end Get_Empty_Arg_List;

   ----------------------
   -- Get_Empty_Result --
   ----------------------

   function Get_Empty_Result
     (OA     : Obj_Adapter;
      Oid    : Droopi.Objects.Object_Id;
      Method : Droopi.Requests.Operation_Id)
     return CORBA.Any
   is
   begin
      return Get_Empty_Result (OA, Oid, Method);
   end Get_Empty_Result;

   ------------------
   -- Find_Servant --
   ------------------

   function Find_Servant
     (OA : access Obj_Adapter;
      Id :        Droopi.Objects.Object_Id)
     return Droopi.Objects.Servant_Access
   is
   begin
      return Find_Servant (OA, Id);
   end Find_Servant;

   ---------------------
   -- Release_Servant --
   ---------------------

   procedure Release_Servant
     (OA      : access Obj_Adapter;
      Id      :        Droopi.Objects.Object_Id;
      Servant : in out Droopi.Objects.Servant_Access)
   is
   begin
      null;
   end Release_Servant;

end CORBA.POA.Basic_POA;
