------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                          P O L Y O R B . P O A                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2022, Free Software Foundation, Inc.          --
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

pragma Ada_2012;

--  Abstract interface for the POA

with Ada.Streams;
with PolyORB.Utils.Unchecked_Deallocation;

with PolyORB.Log;
with PolyORB.Obj_Adapters;
with PolyORB.Obj_Adapter_QoS;
with PolyORB.POA_Config;
with PolyORB.POA_Manager.Basic_Manager;
with PolyORB.Smart_Pointers;
with PolyORB.Tasking;
with PolyORB.Tasking.Threads;
with PolyORB.Utils;

package body PolyORB.POA is

   use Ada.Streams;

   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.POA_Manager;
   use PolyORB.POA_Policies;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Types;
   use PolyORB.Utils;

   package L is new Log.Facility_Log ("polyorb.poa");
   procedure O (Message : Standard.String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;

   subtype Lifespan_Cookie_SEA is
     Stream_Element_Array (1 .. (Lifespan_Cookie'Size + 7) / 8);

   function To_Hex (C : Lifespan_Cookie) return String;
   function To_Lifespan_Cookie (H : String) return Lifespan_Cookie;
   --  Conversion between lifespan cookie and hex string representation

   --------------------
   -- Oid_To_Rel_URI --
   --------------------

   overriding procedure Oid_To_Rel_URI
     (OA    : access Obj_Adapter;
      Id    : access Object_Id;
      URI   : out Types.String;
      Error : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

      U_Oid : Unmarshalled_Oid;
   begin
      Oid_To_U_Oid (Id.all, U_Oid, Error);
      if Found (Error) then
         return;
      end if;
      URI := To_PolyORB_String ("/");
      pragma Debug (C, O ("Oid: Creator: "
                         & To_Standard_String (U_Oid.Creator)
                         & ", Id: " & To_Standard_String (U_Oid.Id)
                         & ", sys = " & Boolean'Image
                         (U_Oid.System_Generated)
                         & ", pf = " & To_Hex (U_Oid.Persistency_Flag)));

      if Length (U_Oid.Creator) /= 0 then
         URI := URI & U_Oid.Creator & To_PolyORB_String ("/");
      end if;

      URI := URI & URI_Encode (To_Standard_String (U_Oid.Id));
      --  XXX Here we make the assumption that Id needs to be URI-escaped, and
      --  Creator needs not, but there is no reason to. What should actually be
      --  done is that Creator should be a list, and each of its components
      --  should be separately URLencoded.

      if U_Oid.System_Generated then
         URI := URI & ";sys";
      end if;

      if U_Oid.Persistency_Flag /= Null_Time_Stamp then
         URI := URI & ";pf=" & To_Hex (U_Oid.Persistency_Flag);
      end if;

      pragma Debug (C, O ("-> URI: " & To_Standard_String (URI)));
   end Oid_To_Rel_URI;

   --------------------
   -- Rel_URI_To_Oid --
   --------------------

   overriding function Rel_URI_To_Oid
     (OA  : access Obj_Adapter;
      URI :        String) return Object_Id_Access
   is
      pragma Unreferenced (OA);

      Colon      : Integer := Find (URI, URI'First, ';');
      Last_Slash : Integer := Colon - 1;

      Creator_First, Creator_Last : Integer;
      Id_First, Id_Last           : Integer;
      System_Generated            : Boolean;
      Persistency_Flag            : Lifespan_Cookie;

   begin
      pragma Debug (C, O ("URI: " & URI));

      while URI (Last_Slash) /= '/'
        and then Last_Slash >= URI'First loop
         Last_Slash := Last_Slash - 1;
      end loop;

      pragma Assert (URI (URI'First) = '/'
                     and then Last_Slash >= URI'First);

      Creator_First := URI'First + 1;
      Creator_Last := Last_Slash - 1;

      Id_First := Last_Slash + 1;
      Id_Last := Colon - 1;

      if Colon + 3 <= URI'Last
        and then URI (Colon + 1 .. Colon + 3) = "sys"
      then
         System_Generated := True;
         Colon := Find (URI, Colon + 1, ';');
      else
         System_Generated := False;
      end if;

      if Colon + 3 <= URI'Last
        and then URI (Colon + 1 .. Colon + 3) = "pf="
      then
         Persistency_Flag := To_Lifespan_Cookie (URI (Colon + 4 .. URI'Last));

      else
         Persistency_Flag := Null_Time_Stamp;
      end if;

      pragma Debug (C, O ("-> Oid: Creator: "
                       & URI (Creator_First .. Creator_Last)
                       & ", Id: "
                       & URI (Id_First .. Id_Last)
                       & ", sys = "
                       & Boolean'Image (System_Generated)
                       & ", pf = "
                       & To_Hex (Persistency_Flag)));

      return Create_Id (Name => URI_Decode (URI (Id_First .. Id_Last)),
                        System_Generated => System_Generated,
                        Persistency_Flag => Persistency_Flag,
                        Creator => URI_Decode
                        (URI (Creator_First .. Creator_Last)));
   end Rel_URI_To_Oid;

   ----------------------
   -- Global POA Table --
   ----------------------

   Global_POATable : POATable;

   --  This table is used to shortcut POA recursive search, when
   --  possible. It contains all registred POAs with full path name.

   ------------------------------------
   --  Code of additional functions  --
   ------------------------------------

   --------------------
   -- POA_Manager_Of --
   --------------------

   function POA_Manager_Of
     (OA : access Obj_Adapter) return POA_Manager.POAManager_Access
   is
      use Smart_Pointers;

      E : constant Entity_Ptr := Entity_Of (OA.POA_Manager);

   begin
      pragma Assert (E.all in POA_Manager.POAManager'Class);

      return POAManager_Access (E);
   end POA_Manager_Of;

   ------------------
   -- Set_Policies --
   ------------------

   procedure Set_Policies
     (OA       : access Obj_Adapter;
      Policies :        POA_Policies.PolicyList;
      Default  :        Boolean)
   is
      use Policy_Lists;

      It       : Iterator := First (Policies);
      A_Policy : Policy_Access;

   begin
      Enter (OA.POA_Lock);

      while not Last (It) loop
         A_Policy := Value (It).all;

         if A_Policy.all in ThreadPolicy'Class then
            if OA.Thread_Policy = null or else not Default then
               if OA.Thread_Policy /= null then
                  pragma Debug
                    (C, O ("Duplicate in ThreadPolicy: using last one"));
                  null;
               end if;
               OA.Thread_Policy := ThreadPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in LifespanPolicy'Class then
            if OA.Lifespan_Policy = null or else not Default then
               if OA.Lifespan_Policy /= null then
                  pragma Debug
                    (C, O ("Duplicate in LifespanPolicy: using last one"));
                  null;
               end if;
               OA.Lifespan_Policy := LifespanPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in IdUniquenessPolicy'Class then
            if OA.Id_Uniqueness_Policy = null or else not Default then
               if OA.Id_Uniqueness_Policy /= null then
                  pragma Debug
                    (C, O ("Duplicate in IdUniquenessPolicy: using last one"));
                  null;
               end if;
               OA.Id_Uniqueness_Policy := IdUniquenessPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in IdAssignmentPolicy'Class then
            if OA.Id_Assignment_Policy = null or else not Default then
               if OA.Id_Assignment_Policy /= null then
                  pragma Debug
                    (C, O ("Duplicate in IdAssignmentPolicy: using last one"));
                  null;
               end if;
               OA.Id_Assignment_Policy := IdAssignmentPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in ServantRetentionPolicy'Class then
            if OA.Servant_Retention_Policy = null or else not Default then
               if OA.Servant_Retention_Policy /= null then
                  pragma Debug
                    (C, O ("Duplicate in ServantRetentionPolicy:"
                        & " using last one"));
                  null;
               end if;
               OA.Servant_Retention_Policy :=
                 ServantRetentionPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in RequestProcessingPolicy'Class then
            if OA.Request_Processing_Policy = null or else not Default then
               if OA.Request_Processing_Policy /= null then
                  pragma Debug
                    (C, O ("Duplicate in RequestProcessingPolicy:"
                        & " using last one"));
                  null;
               end if;
               OA.Request_Processing_Policy :=
                 RequestProcessingPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in ImplicitActivationPolicy'Class then
            if OA.Implicit_Activation_Policy = null or else not Default then
               if OA.Implicit_Activation_Policy /= null then
                  pragma Debug
                    (C, O ("Duplicate in ImplicitActivationPolicy:"
                        & "using last one"));
                  null;
               end if;
               OA.Implicit_Activation_Policy :=
                 ImplicitActivationPolicy_Access (A_Policy);
            end if;

         else
            null;
            pragma Debug (C, O ("Unknown policy ignored"));
         end if;

         Next (It);
      end loop;

      Leave (OA.POA_Lock);
   end Set_Policies;

   ------------
   -- To_Hex --
   ------------

   function To_Hex (C : Lifespan_Cookie) return String is
      C_SEA : Lifespan_Cookie_SEA;
      for C_SEA'Address use C'Address;
      pragma Import (Ada, C_SEA);
   begin
      return SEA_To_Hex_String (C_SEA);
   end To_Hex;

   ------------------------
   -- To_Lifespan_Cookie --
   ------------------------

   function To_Lifespan_Cookie (H : String) return Lifespan_Cookie is
      C     : aliased Lifespan_Cookie;
      C_SEA : Lifespan_Cookie_SEA;
      for C_SEA'Address use C'Address;
      pragma Import (Ada, C_SEA);
   begin
      C_SEA := Hex_String_To_SEA (H);
      return C;
   end To_Lifespan_Cookie;

   -----------------------------
   -- Init_With_User_Policies --
   -----------------------------

   procedure Init_With_User_Policies
     (OA       : access Obj_Adapter;
      Policies :        POA_Policies.PolicyList) is
   begin
      pragma Debug (C, O ("Init POA with user provided policies"));

      Set_Policies (OA, Policies, Default => False);
   end Init_With_User_Policies;

   --------------------------------
   -- Init_With_Default_Policies --
   --------------------------------

   procedure Init_With_Default_Policies
     (OA : access Obj_Adapter) is
   begin
      pragma Debug (C, O ("Init POA with default policies"));

      Set_Policies
        (OA,
         POA_Config.Default_Policies (POA_Config.Configuration.all),
         Default => True);
   end Init_With_Default_Policies;

   ----------------------------------
   -- Check_Policies_Compatibility --
   ----------------------------------

   procedure Check_Policies_Compatibility
     (OA    :        Obj_Adapter_Access;
      Error : in out PolyORB.Errors.Error_Container)
   is
      OA_Policies : AllPolicies;

   begin
      pragma Debug (C, O ("Check compatibilities between policies: enter"));
      Enter (OA.POA_Lock);

      OA_Policies (1) := Policy_Access (OA.Thread_Policy);
      OA_Policies (2) := Policy_Access (OA.Lifespan_Policy);
      OA_Policies (3) := Policy_Access (OA.Id_Uniqueness_Policy);
      OA_Policies (4) := Policy_Access (OA.Id_Assignment_Policy);
      OA_Policies (5) := Policy_Access (OA.Servant_Retention_Policy);
      OA_Policies (6) := Policy_Access (OA.Request_Processing_Policy);
      OA_Policies (7) := Policy_Access (OA.Implicit_Activation_Policy);

      Check_Compatibility
        (OA.Thread_Policy.all,
         OA_Policies,
         Error);

      Check_Compatibility
        (OA.Lifespan_Policy.all,
         OA_Policies,
         Error);

      Check_Compatibility
        (OA.Id_Uniqueness_Policy.all,
         OA_Policies,
         Error);

      Check_Compatibility
        (OA.Id_Assignment_Policy.all,
         OA_Policies,
         Error);

      Check_Compatibility
        (OA.Servant_Retention_Policy.all,
         OA_Policies,
         Error);

      Check_Compatibility
        (OA.Request_Processing_Policy.all,
         OA_Policies,
         Error);

      Check_Compatibility
        (OA.Implicit_Activation_Policy.all,
         OA_Policies,
         Error);

      Leave (OA.POA_Lock);
      pragma Debug (C, O ("Check compatibilities between policies: leave"));
   end Check_Policies_Compatibility;

   ----------------------
   -- Destroy_Policies --
   ----------------------

   procedure Destroy_Policies (OA : in out Obj_Adapter) is
      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Policy'Class,
         Name => Policy_Access);
   begin
      Free (Policy_Access (OA.Thread_Policy));
      Free (Policy_Access (OA.Id_Uniqueness_Policy));
      Free (Policy_Access (OA.Id_Assignment_Policy));
      Free (Policy_Access (OA.Implicit_Activation_Policy));
      Free (Policy_Access (OA.Lifespan_Policy));
      Free (Policy_Access (OA.Request_Processing_Policy));
      Free (Policy_Access (OA.Servant_Retention_Policy));
   end Destroy_Policies;

   ---------------------
   -- Create_Root_POA --
   ---------------------

   procedure Create_Root_POA (New_Obj_Adapter : access Obj_Adapter) is
      use PolyORB.POA_Types.POA_HTables;

   begin
      pragma Debug (C, O ("Create Root_POA"));

      --  Create new Obj Adapter

      New_Obj_Adapter.Boot_Time        := Tasking.Threads.Node_Boot_Time;
      New_Obj_Adapter.Name             := +"RootPOA";
      New_Obj_Adapter.Absolute_Address := +"";
      Create (New_Obj_Adapter.POA_Lock);
      Create (New_Obj_Adapter.Children_Lock);
      Create (New_Obj_Adapter.Map_Lock);

      --  Attach a POA Manager to the root POA

      Set (New_Obj_Adapter.POA_Manager, new Basic_Manager.Basic_POA_Manager);
      Create (POA_Manager_Of (New_Obj_Adapter));
      Register_POA
        (POA_Manager_Of (New_Obj_Adapter),
         POA_Types.Obj_Adapter_Access (New_Obj_Adapter));

      --  Create and initialize policies factory

      POA_Config.Initialize (POA_Config.Configuration.all);
      --  XXX is this really the role of Create_Root_POA to initialize this ???

      --  Use default policies

      Init_With_Default_Policies (New_Obj_Adapter);

      --  Initialize Global POA Table

      Initialize (Global_POATable);
   end Create_Root_POA;

   ---------------------------------------------
   -- CORBA-like POA interface implementation --
   ---------------------------------------------

   --------------------
   -- Initialize_POA --
   --------------------

   procedure Initialize_POA
     (Self         : access Obj_Adapter;
      Adapter_Name : Standard.String;
      A_POAManager : POA_Manager.POAManager_Access;
      Policies     : POA_Policies.PolicyList;
      POA          : in out Obj_Adapter_Access;
      Error        : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.POA_Types.POA_HTables;

      Ref : PolyORB.POA_Types.Obj_Adapter_Ref;

   begin
      pragma Debug (C, O ("Creating POA: " & Adapter_Name));

      --  Validity checks on Adapter_Name:
      --    name must be non-empty and may not contain POA_Path_Separator
      --    or ASCII.NUL

      if Adapter_Name = ""
        or else PolyORB.Utils.Find (Adapter_Name,
                  Adapter_Name'First,
                  POA_Path_Separator) <= Adapter_Name'Last
        or else PolyORB.Utils.Find (Adapter_Name,
                  Adapter_Name'First,
                  ASCII.NUL) <= Adapter_Name'Last
      then
         Throw (Error,
                WrongAdapter_E,
                Null_Members'(Null_Member));
         --  XXX Check error name
         return;
      end if;

      --  Look if there is already a child with this name

      Enter (Self.Children_Lock);

      if Self.Children /= null then
         pragma Debug (C, O ("Check if a POA with the same name exists."));

         if not Is_Null (Lookup (Self.Children.all,
                                 Adapter_Name,
                                 Null_POA_Ref))
         then
            Throw (Error,
                   AdapterAlreadyExists_E,
                   Null_Members'(Null_Member));

            Leave (Self.Children_Lock);
            return;
         end if;
      end if;

      --  Create new object adapter

      Create (POA.POA_Lock);
      Create (POA.Children_Lock);
      Create (POA.Map_Lock);
      POA.Boot_Time := Self.Boot_Time;
      POA.Father    := POA_Types.Obj_Adapter_Access (Self);
      POA.Name      := +Adapter_Name;

      if A_POAManager = null then
         Set (POA.POA_Manager,
              new Basic_Manager.Basic_POA_Manager);
         Create (POA_Manager_Of (POA));
         Register_POA (POA_Manager_Of (POA),
                       POA_Types.Obj_Adapter_Access (POA));
      else
         Set (POA.POA_Manager,
              Smart_Pointers.Entity_Ptr (A_POAManager));
         Register_POA (A_POAManager,
                       POA_Types.Obj_Adapter_Access (POA));
      end if;

      --  NOTE: POA.Children is initialized iff we
      --  need it, see the procedure Register_Child for more details.

      --  Register new obj_adapter as a sibling of the current POA

      if Self.Children = null then
         Self.Children := new POATable;
         Initialize (Self.Children.all);
      end if;

      Set (Ref, Smart_Pointers.Entity_Ptr (POA));
      Insert (Self.Children.all, POA.Name.all, Ref);

      Leave (Self.Children_Lock);

      --  Construct POA Absolute name

      if Self.Absolute_Address.all'Length > 0 then
         POA.Absolute_Address :=
           new Standard.String'
             (Self.Absolute_Address.all
              & POA_Path_Separator
              & Adapter_Name);
      else
         POA.Absolute_Address :=
           new Standard.String'(Self.Absolute_Address.all & Adapter_Name);
      end if;

      pragma Debug
        (C, O ("Absolute name of new POA is " & POA.Absolute_Address.all));

      --  First initialize POA with default policies

      Init_With_Default_Policies (POA);

      --  then override POA policies with those given by the user

      Init_With_User_Policies (POA, Policies);

      --  Check compatibilities between policies

      Check_Policies_Compatibility (POA, Error);

      if Found (Error) then
         pragma Debug (C, O ("Got Error, destroying POA"));
         Destroy (POA,
           Etherealize_Objects => False,
           Wait_For_Completion => False);
         return;
      end if;

      --  Insert POA into Global_POATable

      pragma Debug
        (C, O ("Insert "
            & POA_Path_Separator
            & POA.Absolute_Address.all
            & " into Global_POATable"));

      Insert (Global_POATable,
              POA_Path_Separator & POA.Absolute_Address.all,
              Ref);

      --  Return the created POA

      pragma Debug (C, O ("POA " & Adapter_Name & " created."));
   end Initialize_POA;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self                : access Obj_Adapter;
      Etherealize_Objects : Types.Boolean;
      Wait_For_Completion : Types.Boolean)
   is
      use PolyORB.Object_Maps;
      use PolyORB.POA_Types.POA_HTables;

      procedure Free is new PolyORB.Utils.Unchecked_Deallocation.Free
        (Object => Object_Map'Class,
         Name => Object_Map_Access);

   begin
      --  We might be finalizing a POA (because of reference counting)
      --  on which Destroy has already been called (with non-default
      --  parameters), in which case there is nothing to do.

      if Self.Name = null then
         return;
      end if;

      pragma Debug (C, O ("Start destroying POA: " & Self.Name.all));

      --  Remove Self from Global POA Table

      pragma Debug (C, O ("Removing POA from Global POA Table"));

      PolyORB.POA_Types.POA_HTables.Delete
        (Global_POATable, POA_Path_Separator & Self.Absolute_Address.all);

      --  Destroy all children

      if Self.Children /= null then
         pragma Debug (C, O ("Removing child POAs"));

         Enter (Self.Children_Lock);

         declare
            It        : Iterator := First (Self.Children.all);
            Child_Ref : Obj_Adapter_Ref;
            Child_Ptr : Obj_Adapter_Access;
         begin
            while not Last (It) loop
               --  Hold a ref on the child while we destroy it to ensure
               --  that it does not get finalized early.

               Child_Ref := Value (It);
               Child_Ptr := Obj_Adapter (Entity_Of (Child_Ref).all)'Access;

               Destroy (Child_Ptr, Etherealize_Objects, Wait_For_Completion);

               --  NOTE: The child is detached automatically from the children
               --  map upon destruction.

               Next (It);
            end loop;
         end;
         Finalize (Self.Children.all);
         Free (Self.Children);
         Leave (Self.Children_Lock);
      end if;

      --  Tell father to remove current POA from its list of children

      if Self.Father /= null then
         pragma Debug (C, O ("Requesting parent to detach POA: "
                          & Self.Name.all));

         POA.Remove_POA_By_Name
           (POA.Obj_Adapter_Access (Self.Father),
            Self.Name.all);
      end if;

      --  Destroy self (also unregister from the POAManager)

      pragma Debug (C, O ("About to destroy POA: " & Self.Name.all));

      Free (Self.Absolute_Address);
      Free (Self.Name);

      --  Destroy POA components

      if not Is_Nil (Self.POA_Manager) then
         Remove_POA (POA_Manager_Of (Self), Self.all'Access);
         Unref (Self.POA_Manager);
      end if;

      --  Destroy_Policies (Self.all);
      --  XXX Cannot destroy_policies here because another
      --  POA initialised from the default configuration could
      --  be using the same instances of policy objects!

      --  XXX if so why don't we make policies a derived type from a
      --  Smart Pointer ??

      --  Destroy Locks

      --  As Destroy may be called when an exception is raised during OA
      --  initialization, check for non-null values explicitly.

      if Self.POA_Lock /= null then
         Destroy (Self.POA_Lock);
      end if;

      if Self.Children_Lock /= null then
         Destroy (Self.Children_Lock);
      end if;

      if Self.Map_Lock /= null then
         Destroy (Self.Map_Lock);
      end if;

      --  These members may be null, test before freeing

      if Self.Active_Object_Map /= null then
         Object_Maps.Finalize (Self.Active_Object_Map.all);
         Free (Self.Active_Object_Map);
      end if;

      if Self.Adapter_Activator /= null then
         Free (Self.Adapter_Activator);
      end if;

      if Self.Servant_Manager /= null then
         Free (Self.Servant_Manager);
      end if;

      pragma Debug (C, O ("POA destroyed"));

      --  XXX Add code for Etherealize_Objects and Wait_For_Completion ???
   end Destroy;

   ----------------------------------
   -- Create_Object_Identification --
   ----------------------------------

   procedure Create_Object_Identification
     (Self  : access Obj_Adapter;
      Hint  :        Object_Id_Access;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Errors.Error_Container) is
   begin
      Assign_Object_Identifier
        (Self.Id_Assignment_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         Hint,
         U_Oid,
         Error);
   end Create_Object_Identification;

   ---------------------
   -- Activate_Object --
   ---------------------

   procedure Activate_Object
     (Self      : access Obj_Adapter;
      P_Servant : Servants.Servant_Access;
      Hint      :        Object_Id_Access;
      U_Oid     :    out Unmarshalled_Oid;
      Error     : in out PolyORB.Errors.Error_Container) is
   begin
      pragma Debug (C, O ("Activate_Object: enter"));

      --  Build a well formed Oid from the 'Hint' provided by the user

      Assign_Object_Identifier
        (Self.Id_Assignment_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         Hint,
         U_Oid,
         Error);

      if Found (Error) then
         return;
      end if;

      Retain_Servant_Association
        (Self.Servant_Retention_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         P_Servant,
         U_Oid,
         Error);

      if Found (Error) then
         return;
      end if;

      pragma Debug (C, O ("Activate_Object: leave"));
   end Activate_Object;

   -----------------------
   -- Deactivate_Object --
   -----------------------

   procedure Deactivate_Object
     (Self  : access Obj_Adapter;
      Oid   : Object_Id;
      Error : in out PolyORB.Errors.Error_Container)
   is
      U_Oid : Unmarshalled_Oid;
   begin
      pragma Debug (C, O ("Deactivate_Object: enter"));

      Reconstruct_Object_Identifier
        (Self.Id_Assignment_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         Oid,
         U_Oid,
         Error);

      if Found (Error) then
         return;
      end if;

      if Self.Servant_Manager /= null
        and then Self.Servant_Manager.all in ServantActivator'Class
      then
         pragma Debug (C, O ("Deactivate_Object: Etherealizing"));

         declare
            Activator : aliased ServantActivator'Class :=
              ServantActivator (Self.Servant_Manager.all);

            Servant : Servants.Servant_Access;

         begin
            Id_To_Servant
              (Self,
               U_Oid_To_Oid (U_Oid),
               Servant,
               Error);

            if Found (Error) then
               pragma Debug (C, O ("Deactivate_Object: "
                                & "Failed to retrieve servant"));
               return;
            end if;

            pragma Debug (C, O ("Deactivate_Object: "
                             & "Etherealizing corresponding servant"));
            Etherealize
              (Activator'Access,
               Oid,
               Self,
               Servant,
               Cleanup_In_Progress => True,
               Remaining_Activations => True
               );
            --  XXX should compute Remaining_Activations value
         end;
      end if;

      pragma Debug (C, O ("Deactivate_Object: Forget_Servant_Association"));
      Forget_Servant_Association
        (Self.Servant_Retention_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         U_Oid,
         Error);

      if Found (Error) then
         return;
      end if;

      --  XXX ??? Wait for completion?

      pragma Debug (C, O ("Deactivate_Object: leave"));
   end Deactivate_Object;

   -------------------
   -- Servant_To_Id --
   -------------------

   procedure Servant_To_Id
     (Self      : access Obj_Adapter;
      P_Servant : Servants.Servant_Access;
      Oid       :    out Object_Id_Access;
      Error     : in out PolyORB.Errors.Error_Container)
   is
      Temp_Oid, Temp_Oid2 : Object_Id_Access;
   begin
      Temp_Oid := Retained_Servant_To_Id
        (Self.Servant_Retention_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         P_Servant);

      Activate_Again
        (Self.Id_Uniqueness_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         P_Servant,
         Temp_Oid,
         Temp_Oid2,
         Error);

      if Found (Error) then
         return;
      end if;

      Oid := Temp_Oid2;

      if Oid = null then
         Throw (Error,
                ServantNotActive_E,
                Null_Members'(Null_Member));

         --  XXX here should also check whether we are in the context of
         --  executing a dispatched operation on Servant, and if it is the case
         --  return the 'current' oid (for USE_DEFAULT_SERVANT policy).
      end if;

      Object_Identifier
        (Self.Id_Assignment_Policy.all,
         Temp_Oid2,
         Oid, Error);
   end Servant_To_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   procedure Id_To_Servant
     (Self    : access Obj_Adapter;
      Oid     :        Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      U_Oid : Unmarshalled_Oid;
   begin
      Oid_To_U_Oid (Oid, U_Oid, Error);
      if Found (Error) then
         return;
      end if;

      Ensure_Lifespan
        (Self.Lifespan_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         U_Oid,
         Error);
      if Found (Error) then
         return;
      end if;

      Id_To_Servant
        (Self.Request_Processing_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         U_Oid,
         Servant,
         Error);
   end Id_To_Servant;

   --------------
   -- Find_POA --
   --------------

   procedure Find_POA
     (Self        : access Obj_Adapter;
      Name        :        String;
      Activate_It :        Boolean;
      POA         :    out Obj_Adapter_Access;
      Error       : in out PolyORB.Errors.Error_Container)
   is
      use PolyORB.POA_Types.POA_HTables;

      procedure Find_POA_Recursively
        (Self        : access Obj_Adapter;
         Name        :        String;
         Activate_It :        Boolean;
         POA         :    out Obj_Adapter_Access;
         Error       : in out PolyORB.Errors.Error_Container);
      --  Looks for 'name', searching from 'Self', using a recursive search.
      --  If necessary, will invoke AdapterActivator call backs.

      --------------------------
      -- Find_POA_Recursively --
      --------------------------

      procedure Find_POA_Recursively
        (Self        : access Obj_Adapter;
         Name        :        String;
         Activate_It :        Boolean;
         POA         :    out Obj_Adapter_Access;
         Error       : in out PolyORB.Errors.Error_Container)
      is
         A_Child     : Obj_Adapter_Access;
         Result      : Boolean;
         Split_Point : Integer;

      begin
         pragma Debug (C, O ("Find_POA_Recursively: enter, Name = " & Name));

         --  Name is null => return Self

         if Name'Length = 0 then
            POA := Obj_Adapter_Access (Self);
            return;
         end if;

         Split_Point :=
           PolyORB.Utils.Find (Name, Name'First, POA_Path_Separator);

         pragma Assert (Split_Point /= Name'First);

         --  Check Self's children

         if Self.Children /= null then
            A_Child := Obj_Adapter_Access
              (Entity_Of (Lookup (Self.Children.all,
                                  Name (Name'First .. Split_Point - 1),
                                  Null_POA_Ref)));
         end if;

         if A_Child /= null then

            --  A child corresponds to partial name, follow search

            Find_POA
              (Obj_Adapter (A_Child.all)'Access,
               Name (Split_Point + 1 .. Name'Last),
               Activate_It,
               POA,
               Error);

         else

            --  No child corresponds, activate one POA if requested

            if Activate_It
              and then Self.Adapter_Activator /= null
            then
               Unknown_Adapter
                 (Self.Adapter_Activator,
                  Self,
                  Name (Name'First .. Split_Point - 1),
                  Result,
                  Error);

               if Found (Error) then
                  return;
               end if;

               if not Result then
                  Throw (Error,
                         AdapterNonExistent_E,
                         Null_Member);
               end if;

               Find_POA
                 (Self,
                  Name,
                  Activate_It,
                  POA,
                  Error);
            else
               POA := null;

               Throw (Error,
                      AdapterNonExistent_E,
                      Null_Member);
            end if;
         end if;
      end Find_POA_Recursively;

   begin

      --  Name is null => return self

      if Name'Length = 0 then
         POA := Obj_Adapter_Access (Self);
         return;
      end if;

      --  Then look up name in Global POA Table

      declare
         Full_POA_Name : constant String :=
           Self.Absolute_Address.all
           & POA_Path_Separator
           & Name;
      begin
         pragma Debug (C, O ("Find_POA: enter, Name = "
                          & Full_POA_Name));

         POA := PolyORB.POA.Obj_Adapter_Access
           (Entity_Of
              (Lookup
                 (Global_POATable,
                  Full_POA_Name,
                  Null_POA_Ref)));

         if POA /= null then
            pragma Debug (C, O ("Found POA in Global_POATable"));
            return;
         end if;
      end;

      --  Then make a recursive look up, activating POA if necessary

      pragma Debug (C, O ("Looking for " & Name & " recursively"));

      Find_POA_Recursively
        (Self,
         Name,
         Activate_It,
         POA,
         Error);

      pragma Debug (C, O ("Find_POA: leave"));
   end Find_POA;

   -----------------
   -- Get_Servant --
   -----------------

   procedure Get_Servant
     (Self    : access Obj_Adapter;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container) is
   begin
      Get_Servant
        (Self.Request_Processing_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         Servant,
         Error);
   end Get_Servant;

   -----------------
   -- Set_Servant --
   -----------------

   procedure Set_Servant
     (Self    : access Obj_Adapter;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container) is
   begin
      Set_Servant
        (Self.Request_Processing_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         Servant,
         Error);
   end Set_Servant;

   -------------------------
   -- Get_Servant_Manager --
   -------------------------

   procedure Get_Servant_Manager
     (Self    : access Obj_Adapter;
      Manager :    out ServantManager_Access;
      Error   : in out PolyORB.Errors.Error_Container) is
   begin
      Ensure_Servant_Manager
        (Self.Request_Processing_Policy.all,
         Error);

      if Found (Error) then
         return;
      end if;

      Manager := Self.Servant_Manager;
   end Get_Servant_Manager;

   -------------------------
   -- Set_Servant_Manager --
   -------------------------

   procedure Set_Servant_Manager
     (Self    : access Obj_Adapter;
      Manager :        ServantManager_Access;
      Error   : in out PolyORB.Errors.Error_Container) is
   begin
      Ensure_Servant_Manager
        (Self.Request_Processing_Policy.all,
         Error);

      if Found (Error) then
         return;
      end if;

      Ensure_Servant_Manager_Type
        (Self.Servant_Retention_Policy.all,
         Manager.all,
         Error);

      Self.Servant_Manager := Manager;
   end Set_Servant_Manager;

   ----------------------
   -- Get_The_Children --
   ----------------------

   procedure Get_The_Children
     (Self     : access Obj_Adapter;
      Children :    out POAList)
   is
      use PolyORB.POA_Types.POA_HTables;
   begin
      pragma Debug (C, O ("Get_The_Children: enter"));

      if Self.Children /= null
        and then not Is_Empty (Self.Children.all)
      then
         PolyORB.Tasking.Mutexes.Enter (Self.Children_Lock);

         pragma Debug (C, O ("Iterate over existing children"));
         declare
            It : Iterator := First (Self.Children.all);
         begin
            while not Last (It) loop
               POA_Lists.Append (Children, Value (It));
               Next (It);
            end loop;
         end;

         PolyORB.Tasking.Mutexes.Leave (Self.Children_Lock);
      end if;

      pragma Debug (C, O ("Get_The_Children: end"));
   end Get_The_Children;

   ----------------------
   -- Copy_Obj_Adapter --
   ----------------------

   procedure Copy_Obj_Adapter
     (From : Obj_Adapter;
      To   : access Obj_Adapter) is
   begin
      Enter (From.POA_Lock);
      Enter (To.POA_Lock);

      To.Name                       := From.Name;
      To.POA_Manager                := From.POA_Manager;
      To.Boot_Time                  := From.Boot_Time;
      To.Absolute_Address           := From.Absolute_Address;
      To.Active_Object_Map          := From.Active_Object_Map;
      To.Thread_Policy              := From.Thread_Policy;
      To.Request_Processing_Policy  := From.Request_Processing_Policy;
      To.Id_Assignment_Policy       := From.Id_Assignment_Policy;
      To.Id_Uniqueness_Policy       := From.Id_Uniqueness_Policy;
      To.Servant_Retention_Policy   := From.Servant_Retention_Policy;
      To.Lifespan_Policy            := From.Lifespan_Policy;
      To.Implicit_Activation_Policy := From.Implicit_Activation_Policy;
      To.Father                     := From.Father;
      To.Children                   := From.Children;
      To.Children_Lock              := From.Children_Lock;
      To.Map_Lock                   := From.Map_Lock;

      Leave (From.POA_Lock);
      Leave (To.POA_Lock);
   end Copy_Obj_Adapter;

   ------------------------
   -- Remove_POA_By_Name --
   ------------------------

   procedure Remove_POA_By_Name
     (Self       : access Obj_Adapter;
      Child_Name :        Standard.String) is
   begin
      pragma Debug (C, O (Self.Name.all
                       & ": removing POA with name "
                       & Child_Name
                       & " from my children."));

      PolyORB.POA_Types.POA_HTables.Delete (Self.Children.all, Child_Name);
   end Remove_POA_By_Name;

   --------------------------------------------------
   -- PolyORB Obj_Adapter interface implementation --
   --------------------------------------------------

   ------------
   -- Create --
   ------------

   overriding procedure Create (OA : access Obj_Adapter) is
   begin
      Create_Root_POA (OA);
   end Create;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (OA : access Obj_Adapter) is
   begin
      Destroy (OA, Etherealize_Objects => True, Wait_For_Completion => True);
      Obj_Adapters.Destroy (Obj_Adapters.Obj_Adapter (OA.all)'Access);
   end Destroy;

   ------------
   -- Export --
   ------------

   overriding procedure Export
     (OA    : access Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container) is
   begin
      --  NOTE: Per construction, this procedure has the same semantics as
      --  Servant_To_Ref CORBA procedure.

      --  First find out whether we have retained a previous
      --  association for this servant.

      --  NOTE: Per construction, we can retain an Id iff we are using
      --  UNIQUE Id_Uniqueness policy and RETAIN Servant_Retention
      --  policy. Thus, a non null Oid implies we are using this two
      --  policies. There is no need to test them.

      --  XXX complete explanation

      Oid := Retained_Servant_To_Id
        (Self      => OA.Servant_Retention_Policy.all,
         OA        => POA_Types.Obj_Adapter_Access (OA),
         P_Servant => Obj);

      if Oid /= null then
         return;
      end if;

      Implicit_Activate_Servant
        (Self      => OA.Implicit_Activation_Policy.all,
         OA        => POA_Types.Obj_Adapter_Access (OA),
         P_Servant => Obj,
         Hint      => Key,
         Oid       => Oid,
         Error     => Error);
   end Export;

   --------------
   -- Unexport --
   --------------

   overriding procedure Unexport
     (OA    : access Obj_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Errors.Error_Container) is
   begin
      Deactivate_Object (OA, Id.all, Error);
   end Unexport;

   ----------------
   -- Object_Key --
   ----------------

   overriding procedure Object_Key
     (OA      : access Obj_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Errors.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

      U_Oid : Unmarshalled_Oid;
   begin
      Oid_To_U_Oid (Id.all, U_Oid, Error);
      if Found (Error) then
         return;
      end if;

      if U_Oid.System_Generated then
         Throw (Error,
                Invalid_Object_Id_E,
                Null_Members'(Null_Member));
      else
         User_Id := new Objects.Object_Id'
           (Objects.Hex_String_To_Oid (To_Standard_String (U_Oid.Id)));
      end if;
   end Object_Key;

   -------------
   -- Get_QoS --
   -------------

   overriding procedure Get_QoS
     (OA    : access Obj_Adapter;
      Id    :        Objects.Object_Id;
      QoS   :    out PolyORB.QoS.QoS_Parameters;
      Error : in out PolyORB.Errors.Error_Container)
   is
      Obj_OA : PolyORB.POA.Obj_Adapter_Access;

   begin
      Find_POA (OA, Get_Creator (Id), True, Obj_OA, Error);

      if Found (Error) then
         return;
      end if;

      QoS := PolyORB.Obj_Adapter_QoS.Get_Object_Adapter_QoS (Obj_OA);
   end Get_QoS;

   ------------------------
   -- Get_Empty_Arg_List --
   ------------------------

   overriding function Get_Empty_Arg_List
     (OA     : access Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String) return Any.NVList.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA, Oid, Method);
      pragma Warnings (On);
--        S : Servants.Servant_Access;
      Nil_Result : Any.NVList.Ref;
   begin
--        pragma Debug (C, O ("Get_Empty_Arg_List for Id "
--                         & Objects.To_String (Oid.all)));
--        S := Servants.Servant_Access (Find_Servant (OA, Oid, NO_CHECK));

--        if S.If_Desc.PP_Desc /= null then
--           return S.If_Desc.PP_Desc (Method);
--        else
      return Nil_Result;
--           --  If If_Desc is null (eg in the case of an actual
--           --  use of the DSI, where no generated code is used on
--           --  the server side, another means of determining the
--           --  signature must be used, eg a query to an
--           --  Interface repository. Here we only return a Nil
--           --  NVList.Ref, indicating to the Protocol layer
--           --  that arguments unmarshalling is to be deferred
--           --  until the request processing in the Application
--           --  layer is started (at which time the Application
--           --  layer can provide more information as to the
--           --  signature of the called method).
--        end if;

      --  XXX Actually, the code above shows that the generic
      --  POA implementation does not manage a per-servant
      --  If_Desc at all. If such functionality is desired,
      --  it should be implemented as an annotation on the
      --  generic Servants.Servant type (or else
      --  the generic servant type could contain a
      --  If_Descriptors.If_Descriptor_Access, where
      --  applicable.
   end Get_Empty_Arg_List;

   ----------------------
   -- Get_Empty_Result --
   ----------------------

   overriding function Get_Empty_Result
     (OA     : access Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String) return Any.Any
   is
      --  S : Servants.Servant_Access;
   begin
--        pragma Debug (C, O ("Get_Empty_Result for Id "
--                         & Objects.To_String (Oid.all)));
--        S := Servants.Servant_Access (Find_Servant (OA, Oid, NO_CHECK));
--        if S.If_Desc.RP_Desc /= null then
--           return S.If_Desc.RP_Desc (Method);
--        end if;
      raise Program_Error;
      pragma Warnings (Off);
      return Get_Empty_Result (OA, Oid, Method);
      pragma Warnings (On);
      --  Cf. comment above
   end Get_Empty_Result;

   ------------------
   -- Find_Servant --
   ------------------

   overriding procedure Find_Servant
     (OA      : access Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Errors.Error_Container) is
   begin
      Find_Servant (OA, Id, True, Servant, Error);
   end Find_Servant;

   procedure Find_Servant
     (OA       : access Obj_Adapter;
      Id       : access Objects.Object_Id;
      Do_Check :        Boolean;
      Servant  :    out Servants.Servant_Access;
      Error    : in out PolyORB.Errors.Error_Container)
   is
      use type PolyORB.Servants.Servant_Access;

      Obj_OA : Obj_Adapter_Access;
   begin
      pragma Debug (C, O ("Find_Servant: enter"));

      Find_POA (OA,
                Get_Creator (Id.all),
                True,
                Obj_OA,
                Error);

      if Found (Error) then
         pragma Debug (C, O ("Find_Servant: leave (error)"));
         return;
      end if;

      if Obj_OA = null then
         Throw (Error,
                Object_Not_Exist_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));
         pragma Debug (C, O ("Find_Servant: leave (no OA)"));
         return;
      end if;

      Enter (Obj_OA.POA_Lock);

      --  Check POA Manager state

      if Do_Check then
         case Get_State (POA_Manager_Of (Obj_OA).all) is
            when DISCARDING | INACTIVE =>

               --  XXX Do we have to do something special for INACTIVE ???

               Throw (Error,
                      Transient_E,
                      System_Exception_Members'(Minor => 0,
                                                Completed => Completed_No));
               Leave (Obj_OA.POA_Lock);
               pragma Debug (C, O ("Find_Servant: leave (OA not active)"));
               return;

            when HOLDING =>
               Servant := Get_Hold_Servant
                 (POA_Manager_Of (Obj_OA),
                  POA_Types.Obj_Adapter_Access (Obj_OA));
               Servants.Set_Executor
                 (Servant,
                  Executor (Obj_OA.Thread_Policy));
               Leave (Obj_OA.POA_Lock);
               pragma Debug (C, O ("Find_Servant: leave (OA holding)"));
               return;

            when others =>
               null;
         end case;
      end if;

      --  Find servant

      pragma Debug
        (C, O ("Find_Servant: querying " & Obj_OA.Name.all
            & " for servant with id "
            & Objects.Oid_To_Hex_String (Id.all)));

      Id_To_Servant (Obj_OA,
                     Id.all,
                     Servant,
                     Error);

      Leave (Obj_OA.POA_Lock);

      if Found (Error) then
         if Error.Kind = ObjectNotActive_E then
            --  When POA is configured to ACTIVE_OBJECT_MAP_ONLY of Request
            --  Processing Policy, reported error may be ObjectNotActive_E
            --  for compatibility with PortableServer::POA::id_to_servant
            --  behavior. This error need to be translated to system's
            --  Object_Not_Exists_E error, thus cleanup error state and
            --  continue execution, error state will be set at the end of
            --  the subprogram.

            Catch (Error);

         else
            pragma Debug (C, O ("Find_Servant: leave (error)"));
            return;
         end if;
      end if;

      --  Servant not found, we try to activate one, if POA policies allow it

      if Servant = null
        and then Obj_OA.Servant_Manager /= null
        and then Obj_OA.Servant_Manager.all in ServantActivator'Class
      then
         pragma Debug (C, O ("Find_Servant: activating servant"));

         declare
            Activator : aliased ServantActivator'Class :=
              ServantActivator (Obj_OA.Servant_Manager.all);
            Oid       : Objects.Object_Id_Access;

         begin
            Object_Identifier
              (Obj_OA.Id_Assignment_Policy.all,
               Objects.Object_Id_Access (Id),
               Oid,
               Error);

            if Found (Error) then
               pragma Debug (C, O ("Find_Servant: leave (error)"));
               return;
            end if;

            Incarnate
              (Activator'Access,
               Oid.all,
               Obj_OA,
               Servant,
               Error);
            Free (Oid);

            if Found (Error) then
               pragma Assert (Error.Kind = ForwardRequest_E);
               pragma Debug (C, O ("Find_Servant: leave"));
               return;
            end if;

         end;
      end if;

      if Servant = null then
         Throw (Error,
                Object_Not_Exist_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));
         pragma Debug (C, O ("Find_Servant: leave (null servant)"));
         return;
      end if;
      Servants.Set_Executor (Servant, Executor (OA.Thread_Policy));
      pragma Debug (C, O ("Find_Servant: leave"));
   end Find_Servant;

   ---------------------
   -- Release_Servant --
   ---------------------

   overriding procedure Release_Servant
     (OA      : access Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant : in out Servants.Servant_Access)
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Unreferenced (Id);
      pragma Unreferenced (Servant);
      pragma Warnings (On);

   begin
      null;
      --  XXX if servant has been created on the fly, should
      --  destroy it now (else do nothing).
   end Release_Servant;

end PolyORB.POA;
