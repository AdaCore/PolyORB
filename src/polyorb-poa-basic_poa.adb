------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . P O A . B A S I C _ P O A                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2003 Free Software Foundation, Inc.           --
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

--  Basic POA implementation.

--  XXX double check concurrent accesses for this implementation !!!
--  e.g. copy obj_adapter, find_poa ...

--  $Id$

with Ada.Streams;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with PolyORB.Exceptions;
with PolyORB.Log;
with PolyORB.Objects;
with PolyORB.POA_Config;
with PolyORB.POA_Manager.Basic_Manager;
with PolyORB.POA_Types;
with PolyORB.References.IOR;
with PolyORB.Smart_Pointers;
with PolyORB.Types;
with PolyORB.Utils;

package body PolyORB.POA.Basic_POA is

   use PolyORB.Exceptions;
   use PolyORB.Log;
   use PolyORB.POA_Manager;
   use PolyORB.POA_Manager.Basic_Manager;
   use PolyORB.POA_Policies;
   use PolyORB.POA_Types;
   use PolyORB.Tasking.Mutexes;
   use PolyORB.Types;

   package L is new Log.Facility_Log ("polyorb.poa.basic_poa");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   POA_Path_Separator : constant Character := '/';

   --------------------------------------------------------
   -- Declaration of additional procedures and functions --
   --------------------------------------------------------

   function Get_Boot_Time
     return Time_Stamp;

   procedure Init_With_User_Policies
     (OA       : access Basic_Obj_Adapter;
      Policies :        POA_Policies.PolicyList);
   --  Initialize OA with a set of policies provided by the user.

   procedure Init_With_Default_Policies
     (OA : access Basic_Obj_Adapter);
   --  Initialize OA with a default set of policies provided by
   --  the currently active POA configuration.

   procedure Check_Policies_Compatibility
     (OA    :        Basic_Obj_Adapter_Access;
      Error : in out PolyORB.Exceptions.Error_Container);

   procedure Register_Child
     (Self  : access Basic_Obj_Adapter;
      Child :        Basic_Obj_Adapter_Access);
   --  Add a child to the current POA.
   --  This procedure should be called from a point that protects the
   --  Self's Children List.

   procedure Destroy_Policies
     (OA : in out Basic_Obj_Adapter);
   pragma Warnings (Off);
   pragma Unreferenced (Destroy_Policies);
   pragma Warnings (On);
   --  Destroys OA's policies.

   procedure Destroy_OA
     (OA : in out Basic_Obj_Adapter_Access);
   --  Destroy OA's components.

   procedure Create_Root_POA
     (New_Obj_Adapter : access Basic_Obj_Adapter);
   --  Create the Root of all POAs.

   procedure Find_Servant
     (OA       : access Basic_Obj_Adapter;
      Id       : access Objects.Object_Id;
      Do_Check :        Boolean;
      Servant  :    out Servants.Servant_Access;
      Error    : in out PolyORB.Exceptions.Error_Container);
   --  The Find_Servant from PolyORB, plus a parameter.
   --  If Do_Check is True, then the POA checks the state of its POA
   --  Manager.

   procedure Set_Policies
     (OA       : access Basic_Obj_Adapter;
      Policies : POA_Policies.PolicyList;
      Default  : Boolean);
   --  Set OA policies from the values in Policies.
   --  If Default is True, set only those policies that
   --  are not yet explicitly set in OA. If Default is False,
   --  set all policies, and warn for duplicates.

   function POA_Manager_Of
     (OA : access Basic_Obj_Adapter)
     return POA_Manager.POAManager_Access;
   --  Return the POA Manager associated to 'OA'.

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
     (OA : access Basic_Obj_Adapter)
     return POA_Manager.POAManager_Access
   is
      use Smart_Pointers;

      E : constant Entity_Ptr := Entity_Of (OA.POA_Manager);
   begin
      pragma Assert (E.all in POA_Manager.POAManager'Class);

      return POAManager_Access (E);
   end POA_Manager_Of;

   -------------------
   -- Get_Boot_Time --
   -------------------

   function Get_Boot_Time
     return Time_Stamp is
   begin
      return Time_Stamp (16#0deadc0d#);
      --  XXX should compute a real time stamp! But:
      --  Cannot depend on Ada.Real_Time (which pulls the tasking runtime)
      --  Cannot depend on Ada.Calendar (not permitted by Ravenscar).
   end Get_Boot_Time;

   ------------------
   -- Set_Policies --
   ------------------

   procedure Set_Policies
     (OA       : access Basic_Obj_Adapter;
      Policies :        POA_Policies.PolicyList;
      Default  :        Boolean)
   is
      use Policy_Sequences;

      Policies_Array : constant Element_Array := To_Element_Array (Policies);
      A_Policy : Policy_Access;
   begin
      Enter (OA.POA_Lock);

      for J in Policies_Array'Range loop
         A_Policy := Policies_Array (J);

         if A_Policy.all in ThreadPolicy'Class then
            if OA.Thread_Policy = null or else not Default then
               if OA.Thread_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in ThreadPolicy: using last one"));
                  null;
               end if;
               OA.Thread_Policy := ThreadPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in LifespanPolicy'Class then
            if OA.Lifespan_Policy = null or else not Default then
               if OA.Lifespan_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in LifespanPolicy: using last one"));
                  null;
               end if;
               OA.Lifespan_Policy := LifespanPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in IdUniquenessPolicy'Class then
            if OA.Id_Uniqueness_Policy = null or else not Default then
               if OA.Id_Uniqueness_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in IdUniquenessPolicy: using last one"));
                  null;
               end if;
               OA.Id_Uniqueness_Policy := IdUniquenessPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in IdAssignmentPolicy'Class then
            if OA.Id_Assignment_Policy = null or else not Default then
               if OA.Id_Assignment_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in IdAssignmentPolicy: using last one"));
                  null;
               end if;
               OA.Id_Assignment_Policy := IdAssignmentPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in ServantRetentionPolicy'Class then
            if OA.Servant_Retention_Policy = null or else not Default then
               if OA.Servant_Retention_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in ServantRetentionPolicy:"
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
                    (O ("Duplicate in RequestProcessingPolicy:"
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
                    (O ("Duplicate in ImplicitActivationPolicy:"
                        & "using last one"));
                  null;
               end if;
               OA.Implicit_Activation_Policy :=
                 ImplicitActivationPolicy_Access (A_Policy);
            end if;

         else
            null;
            pragma Debug (O ("Unknown policy ignored"));
         end if;
      end loop;

      Leave (OA.POA_Lock);
   end Set_Policies;

   -----------------------------
   -- Init_With_User_Policies --
   -----------------------------

   procedure Init_With_User_Policies
     (OA       : access Basic_Obj_Adapter;
      Policies :        POA_Policies.PolicyList) is
   begin
      pragma Debug (O ("Init POA with user provided policies"));

      Set_Policies (OA, Policies, Default => False);
   end Init_With_User_Policies;

   --------------------------------
   -- Init_With_Default_Policies --
   --------------------------------

   procedure Init_With_Default_Policies
     (OA : access Basic_Obj_Adapter) is
   begin
      pragma Debug (O ("Init POA with default policies"));

      Set_Policies
        (OA,
         POA_Config.Default_Policies (POA_Config.Configuration.all),
         Default => True);
   end Init_With_Default_Policies;

   ----------------------------------
   -- Check_Policies_Compatibility --
   ----------------------------------

   procedure Check_Policies_Compatibility
     (OA    :        Basic_Obj_Adapter_Access;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      OA_Policies : AllPolicies;

   begin
      pragma Debug (O ("Check compatibilities between policies: enter"));
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
      pragma Debug (O ("Check compatibilities between policies: leave"));
   end Check_Policies_Compatibility;

   --------------------
   -- Register_Child --
   --------------------

   procedure Register_Child
     (Self  : access Basic_Obj_Adapter;
      Child :        Basic_Obj_Adapter_Access)
   is
      use PolyORB.POA_Types.POA_HTables;

   begin
      pragma Debug (O (To_Standard_String (Self.Name)
                       & " registers child "
                       & To_Standard_String (Child.Name)));

      if Self.Children = null then
         Self.Children := new POATable;
         Initialize (Self.Children.all);
      end if;

      Insert (Self.Children.all,
              To_Standard_String (Child.Name),
              POA_Types.Obj_Adapter_Access (Child));

   end Register_Child;

   ----------------------
   -- Destroy_Policies --
   ----------------------

   procedure Destroy_Policies
     (OA : in out Basic_Obj_Adapter)
   is
      procedure Free is new Ada.Unchecked_Deallocation
        (Policy'Class, Policy_Access);
   begin
      Free (Policy_Access (OA.Thread_Policy));
      Free (Policy_Access (OA.Id_Uniqueness_Policy));
      Free (Policy_Access (OA.Id_Assignment_Policy));
      Free (Policy_Access (OA.Implicit_Activation_Policy));
      Free (Policy_Access (OA.Lifespan_Policy));
      Free (Policy_Access (OA.Request_Processing_Policy));
      Free (Policy_Access (OA.Servant_Retention_Policy));
   end Destroy_Policies;

   ----------------
   -- Destroy_OA --
   ----------------

   procedure Destroy_OA
     (OA : in out Basic_Obj_Adapter_Access)
   is
      use PolyORB.Object_Maps;

      procedure Free is new Ada.Unchecked_Deallocation
        (Object_Map'Class, Object_Map_Access);

   begin
      pragma Debug (O ("Destroy_OA: enter"));

      if not Is_Nil (OA.POA_Manager) then
         Remove_POA
           (POA_Manager_Of (OA),
            POA_Types.Obj_Adapter_Access (OA));
         Set (OA.POA_Manager, null);
      end if;

      --  Destroy_Policies (OA.all);
      --  XXX Cannot destroy_policies here because another
      --  POA initialised from the default configuration could
      --  be using the same instances of policy objects!

      --  XXX if so why don't we make policies a derived type from a
      --  Smart Pointer ??

      --  Destroy Locks.

      --  As Destroy_OA may be called when an exception is raised in
      --  OA initialization, we first test the call is valid.

      if OA.POA_Lock /= null then
         Destroy (OA.POA_Lock);
      end if;

      if OA.Children_Lock /= null then
         Destroy (OA.Children_Lock);
      end if;

      if OA.Map_Lock /= null then
         Destroy (OA.Map_Lock);
      end if;

      --  These members may be null, test before freeing

      if OA.Active_Object_Map /= null then
         PolyORB.Object_Maps.Finalize (OA.Active_Object_Map.all);
         Free (OA.Active_Object_Map);
      end if;

      if OA.Adapter_Activator /= null then
         Free (OA.Adapter_Activator);
      end if;

      if OA.Servant_Manager /= null then
         Free (OA.Servant_Manager);
      end if;

      --  Basic_Obj_Adapter is derived from Smart Pointers
      --  so there is no need to deallocate the OA itself.
      --  XXX is it pertinent to have this ???

      pragma Debug (O ("Destroy_OA: end"));
   end Destroy_OA;

   ---------------------
   -- Create_Root_POA --
   ---------------------

   procedure Create_Root_POA
     (New_Obj_Adapter : access Basic_Obj_Adapter)
   is
      use PolyORB.POA_Types.POA_HTables;

   begin
      pragma Debug (O ("Create Root_POA"));

      --  Create new Obj Adapter.

      New_Obj_Adapter.Boot_Time        := Get_Boot_Time;
      New_Obj_Adapter.Name             := To_PolyORB_String ("RootPOA");
      New_Obj_Adapter.Absolute_Address := To_PolyORB_String ("");
      Create (New_Obj_Adapter.POA_Lock);
      Create (New_Obj_Adapter.Children_Lock);
      Create (New_Obj_Adapter.Map_Lock);

      --  Attach a POA Manager to the root POA.

      Set (New_Obj_Adapter.POA_Manager, new Basic_POA_Manager);
      Create (POA_Manager_Of (New_Obj_Adapter));
      Register_POA
        (POA_Manager_Of (New_Obj_Adapter),
         POA_Types.Obj_Adapter_Access (New_Obj_Adapter));

      --  Create and initialize policies factory

      POA_Config.Initialize (POA_Config.Configuration.all);
      --  XXX is this really the role of Create_Root_POA to initialize this ???

      --  Use default policies.

      Init_With_Default_Policies (New_Obj_Adapter);

      --  Initialize Global POA Table

      Initialize (Global_POATable);
   end Create_Root_POA;

   ---------------------------------------------
   -- CORBA-like POA interface implementation --
   ---------------------------------------------

   ----------------
   -- Create_POA --
   ----------------

   procedure Create_POA
     (Self         : access Basic_Obj_Adapter;
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        POA_Policies.PolicyList;
      POA          :    out Obj_Adapter_Access;
      Error        : in out PolyORB.Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;
      use PolyORB.POA_Types.POA_HTables;

      New_Obj_Adapter : Basic_Obj_Adapter_Access;

   begin
      pragma Debug (O ("Creating POA: " & To_String (Adapter_Name)));

      --  Validity checks on Adapter_Name

      if Adapter_Name = ""
        or else Index (Adapter_Name, (1 => POA_Path_Separator)) /= 0
      then
         Throw (Error,
                WrongAdapter_E,
                Null_Members'(Null_Member));
         --  XXX Check  error name
         return;
      end if;

      --  Look if there is already a child with this name

      Enter (Self.Children_Lock);

      if Self.Children /= null then
         pragma Debug (O ("Check if a POA with the same name exists."));

         if Lookup (Self.Children.all,
                    To_Standard_String (Adapter_Name), null) /= null then
            Throw (Error,
                   AdapterAlreadyExists_E,
                   Null_Members'(Null_Member));

            Leave (Self.Children_Lock);
            return;
         end if;
      end if;

      --  Create new object adapter

      New_Obj_Adapter           := new Basic_Obj_Adapter;
      Create (New_Obj_Adapter.POA_Lock);
      Create (New_Obj_Adapter.Children_Lock);
      Create (New_Obj_Adapter.Map_Lock);
      New_Obj_Adapter.Boot_Time := Get_Boot_Time;
      New_Obj_Adapter.Father    := POA_Types.Obj_Adapter_Access (Self);
      New_Obj_Adapter.Name      := Adapter_Name;

      if A_POAManager = null then
         Set (New_Obj_Adapter.POA_Manager, new Basic_POA_Manager);
         Create (POA_Manager_Of (New_Obj_Adapter));
         Register_POA (POA_Manager_Of (New_Obj_Adapter),
                       POA_Types.Obj_Adapter_Access (New_Obj_Adapter));
      else
         Set (New_Obj_Adapter.POA_Manager,
              Smart_Pointers.Entity_Ptr (A_POAManager));
         Register_POA (A_POAManager,
                       POA_Types.Obj_Adapter_Access (New_Obj_Adapter));
      end if;

      --  NOTE: New_Obj_Adapter.Children is initialized iff we
      --  need it, see the procedure Register_Child for more details.

      --  Register new obj_adapter as a sibling of the current POA.

      Register_Child (Self, New_Obj_Adapter);
      Leave (Self.Children_Lock);

      --  Construct POA Absolute name.

      if Length (Self.Absolute_Address) > 0 then
         New_Obj_Adapter.Absolute_Address := Self.Absolute_Address
           & To_PolyORB_String ((1 => POA_Path_Separator)) & Adapter_Name;
      else
         New_Obj_Adapter.Absolute_Address :=
           Self.Absolute_Address & Adapter_Name;

      end if;

      pragma Debug
        (O ("Absolute name of new POA is "
            & To_Standard_String (New_Obj_Adapter.Absolute_Address)));

      --  First initialize POA with default policies.

      Init_With_Default_Policies (New_Obj_Adapter);

      --  then override POA policies with those given by the user.

      Init_With_User_Policies (New_Obj_Adapter, Policies);

      --  Check compatibilities between policies.

      Check_Policies_Compatibility (New_Obj_Adapter, Error);

      if Found (Error) then
         pragma Debug (O ("Got Error, destroying POA"));
         Destroy (New_Obj_Adapter, False, False);
         return;
      end if;

      --  Insert POA into Global_POATable

      pragma Debug (O ("Insert POA into Global_POATable"));
      Insert (Global_POATable,
              POA_Path_Separator
              & To_Standard_String (New_Obj_Adapter.Absolute_Address),
              POA_Types.Obj_Adapter_Access (New_Obj_Adapter));

      --  Return the created POA.

      pragma Debug (O ("POA " & To_String (Adapter_Name) & " created."));
      POA := Obj_Adapter_Access (New_Obj_Adapter);

   end Create_POA;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self                : access Basic_Obj_Adapter;
      Etherealize_Objects : in     Types.Boolean;
      Wait_For_Completion : in     Types.Boolean)
   is
      use PolyORB.POA_Types.POA_HTables;

      Name : constant String := To_Standard_String (Self.Name);
   begin
      pragma Debug (O ("Start destroying POA: " & Name));

      --  Remove Self from Global POA Table

      pragma Debug (O ("Removing POA from Global POA Table"));
      PolyORB.POA_Types.POA_HTables.Delete
        (Global_POATable,
         To_Standard_String (Self.Absolute_Address));

      --  Destroy all children

      if Self.Children /= null
        and then not Is_Empty (Self.Children.all) then
         Enter (Self.Children_Lock);

         declare
            It : Iterator := First (Self.Children.all);
            A_Child : Basic_Obj_Adapter_Access;
         begin
            while not Last (It) loop
               A_Child := Basic_Obj_Adapter (Value (It).all)'Access;

               Destroy (A_Child,
                        Etherealize_Objects,
                        Wait_For_Completion);

               --  NOTE: there is no need to delete A_Child from
               --  Self.Children, this will be done when finalizing it.

               Next (It);
            end loop;

            Finalize (Self.Children.all);
            Free (Self.Children);

            Leave (Self.Children_Lock);

         exception
            when others =>
               pragma Debug (O ("Got exception when destroying Child POA"));
               Leave (Self.Children_Lock);
               raise;
         end;
      end if;

      --  Tell father to remove current POA from its list of children

      if Self.Father /= null then
         pragma Debug (O ("Notify parent POA of POA destruction"));

         POA.Remove_POA_By_Name
           (POA.Obj_Adapter_Access (Self.Father),
            Self.Name);
      end if;

      --  Destroy self (also unregister from the POAManager)

      pragma Debug (O ("About to destroy POA"));

      --  Destroy POA components.

      declare
         OA : Basic_Obj_Adapter_Access := Basic_Obj_Adapter_Access (Self);
      begin
         Destroy_OA (OA);
      end;

      pragma Debug (O ("POA '" & Name & "' destroyed"));

      --  XXX Add code for Etherealize_Objects and Wait_For_Completion ???

   end Destroy;

   ----------------------------------
   -- Create_Object_Identification --
   ----------------------------------

   procedure Create_Object_Identification
     (Self  : access Basic_Obj_Adapter;
      Hint  :        Object_Id_Access;
      U_Oid :    out Unmarshalled_Oid;
      Error : in out PolyORB.Exceptions.Error_Container) is
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
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servants.Servant_Access;
      Hint      :        Object_Id_Access;
      U_Oid     :    out Unmarshalled_Oid;
      Error     : in out PolyORB.Exceptions.Error_Container) is
   begin
      pragma Debug (O ("Activate_Object: enter"));

      --  Build a well formed Oid from the 'Hint' provided by the user.

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

      pragma Debug (O ("Activate_Object: leave"));
   end Activate_Object;

   -----------------------
   -- Deactivate_Object --
   -----------------------

   procedure Deactivate_Object
     (Self  : access Basic_Obj_Adapter;
      Oid   : in     Object_Id;
      Error : in out PolyORB.Exceptions.Error_Container)
   is
      U_Oid : Unmarshalled_Oid;
   begin
      pragma Debug (O ("Deactivate_Object: enter"));

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
         pragma Debug (O ("Call POA Servant Manager's etherealize"));

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
               return;
            end if;

            Etherealize
              (Activator'Access,
               Oid,
               Self,
               Servant,
               Cleanup_In_Progress => True,
               Remaining_Activations => True
               );
            --  XXX should compute Remaining_Activations value ..
         end;
      end if;

      Etherealize_All
        (Self.Request_Processing_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         U_Oid);

      Forget_Servant_Association
        (Self.Servant_Retention_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         U_Oid,
         Error);

      if Found (Error) then
         return;
      end if;

      --  XXX ??? Wait for completion?

      pragma Debug (O ("Deactivate_Object: leave"));
   end Deactivate_Object;

   -------------------
   -- Servant_To_Id --
   -------------------

   procedure Servant_To_Id
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servants.Servant_Access;
      Oid       :    out Object_Id_Access;
      Error     : in out PolyORB.Exceptions.Error_Container)
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

         --  XXX here should also check whether we are in the
         --  context of executing a dispatched operation on
         --  Servant, and if it is the case return the 'current'
         --  oid (for USE_DEFAULT_SERVANT policy).
      end if;

      Object_Identifier
        (Self.Id_Assignment_Policy.all,
         Temp_Oid2,
         Oid);

   end Servant_To_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   procedure Id_To_Servant
     (Self    : access Basic_Obj_Adapter;
      Oid     :        Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
      A_Oid : aliased Object_Id := Oid;

      U_Oid : Unmarshalled_Oid := Oid_To_U_Oid (A_Oid'Access);

   begin
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
     (Self        : access Basic_Obj_Adapter;
      Name        :        String;
      Activate_It :        Boolean;
      POA         :    out Obj_Adapter_Access;
      Error       : in out PolyORB.Exceptions.Error_Container)
   is
      use PolyORB.Exceptions;
      use PolyORB.POA_Types.POA_HTables;

      --------------------------
      -- Find_POA_Recursively --
      --------------------------

      procedure Find_POA_Recursively
        (Self        : access Basic_Obj_Adapter;
         Name        :        String;
         Activate_It :        Boolean;
         POA         :    out Obj_Adapter_Access;
         Error       : in out PolyORB.Exceptions.Error_Container);
      --  Looks for 'name', searching from 'Self', using a recursive search.
      --  If necessary, will invoke AdapterActivator call backs.

      procedure Find_POA_Recursively
        (Self        : access Basic_Obj_Adapter;
         Name        :        String;
         Activate_It :        Boolean;
         POA         :    out Obj_Adapter_Access;
         Error       : in out PolyORB.Exceptions.Error_Container)
      is
         A_Child     : Obj_Adapter_Access;
         Result      : Boolean;
         Split_Point : constant Integer :=
           PolyORB.Utils.Find (Name, Name'First, POA_Path_Separator);

      begin
         if Found (Error) then
            return;
         end if;

         pragma Debug (O ("Find_POA_Recursively: enter, Name = " & Name));

         --  Name is null => return Self

         if Name'Length = 0 then
            POA := Obj_Adapter_Access (Self);
            return;
         end if;

         pragma Assert (Split_Point /= Name'First);

         --  Check Self's children

         if Self.Children /= null then
            A_Child := PolyORB.POA.Obj_Adapter_Access
              (Lookup (Self.Children.all,
                       Name (Name'First .. Split_Point - 1), null));
         end if;

         if A_Child /= null then

            --  A child corresponds to partial name, follow search

            Find_POA
              (Basic_Obj_Adapter (A_Child.all)'Access,
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
           To_Standard_String (Self.Absolute_Address)
           & POA_Path_Separator
           & Name;

      begin
         pragma Debug (O ("Find_POA: enter, Name = "
                          & Full_POA_Name));

         --  XXX: Suppressed lookup in Global POA HTable, as of
         --  2003/09/05, Global POA HTable is not updated when CORBA's POA
         --  finalizes a reference on a POA.

--         POA := PolyORB.POA.Obj_Adapter_Access
--           (Lookup
--            (Global_POATable,
--             Full_POA_Name,
--             null));

         POA := null;

         if POA /= null then
            pragma Debug (O ("Found POA in Global_POATable"));
            return;
         end if;
      end;

      --  Then make a recursive look up, activating POA if necessary.

      pragma Debug (O ("Looking for " & Name & " recursively"));

      Find_POA_Recursively
        (Self,
         Name,
         Activate_It,
         POA,
         Error);

      pragma Debug (O ("Find_POA: leave"));
   end Find_POA;

   -----------------
   -- Get_Servant --
   -----------------

   procedure Get_Servant
     (Self    : access Basic_Obj_Adapter;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container) is
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
     (Self    : access Basic_Obj_Adapter;
      Servant :        Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container) is
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
     (Self    : access Basic_Obj_Adapter;
      Manager :    out ServantManager_Access;
      Error   : in out PolyORB.Exceptions.Error_Container) is
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
     (Self    : access Basic_Obj_Adapter;
      Manager :        ServantManager_Access;
      Error   : in out PolyORB.Exceptions.Error_Container) is
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
   -- Copy_Obj_Adapter --
   ----------------------

   procedure Copy_Obj_Adapter
     (From : in     Basic_Obj_Adapter;
      To   : access Basic_Obj_Adapter) is
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
     (Self       : access Basic_Obj_Adapter;
      Child_Name :        Types.String) is
   begin
      pragma Debug (O (To_Standard_String (Self.Name)
                       & ": removing POA with name "
                       & To_Standard_String (Child_Name)
                       & " from my children."));

      PolyORB.POA_Types.POA_HTables.Delete (Self.Children.all,
                                            To_Standard_String (Child_Name));

   end Remove_POA_By_Name;

   --------------------------------------------------
   -- PolyORB Obj_Adapter interface implementation --
   --------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create
     (OA : access Basic_Obj_Adapter) is
   begin
      Create_Root_POA (OA);
   end Create;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (OA : access Basic_Obj_Adapter)
   is
      The_OA : constant Basic_Obj_Adapter_Access :=
        Basic_Obj_Adapter_Access (OA);

   begin
      Destroy (The_OA, True, True);
   end Destroy;

   ------------
   -- Export --
   ------------

   procedure Export
     (OA    : access Basic_Obj_Adapter;
      Obj   :        Servants.Servant_Access;
      Key   :        Objects.Object_Id_Access;
      Oid   :    out Objects.Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container) is
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
        (OA.Implicit_Activation_Policy.all,
         POA_Types.Obj_Adapter_Access (OA),
         Obj,
         Key,
         Oid,
         Error);

   end Export;

   --------------
   -- Unexport --
   --------------

   procedure Unexport
     (OA    : access Basic_Obj_Adapter;
      Id    :        Objects.Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container) is
   begin
      Deactivate_Object (OA, Id.all, Error);
   end Unexport;

   ----------------
   -- Object_Key --
   ----------------

   procedure Object_Key
     (OA      : access Basic_Obj_Adapter;
      Id      :        Objects.Object_Id_Access;
      User_Id :    out Objects.Object_Id_Access;
      Error   : in out PolyORB.Exceptions.Error_Container)
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

      U_Oid : constant Unmarshalled_Oid := Oid_To_U_Oid (Id);
   begin
      if U_Oid.System_Generated then
         Throw (Error,
                Invalid_Object_Id_E,
                Null_Members'(Null_Member));
      else
         User_Id := new Objects.Object_Id'
           (Objects.To_Oid (To_Standard_String (U_Oid.Id)));
      end if;
   end Object_Key;

   ------------------------
   -- Get_Empty_Arg_List --
   ------------------------

   function Get_Empty_Arg_List
     (OA     : access Basic_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.NVList.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA, Oid, Method);
      pragma Warnings (On);
--        S : Servants.Servant_Access;
      Nil_Result : Any.NVList.Ref;
   begin
--        pragma Debug (O ("Get_Empty_Arg_List for Id "
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
      --  Basic_POA implementation does not manage a per-servant
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

   function Get_Empty_Result
     (OA     : access Basic_Obj_Adapter;
      Oid    : access Objects.Object_Id;
      Method :        String)
     return Any.Any
   is
      --  S : Servants.Servant_Access;
   begin
--        pragma Debug (O ("Get_Empty_Result for Id "
--                         & Objects.To_String (Oid.all)));
--        S := Servants.Servant_Access (Find_Servant (OA, Oid, NO_CHECK));
--        if S.If_Desc.RP_Desc /= null then
--           return S.If_Desc.RP_Desc (Method);
--        end if;
      raise Not_Implemented;
      pragma Warnings (Off);
      return Get_Empty_Result (OA, Oid, Method);
      pragma Warnings (On);
      --  Cf. comment above.
   end Get_Empty_Result;

   ------------------
   -- Find_Servant --
   ------------------

   procedure Find_Servant
     (OA      : access Basic_Obj_Adapter;
      Id      : access Objects.Object_Id;
      Servant :    out Servants.Servant_Access;
      Error   : in out PolyORB.Exceptions.Error_Container) is
   begin
      Find_Servant (OA, Id, True, Servant, Error);
   end Find_Servant;

   procedure Find_Servant
     (OA       : access Basic_Obj_Adapter;
      Id       : access Objects.Object_Id;
      Do_Check :        Boolean;
      Servant  :    out Servants.Servant_Access;
      Error    : in out PolyORB.Exceptions.Error_Container)
   is
      use type PolyORB.Servants.Servant_Access;

      U_Oid    : constant Unmarshalled_Oid := Oid_To_U_Oid (Id);

      Obj_OA   : Obj_Adapter_Access;
      Basic_OA : Basic_Obj_Adapter_Access;
   begin
      pragma Debug (O ("Find_Servant: Enter."));

      Find_POA (OA,
                To_Standard_String (U_Oid.Creator),
                True,
                Obj_OA,
                Error);

      if Found (Error) then
         return;
      end if;

      if Obj_OA = null then
         Throw (Error,
                Object_Not_Exist_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));
         return;
      end if;

      Basic_OA := Basic_Obj_Adapter_Access (Obj_OA);

      Enter (Basic_OA.POA_Lock);

      --  Check POA Manager state.

      if Do_Check then
         case Get_State (POA_Manager_Of (Basic_OA).all) is
            when DISCARDING | INACTIVE =>

               --  XXX Do we have to do something special for INACTIVE ???

               Throw (Error,
                      Transient_E,
                      System_Exception_Members'(Minor => 0,
                                                Completed => Completed_No));
               Leave (Basic_OA.POA_Lock);
               return;

            when HOLDING =>
               Servant := Servants.Servant_Access
                 (Get_Hold_Servant
                  (POA_Manager_Of (Basic_OA),
                   POA_Types.Obj_Adapter_Access (Basic_OA)));
               Servants.Set_Thread_Policy (Servant, Basic_OA.Thread_Policy);
               Leave (Basic_OA.POA_Lock);
               return;

            when others =>
               null;
         end case;
      end if;

      --  Find servant.

      pragma Debug
        (O ("OA : " & To_Standard_String (Basic_OA.Name)
            & " looks for servant associated with Id "
            & Objects.To_String (Id.all)));

      Id_To_Servant (Basic_OA,
                     Id.all,
                     Servant,
                     Error);

      Leave (Basic_OA.POA_Lock);

      if Found (Error) then
         return;
      end if;

      --  Servant not found, we try to activate one, if POA policies allow it

      if Servant = null
        and then Basic_OA.Servant_Manager /= null
        and then Basic_OA.Servant_Manager.all in ServantActivator'Class
      then
         declare
            Activator : aliased ServantActivator'Class :=
              ServantActivator (Basic_OA.Servant_Manager.all);
         begin
            Servant := Incarnate
              (Activator'Access,
               Id.all,
               Basic_OA);
         end;
      end if;

      if Servant = null then
         Throw (Error,
                Object_Not_Exist_E,
                System_Exception_Members'(Minor => 0,
                                          Completed => Completed_No));
         return;
      end if;

      Servants.Set_Thread_Policy (Servant, Basic_OA.Thread_Policy);

      pragma Debug (O ("Find_Servant: Leave."));
   end Find_Servant;

   ---------------------
   -- Release_Servant --
   ---------------------

   procedure Release_Servant
     (OA      : access Basic_Obj_Adapter;
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

   --------------------
   -- Set_Proxies_OA --
   --------------------

   procedure Set_Proxies_OA
     (OA         : access Basic_Obj_Adapter;
      Proxies_OA :        Basic_Obj_Adapter_Access) is
   begin
      pragma Assert (OA.Proxies_OA = null
                       and then Proxies_OA /= null);
      OA.Proxies_OA := Proxies_OA;
   end Set_Proxies_OA;

   ------------------
   -- Is_Proxy_Oid --
   ------------------

   function Is_Proxy_Oid
     (OA  : access Basic_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return Boolean is
   begin
      if OA.Proxies_OA = null then
         return False;
      end if;

      declare
         U_Oid : constant Unmarshalled_Oid := Oid_To_U_Oid (Oid);

         Obj_OA : Obj_Adapter_Access;
         Error  : PolyORB.Exceptions.Error_Container;

      begin
         Find_POA (OA,
                   To_Standard_String (U_Oid.Creator),
                   False,
                   Obj_OA,
                   Error);

         if Found (Error) then
            Catch (Error);
         end if;

         return Basic_Obj_Adapter_Access (Obj_OA) = OA.Proxies_OA;
      end;
   end Is_Proxy_Oid;

   ------------------
   -- To_Proxy_Oid --
   ------------------

   procedure To_Proxy_Oid
     (OA    : access Basic_Obj_Adapter;
      R     :        References.Ref;
      Oid   :    out Object_Id_Access;
      Error : in out PolyORB.Exceptions.Error_Container) is
   begin
      pragma Debug (O ("To_Proxy_Oid: enter"));

      if OA.Proxies_OA = null then
         pragma Debug (O ("No Proxies_OA."));
         Oid := null;
         return;
      end if;

      declare
         Oid_Data : aliased Object_Id :=
           Object_Id (References.IOR.Object_To_Opaque (R));

         U_Oid : Unmarshalled_Oid;

      begin
         pragma Debug (O ("To_Proxy_Oid: Oid data length:"
                          & Integer'Image (Oid_Data'Length)));

         Assign_Object_Identifier
           (OA.Id_Assignment_Policy.all,
            POA_Types.Obj_Adapter_Access (OA),
            Oid_Data'Unchecked_Access,
            U_Oid,
            Error);

         if Found (Error) then
            return;
         end if;

         pragma Debug (O ("To_Proxy_Oid: leave"));

         Oid := U_Oid_To_Oid (U_Oid);
      end;
   end To_Proxy_Oid;

   ------------------
   -- Proxy_To_Ref --
   ------------------

   function Proxy_To_Ref
     (OA  : access Basic_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return References.Ref
   is
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Warnings (On);

      Oid_Data : aliased Object_Id := Objects.To_Oid
        (To_Standard_String (Oid_To_U_Oid (Oid).Id));

      type SEA_Access is access all Ada.Streams.Stream_Element_Array;

      function As_SEA_Access is new Ada.Unchecked_Conversion
        (Object_Id_Access, SEA_Access);

   begin
      pragma Debug (O ("PTR: Oid data length:"
                       & Integer'Image (Oid_Data'Length)));

      return References.IOR.Opaque_To_Object
        (As_SEA_Access (Oid_Data'Unchecked_Access));
   end Proxy_To_Ref;

end PolyORB.POA.Basic_POA;
