------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                P O L Y O R B . P O A . B A S I C _ P O A                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                Copyright (C) 2001 Free Software Fundation                --
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
--              PolyORB is maintained by ENST Paris University.             --
--                                                                          --
------------------------------------------------------------------------------

--  Basic POA implementation.

--  $Id$

with Ada.Real_Time;
with Ada.Streams;
with Ada.Unchecked_Conversion;

with PolyORB.Log;
pragma Elaborate_All (PolyORB.Log);
with PolyORB.Objects;
with PolyORB.POA_Config;
with PolyORB.POA_Manager.Basic_Manager;
with PolyORB.POA_Types;
with PolyORB.References.IOR;
with PolyORB.Smart_Pointers;
with PolyORB.Types;

package body PolyORB.POA.Basic_POA is

   use PolyORB.Locks;
   use PolyORB.Log;
   use PolyORB.POA_Manager;
   use PolyORB.POA_Manager.Basic_Manager;
   use PolyORB.POA_Policies;
   use PolyORB.POA_Types;
   use PolyORB.Types;

   package L is new PolyORB.Log.Facility_Log ("polyorb.poa.basic_poa");
   procedure O (Message : in Standard.String; Level : Log_Level := Debug)
     renames L.Output;

   POA_Path_Separator : constant String := "/";

   --------------------------------------------------------
   -- Declaration of additional procedures and functions --
   --------------------------------------------------------

   function Get_Boot_Time return Time_Stamp;

   function Get_Child (Adapter : access Basic_Obj_Adapter;
                       Name    : in     String)
                      return POA_Types.Obj_Adapter_Access;
   --  Look in the list of children of the Adapter if an OA with
   --  the given name already exists.
   --  The function doesn't take care of locking the list of children!

   procedure Init_With_User_Policies
     (OA       : access Basic_Obj_Adapter;
      Policies : PolyORB.POA_Policies.PolicyList);

   procedure Init_With_Default_Policies
     (OA : access Basic_Obj_Adapter);
   --  Initialize OA with a default set of policies provided by
   --  the currently active POA configuration.

   procedure Check_Policies_Compatibility
     (OA : Basic_Obj_Adapter_Access);

   function Register_Child
     (Self  : access Basic_Obj_Adapter;
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
      Id       : access PolyORB.Objects.Object_Id;
      Do_Check :        Check_State)
     return PolyORB.Objects.Servant_Access;
   --  The Find_Servant from PolyORB, plus a parameter.
   --  If check is NO_CHECK, the POA doesn't check its state.

   function Find_POA_Recursively
     (Self : access Basic_Obj_Adapter;
      Name :        Types.String)
     return Basic_Obj_Adapter_Access;
   --  Starting from given POA, looks for the POA in all the descendancy whose
   --  name is Name. Returns null if not found.

   function POA_Manager_Of (OA : access Basic_Obj_Adapter)
     return POA_Manager.POAManager_Access;

   ------------------------------------
   --  Code of additional functions  --
   ------------------------------------

   --------------------
   -- POA_Manager_Of --
   --------------------

   function POA_Manager_Of (OA : access Basic_Obj_Adapter)
     return POA_Manager.POAManager_Access
   is
      use PolyORB.Smart_Pointers;

      E : constant Entity_Ptr := Entity_Of (OA.POA_Manager);
   begin
      pragma Assert (E.all in POA_Manager.POAManager'Class);
      return POAManager_Access (E);
   end POA_Manager_Of;

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
      if Adapter.Children /= null then
         for I in 1 .. Length (Adapter.Children.all) loop
            A_Child := Element_Of (Adapter.Children.all, I);
            if PolyORB.POA.Obj_Adapter_Access (A_Child).Name = Name then
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

   ------------------
   -- Set_Policies --
   ------------------

   procedure Set_Policies
     (OA       : access Basic_Obj_Adapter;
      Policies : PolyORB.POA_Policies.PolicyList;
      Default  : Boolean);
   --  Set OA policies from the values in Policies.
   --  If Default is True, set only those policies that
   --  are not yet explicitly set in OA. If Default is False,
   --  set all policies, and warn for duplicates.

   procedure Set_Policies
     (OA       : access Basic_Obj_Adapter;
      Policies : PolyORB.POA_Policies.PolicyList;
      Default  : Boolean)
   is
      use Policy_Sequences;

      Policies_Array : constant Element_Array
        := To_Element_Array (Policies);
      A_Policy : Policy_Access;
   begin
      pragma Debug (O ("Init Basic_POA with user provided policies"));
      for I in Policies_Array'Range loop
         A_Policy := Policies_Array (I);

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
               OA.Servant_Retention_Policy
                 := ServantRetentionPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in RequestProcessingPolicy'Class then
            if OA.Request_Processing_Policy = null or else not Default then
               if OA.Request_Processing_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in RequestProcessingPolicy:"
                        & " using last one"));
                  null;
               end if;
               OA.Request_Processing_Policy
                 := RequestProcessingPolicy_Access (A_Policy);
            end if;

         elsif A_Policy.all in ImplicitActivationPolicy'Class then
            if OA.Implicit_Activation_Policy = null or else not Default then
               if OA.Implicit_Activation_Policy /= null then
                  pragma Debug
                    (O ("Duplicate in ImplicitActivationPolicy:"
                        & "using last one"));
                  null;
               end if;
               OA.Implicit_Activation_Policy
                 := ImplicitActivationPolicy_Access (A_Policy);
            end if;

         else
            null;
            pragma Debug (O ("Unknown policy ignored"));
         end if;
      end loop;
   end Set_Policies;

   -----------------------------
   -- Init_With_User_Policies --
   -----------------------------

   procedure Init_With_User_Policies
     (OA       : access Basic_Obj_Adapter;
      Policies : PolyORB.POA_Policies.PolicyList) is
   begin
      pragma Debug (O ("Init Basic_POA with user provided policies"));
      Set_Policies (OA, Policies, Default => False);
   end Init_With_User_Policies;

   --------------------------------
   -- Init_With_Default_Policies --
   --------------------------------

   procedure Init_With_Default_Policies
     (OA : access Basic_Obj_Adapter) is
   begin
      pragma Debug (O ("Init Basic_POA with default policies"));
      Set_Policies
        (OA, PolyORB.POA_Config.Default_Policies
         (PolyORB.POA_Config.Configuration.all),
         Default => True);
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
         PolyORB.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Lifespan_Policy.all,
         PolyORB.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Id_Uniqueness_Policy.all,
         PolyORB.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Id_Assignment_Policy.all,
         PolyORB.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Servant_Retention_Policy.all,
         PolyORB.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Request_Processing_Policy.all,
         PolyORB.POA_Types.Obj_Adapter_Access (OA));
      Check_Compatibility
        (OA.Implicit_Activation_Policy.all,
         PolyORB.POA_Types.Obj_Adapter_Access (OA));
   end Check_Policies_Compatibility;

   --------------------
   -- Register_Child --
   --------------------

   function Register_Child
     (Self  : access Basic_Obj_Adapter;
      Child :        Basic_Obj_Adapter_Access)
     return Positive
   is
      use PolyORB.POA_Types.POA_Sequences;
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
                             PolyORB.POA_Types.Obj_Adapter_Access (Child));
            return I;
         end if;
      end loop;
      Append (Sequence (Self.Children.all),
              PolyORB.POA_Types.Obj_Adapter_Access (Child));
      return Length (Sequence (Self.Children.all));
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

   -------------------
   -- Destroy_Locks --
   -------------------

   procedure Destroy_Locks
     (OA : in out Basic_Obj_Adapter)
   is
      use PolyORB.Locks;
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
      if not Is_Nil (OA.POA_Manager) then
         Remove_POA
           (POA_Manager_Of (OA),
            PolyORB.POA_Types.Obj_Adapter_Access (OA));
         Set (OA.POA_Manager, null);
      end if;
      --  Destroy_Policies (OA.all);
      --  XXX Cannot destroy_policies here because another
      --  POA initialised from the default configuration could
      --  be using the same instances of policy objects!
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

   procedure Create_Root_POA
     (New_Obj_Adapter : access Basic_Obj_Adapter);

   procedure Create_Root_POA
     (New_Obj_Adapter : access Basic_Obj_Adapter) is
   begin
      pragma Debug (O ("Create a new Root_POA"));

      --  Create new Obj Adapter
      New_Obj_Adapter.Boot_Time        := Get_Boot_Time;
      New_Obj_Adapter.Name             := To_PolyORB_String ("RootPOA");
      New_Obj_Adapter.Absolute_Address := To_PolyORB_String ("");

      Create (New_Obj_Adapter.Children_Lock);
      Create (New_Obj_Adapter.Map_Lock);

      Set (New_Obj_Adapter.POA_Manager, new Basic_POA_Manager);
      Create (POA_Manager_Of (New_Obj_Adapter));
      Register_POA
        (POA_Manager_Of (New_Obj_Adapter),
         POA_Types.Obj_Adapter_Access (New_Obj_Adapter));

      --  Create and initialize policies factory
      PolyORB.POA_Config.Initialize
        (PolyORB.POA_Config.Configuration.all);

      --  Use default policies
      Init_With_Default_Policies (New_Obj_Adapter);
   end Create_Root_POA;

   -------------------------------------------------
   -- Procedures and functions required by Corba  --
   -------------------------------------------------

   ----------------
   -- Create_POA --
   ----------------

   function Create_POA
     (Self         : access Basic_Obj_Adapter;
      Adapter_Name :        Types.String;
      A_POAManager :        POA_Manager.POAManager_Access;
      Policies     :        PolyORB.POA_Policies.PolicyList)
     return Obj_Adapter_Access
   is
      New_Obj_Adapter : Basic_Obj_Adapter_Access;
      Children_Locked : Boolean := False;
      Child_Id : Integer;

   begin
      --  Validity checks on Adapter_Name
      if Adapter_Name = ""
        or else Index (Adapter_Name, POA_Path_Separator) /= 0
      then
         raise PolyORB.POA.Invalid_Name;
      end if;

      --  Look if there is already a child with this name
      if Self.Children /= null then
         Lock_W (Self.Children_Lock);
         --  Write Lock here: content of children has to be the same when
         --  we add the new child.
         Children_Locked := True;
         if Get_Child (Self, To_Standard_String (Adapter_Name)) /= null then
            raise PolyORB.POA.Adapter_Already_Exists;
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
         Set (New_Obj_Adapter.POA_Manager, new Basic_POA_Manager);
         Create (POA_Manager_Of (New_Obj_Adapter));
         Register_POA
           (POA_Manager_Of (New_Obj_Adapter),
            PolyORB.POA_Types.Obj_Adapter_Access (New_Obj_Adapter));
      else
         Set
           (New_Obj_Adapter.POA_Manager,
            Smart_Pointers.Entity_Ptr (A_POAManager));
         Register_POA
           (A_POAManager,
            PolyORB.POA_Types.Obj_Adapter_Access (New_Obj_Adapter));
      end if;

      --  Init policies with those given by the user
      Init_With_User_Policies (New_Obj_Adapter, Policies);

      --  Use default policies if not provided by the user
      Init_With_Default_Policies (New_Obj_Adapter);

      --  Check compatibilities between policies
      Check_Policies_Compatibility (New_Obj_Adapter);

      --  Register new obj_adapter as a sibling of the current POA
      if not Children_Locked then
         Lock_W (Self.Children_Lock);
      end if;

      Child_Id := Register_Child (Self, New_Obj_Adapter);
      if Length (Self.Absolute_Address) > 0 then
         New_Obj_Adapter.Absolute_Address := Self.Absolute_Address
           & To_PolyORB_String (POA_Path_Separator) & Adapter_Name;
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
      when PolyORB.POA.Adapter_Already_Exists =>
         raise;
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
      use PolyORB.POA_Types.POA_Sequences;
      A_Child : PolyORB.POA.Obj_Adapter_Access;
      Name    : Types.String := Self.Name;
   begin
      pragma Debug (O ("Start destroying POA "
                       & To_Standard_String (Name)));

      --  Destroy all children
      Lock_W (Self.Children_Lock);
      if Self.Children /= null then
         for I in 1 .. Length (Sequence (Self.Children.all)) loop
            A_Child := PolyORB.POA.Obj_Adapter_Access
              (Element_Of (Sequence (Self.Children.all), I));
            POA.Destroy
              (A_Child.all'Access,
               Etherealize_Objects,
               Wait_For_Completion);
            Replace_Element (Sequence (Self.Children.all), I, null);
         end loop;
      end if;
      Unlock_W (Self.Children_Lock);

      --  Tell father to remove current POA from its list of children
      if Self.Father /= null then
         POA.Remove_POA_By_Name
           (PolyORB.POA.Obj_Adapter_Access (Self.Father).all'Access,
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

   ----------------------------------
   -- Create_Object_Identification --
   ----------------------------------

   function Create_Object_Identification
     (Self : access Basic_Obj_Adapter;
      Hint :        Object_Id_Access := null)
     return Unmarshalled_Oid is
   begin
      return Assign_Object_Identifier
        (Self.Id_Assignment_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         Hint);
   end Create_Object_Identification;

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self      : access Basic_Obj_Adapter;
      P_Servant : in     Servant_Access;
      Hint      :        Object_Id_Access := null)
     return Object_Id
   is
      Allocated_Oid : constant Unmarshalled_Oid
        := Create_Object_Identification (Self, Hint);
      --  Must be free'd when the object is deactivated.
   begin
      Retain_Servant_Association
        (Self.Servant_Retention_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         P_Servant, Allocated_Oid);

      declare
         A_Oid : Object_Id_Access := U_Oid_To_Oid (Allocated_Oid);
         Oid : constant Object_Id := A_Oid.all;
      begin
         Free (A_Oid);
         return Oid;
         --  XXX yuk yuk:
         --    a number of Oid copies;
         --    excessive stack allocation.
      end;
   exception
      when Invalid_Policy =>
         --  Deallocate_Object_Identification (Self, Allocated_Oid);
         raise;
   end Activate_Object;

   -----------------------
   -- Deactivate_Object --
   -----------------------

   procedure Deactivate_Object
     (Self      : access Basic_Obj_Adapter;
      Oid       : in Object_Id)
   is
      A_Oid : aliased Object_Id := Oid;
      U_Oid : Unmarshalled_Oid
        := Oid_To_U_Oid (A_Oid'Unchecked_Access);
   begin
      Etherealize_All
        (Self.Request_Processing_Policy.all,
         PolyORB.POA_Types.Obj_Adapter_Access (Self),
         U_Oid);

      Forget_Servant_Association
        (Self.Servant_Retention_Policy.all,
         PolyORB.POA_Types.Obj_Adapter_Access (Self),
         U_Oid);
      --  XXX ??? Wait for completion?
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
        := Retained_Servant_To_Id
        (Self.Servant_Retention_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         P_Servant);
   begin
      Oid := Activate_Again
        (Self.Id_Uniqueness_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         P_Servant,
         Oid);

      if Oid = null then

         --  XXX here should also check whether we are in the
         --  context of executing a dispatched operation on
         --  Servant, and if it is the case return the 'current'
         --  oid (for USE_DEFAULT_SERVANT policy).
         raise PolyORB.POA.Servant_Not_Active;
      end if;
      return Oid.all;
   end Servant_To_Id;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant
     (Self : access Basic_Obj_Adapter;
      Oid  :        Object_Id)
     return Servant_Access
   is
      Servant : Servant_Access;
      A_Oid : aliased Object_Id := Oid;
      U_Oid : constant Unmarshalled_Oid
        := Oid_To_U_Oid (A_Oid'Access);
   begin
      Ensure_Lifespan
        (Self.Lifespan_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         U_Oid);

      Servant := Id_To_Servant
        (Self.Request_Processing_Policy.all,
         POA_Types.Obj_Adapter_Access (Self),
         U_Oid);
      return Servant;
   end Id_To_Servant;

   --------------------------
   -- Find_POA_Recursively --
   --------------------------

   function Find_POA_Recursively
     (Self : access Basic_Obj_Adapter;
      Name : Types.String)
     return Basic_Obj_Adapter_Access
   is
      use PolyORB.POA_Types.POA_Sequences;

      Split_Point      : constant Natural
        := Index (Name, POA_Path_Separator);

      Remaining_Name   : Types.String;
      A_Child_Name     : Types.String;
      A_Child          : Obj_Adapter_Access;
   begin
      pragma Debug (O ("Find_POA_Recursively: enter, Name = "
                       & To_Standard_String (Name)));

      if Name = "" then
         return Basic_Obj_Adapter_Access (Self);
      end if;

      pragma Assert (Split_Point /= 1);
      if Split_Point = 0 then
         A_Child_Name := Name;
      else
         A_Child_Name := Head (Name, Split_Point - 1);
         Remaining_Name := Tail (Name, Length (Name) - Split_Point);
      end if;

      for I in 1 .. Length (Sequence (Self.Children.all)) loop
         A_Child := Obj_Adapter_Access
           (Element_Of (Sequence (Self.Children.all), I));

         if A_Child.Name = A_Child_Name then
            return Find_POA_Recursively
              (Basic_Obj_Adapter (A_Child.all)'Access,
               Remaining_Name);
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
      To.Id_Assignment_Policy      := From.Id_Assignment_Policy;
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
      Child_Name :        Types.String)
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
           and then PolyORB.POA.Obj_Adapter_Access (A_Child).Name = Child_Name
         then
            Replace_Element (Sequence (Self.Children.all), I, null);
            return;
         end if;
      end loop;
   end Remove_POA_By_Name;

   ---------------------------------------------------
   --  Procedures and functions required by PolyORB  --
   ---------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (OA : access Basic_Obj_Adapter) is
   begin
      Create_Root_POA (OA);
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
      Obj :        PolyORB.Objects.Servant_Access)
     return PolyORB.Objects.Object_Id
   is
      Id : constant PolyORB.Objects.Object_Id
        := PolyORB.Objects.Object_Id
        (Activate_Object (OA, Servant_Access (Obj)));

      --  XXX Is it approriate to call Activate_Object
      --  (a standard operation of the POA) at this point?

      --  Activation will actually be performed only if
      --  Obj has not already been activated *and*
      --  the implicit activation policy allows implicit activation.

      --  If the implicit allocation policy does not allow implicit
      --  activation, then this call to Activate_Object must not
      --  activate Obj. It must return the previous Id if Obj has
      --  been activated *and* the retention policy is RETAIN.
      --  It must return an error condition if Obj has not been
      --  activated, or if the retention policy is NON_RETAIN.

      --  If implicit activation is allowed but the object has
      --  already been activated, and the retention policy is RETAIN,
      --  then the previous ID must be return and no activation must
      --  be performed. If the retention policy is not RETAIN, the
      --  behaviour then depends on the Id_Uniqueness_Policy...

      --  To make a long story short: there are a number of conditions
      --  where the correct behaviour here consists in NOT activating
      --  an object. So, is the above correct? This is not a
      --  trivial question.
   begin
      pragma Debug (O ("Exporting Servant, resulting Id is "
                       & PolyORB.Objects.To_String (Id)));
      return Id;
   end Export;

   --------------
   -- Unexport --
   --------------

   procedure Unexport
     (OA : access Basic_Obj_Adapter;
      Id :        PolyORB.Objects.Object_Id)
   is
   begin
      Deactivate_Object (OA, Object_Id (Id));
   end Unexport;

   ------------------------
   -- Get_Empty_Arg_List --
   ------------------------

   function Get_Empty_Arg_List
     (OA     : access Basic_Obj_Adapter;
      Oid    : access PolyORB.Objects.Object_Id;
      Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.NVList.Ref
   is
      S : Servant_Access;
      Nil_Result : PolyORB.Any.NVList.Ref;
   begin
      pragma Debug (O ("Get_Empty_Arg_List for Id "
                       & PolyORB.Objects.To_String (Oid.all)));
      S := Servant_Access (Find_Servant (OA, Oid, NO_CHECK));
      if S.If_Desc.PP_Desc /= null then
         return S.If_Desc.PP_Desc (Method);
      else
         return Nil_Result;
         --  If If_Desc is null (eg in the case of an actual
         --  use of the DSI, where no generated code is used on
         --  the server side, another means of determining the
         --  signature must be used, eg a query to an
         --  Interface repository. Here we only return a Nil
         --  NVList.Ref, indicating to the Protocol layer
         --  that arguments unmarshalling is to be deferred
         --  until the request processing in the Application
         --  layer is started (at which time the Application
         --  layer can provide more information as to the
         --  signature of the called method).
      end if;
   end Get_Empty_Arg_List;

   ----------------------
   -- Get_Empty_Result --
   ----------------------

   function Get_Empty_Result
     (OA     : access Basic_Obj_Adapter;
      Oid    : access PolyORB.Objects.Object_Id;
      Method : PolyORB.Requests.Operation_Id)
     return PolyORB.Any.Any
   is
      S : Servant_Access;
   begin
      pragma Debug (O ("Get_Empty_Result for Id "
                       & PolyORB.Objects.To_String (Oid.all)));
      S := Servant_Access (Find_Servant (OA, Oid, NO_CHECK));
      if S.If_Desc.RP_Desc /= null then
         return S.If_Desc.RP_Desc (Method);
      end if;
      raise PolyORB.Not_Implemented;
      --  Cf. comment above.
   end Get_Empty_Result;

   ------------------
   -- Find_Servant --
   ------------------

   function Find_Servant
     (OA : access Basic_Obj_Adapter;
      Id : access PolyORB.Objects.Object_Id)
     return PolyORB.Objects.Servant_Access
   is
   begin
      return Find_Servant (OA, Id, CHECK);
   end Find_Servant;

   ------------------
   -- Find_Servant --
   ------------------

   function Find_Servant
     (OA       : access Basic_Obj_Adapter;
      Id       : access PolyORB.Objects.Object_Id;
      Do_Check :        Check_State)
     return PolyORB.Objects.Servant_Access
   is
      U_Oid : constant Unmarshalled_Oid := Oid_To_U_Oid (Id);
   begin
      if Do_Check = CHECK then
         case Get_State (POA_Manager_Of (OA).all) is
            when DISCARDING | INACTIVE =>
               --  Raise_Transient (1);
               --  ??? Do we have to do something special for INACTIVE
               raise Transient;
            when HOLDING =>
               declare
                  S : PolyORB.Objects.Servant_Access;
               begin
                  S := PolyORB.Objects.Servant_Access
                    (Get_Hold_Servant
                     (POA_Manager_Of (OA),
                      PolyORB.POA_Types.Obj_Adapter_Access (OA)));
                  return S;
               end;
            when others =>
               null;
         end case;
      end if;

      pragma Debug
        (O ("Look for OA with name #"
            & To_Standard_String (U_Oid.Creator)
            & "# starting from RootPOA"));

      declare
         The_OA : constant Basic_Obj_Adapter_Access
           := Find_POA_Recursively (OA, U_Oid.Creator);
      begin
         pragma Debug
           (O ("OA : " & To_Standard_String (The_OA.Name)
               & " looks for servant associated with Id "
               & PolyORB.Objects.To_String (Id.all)));

         if The_OA /= null then
            return PolyORB.Objects.Servant_Access
              (Id_To_Servant (The_OA, Id.all));
         else
            raise Invalid_Object_Id;
            --  This is an exception from PolyORB
         end if;
      end;
   end Find_Servant;

   ---------------------
   -- Release_Servant --
   ---------------------

   procedure Release_Servant
     (OA      : access Basic_Obj_Adapter;
      Id      : access PolyORB.Objects.Object_Id;
      Servant : in out PolyORB.Objects.Servant_Access)
   is
   begin
      pragma Warnings (Off);
      pragma Unreferenced (OA);
      pragma Unreferenced (Id);
      pragma Unreferenced (Servant);
      pragma Warnings (On);

      null;
      --  XXX if servant has been created on the fly, should
      --  destroy it now (else do nothing).
   end Release_Servant;

   procedure Set_Proxies_OA
     (OA         : access Basic_Obj_Adapter;
      Proxies_OA :        Basic_Obj_Adapter_Access)
   is
   begin
      pragma Assert (OA.Proxies_OA = null and then Proxies_OA /= null);
      OA.Proxies_OA := Proxies_OA;
   end Set_Proxies_OA;


   function Is_Proxy_Oid
     (OA  : access Basic_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return Boolean
   is
      U_Oid : constant Unmarshalled_Oid
        := Oid_To_U_Oid (Oid);
      Id_OA : constant Basic_Obj_Adapter_Access
        := Find_POA_Recursively (OA, U_Oid.Creator);
   begin
      return OA.Proxies_OA /= null
        and then Id_OA = OA.Proxies_OA;
   end Is_Proxy_Oid;

   use Ada.Streams;

   function To_Proxy_Oid
     (OA : access Basic_Obj_Adapter;
      R  :        References.Ref)
     return Object_Id_Access
   is
   begin
      pragma Debug (O ("To_Proxy_Oid: enter"));

      if OA.Proxies_OA = null then
         pragma Debug (O ("No Proxies_OA."));
         return null;
      end if;

      declare
         Oid_Data : aliased Object_Id
           := Object_Id (References.IOR.Object_To_Opaque (R));
         U_Oid : constant Unmarshalled_Oid
           := Create_Object_Identification
           (OA.Proxies_OA, Oid_Data'Unchecked_Access);
      begin
         pragma Debug (O ("TPO: Oid data length:"
                          & Oid_Data'Length'Img));
         pragma Debug (O ("To_Proxy_Oid: leave"));
         return U_Oid_To_Oid (U_Oid);
      end;
   end To_Proxy_Oid;

   function Proxy_To_Ref
     (OA  : access Basic_Obj_Adapter;
      Oid : access Objects.Object_Id)
     return References.Ref
   is
      Oid_Data : aliased Object_Id := Objects.To_Oid
        (To_Standard_String (Oid_To_U_Oid (Oid).Id));
      type SEA_Access is access all Ada.Streams.Stream_Element_Array;
      function As_SEA_Access is new Ada.Unchecked_Conversion
        (Object_Id_Access, SEA_Access);

   begin
      pragma Debug (O ("PTR: Oid data length:"
                       & Oid_Data'Length'Img));
      return References.IOR.Opaque_To_Object
        (As_SEA_Access (Oid_Data'Unchecked_Access));
   end Proxy_To_Ref;

end PolyORB.POA.Basic_POA;
