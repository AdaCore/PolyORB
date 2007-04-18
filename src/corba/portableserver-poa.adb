------------------------------------------------------------------------------
--                                                                          --
--                           POLYORB COMPONENTS                             --
--                                                                          --
--                   P O R T A B L E S E R V E R . P O A                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2001-2007, Free Software Foundation, Inc.          --
--                                                                          --
-- PolyORB is free software; you  can  redistribute  it and/or modify it    --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. PolyORB is distributed  in the hope that it will be  useful,    --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with PolyORB; see file COPYING. If    --
-- not, write to the Free Software Foundation, 51 Franklin Street, Fifth    --
-- Floor, Boston, MA 02111-1301, USA.                                       --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
--                  PolyORB is maintained by AdaCore                        --
--                     (email: sales@adacore.com)                           --
--                                                                          --
------------------------------------------------------------------------------

with CORBA.DomainManager.Helper;
with CORBA.ORB;

with PortableServer.Helper;
with PortableServer.ServantActivator;
with PortableServer.ServantLocator;

with PolyORB.Annotations;
with PolyORB.Binding_Data;
with PolyORB.Components;
with PolyORB.CORBA_P.Initial_References;
with PolyORB.Exceptions;
with PolyORB.Initialization;
with PolyORB.Log;
with PolyORB.ORB;
with PolyORB.POA_Manager;
with PolyORB.POA_Policies;
with PolyORB.POA_Policies.Id_Assignment_Policy;
with PolyORB.POA_Types;
with PolyORB.References;
with PolyORB.References.Binding;
with PolyORB.Servants;
with PolyORB.Setup;
with PolyORB.Smart_Pointers;
with PolyORB.Types;
with PolyORB.Utils.Strings;

with PolyORB.CORBA_P.AdapterActivator;
with PolyORB.CORBA_P.Domain_Management;
with PolyORB.CORBA_P.Exceptions;
with PolyORB.CORBA_P.Interceptors_Hooks;
with PolyORB.CORBA_P.POA_Config;
with PolyORB.CORBA_P.Policy_Management;
with PolyORB.CORBA_P.ServantActivator;
with PolyORB.CORBA_P.ServantLocator;

package body PortableServer.POA is

   use PolyORB.Errors;
   use PolyORB.Log;
   use PolyORB.POA_Types;
   use PortableServer.Helper;

   package L is new PolyORB.Log.Facility_Log ("portableserver.poa");
   procedure O (Message : String; Level : Log_Level := Debug)
     renames L.Output;
   function C (Level : Log_Level := Debug) return Boolean
     renames L.Enabled;
   pragma Unreferenced (C); --  For conditional pragma Debug

   procedure Initialize;
   --  Register root POA and set it to HOLD state

   procedure Associate_To_Domain_Managers (P_Servant : Servant);
   --  Associate servant with domain managers

   procedure Extract_Reference_Info
     (Self          : Local_Ref;
      Reference     : CORBA.Object.Ref'Class;
      Ref_Servant   : out Servant;
      Ref_Object_Id : out Object_Id_Access);
   --  Given a Reference to a local object, return its servant and object id.
   --  Shared code between Reference_To_Servant and Reference_To_Id.

   function To_POA (Self : Local_Ref) return PolyORB.POA.Obj_Adapter_Access;
   --  Convert a Ref to a CORBA POA to a PolyORB POA.

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self      : Local_Ref;
      P_Servant : Servant)
     return ObjectId
   is
      Error : PolyORB.Errors.Error_Container;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
   begin
      PolyORB.POA.Activate_Object
        (POA,
         PolyORB.Servants.Servant_Access (To_PolyORB_Servant (P_Servant)),
         null,
         U_Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      Associate_To_Domain_Managers (P_Servant);

      declare
         Oid : constant PolyORB.POA_Types.Object_Id :=
           PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);
      begin
         return PortableServer.Internals.To_PortableServer_ObjectId (Oid);
      end;
   end Activate_Object;

   -----------------------------
   -- Activate_Object_With_Id --
   -----------------------------

   procedure Activate_Object_With_Id
     (Self      : Local_Ref;
      Oid       : ObjectId;
      P_Servant : Servant)
   is
      Error : PolyORB.Errors.Error_Container;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;

      A_Oid : aliased PolyORB.POA_Types.Object_Id :=
        PolyORB.POA_Types.Object_Id
        (PortableServer.Internals.To_PolyORB_Object_Id (Oid));

   begin
      PolyORB.POA.Activate_Object
        (POA,
         PolyORB.Servants.Servant_Access (To_PolyORB_Servant (P_Servant)),
         A_Oid'Unchecked_Access,
         U_Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      Associate_To_Domain_Managers (P_Servant);
   end Activate_Object_With_Id;

   ----------------------------------
   -- Associate_To_Domain_Managers --
   ----------------------------------

   procedure Associate_To_Domain_Managers (P_Servant : Servant) is
      use PolyORB.CORBA_P.Initial_References;

      Policy_Manager : constant CORBA.DomainManager.Ref
        := CORBA.DomainManager.Helper.To_Ref
        (Resolve_Initial_References ("PolyORBPolicyDomainManager"));

      Note           : PolyORB.CORBA_P.Domain_Management.Domain_Manager_Note;

   begin
      if CORBA.DomainManager.Is_Nil (Policy_Manager) then
         pragma Debug (O ("No policy domain manager registered"));
         return;
      end if;

      --  Associate activated servant with domain managers. For now we just
      --  add policy domain manager into list of object domain managers.

      CORBA.DomainManager.IDL_SEQUENCE_DomainManager.Append
        (Note.Domain_Managers, Policy_Manager);

      PolyORB.Annotations.Set_Note
        (PolyORB.Servants.Notepad_Of
         (CORBA.Impl.To_PolyORB_Servant
          (CORBA.Impl.Object (P_Servant.all)'Access)).all,
         Note);

      pragma Debug (O ("Servant associated with policy domain manager"));
   end Associate_To_Domain_Managers;

   ---------------------------------
   -- Create_Id_Assignment_Policy --
   ---------------------------------

   function Create_Id_Assignment_Policy
     (Value : PortableServer.IdAssignmentPolicyValue)
     return PortableServer.IdAssignmentPolicy.Ref is
   begin
      return PortableServer.IdAssignmentPolicy.To_Ref
        (CORBA.ORB.Create_Policy
         (ID_ASSIGNMENT_POLICY_ID, To_Any (Value)));
   end Create_Id_Assignment_Policy;

   ---------------------------------
   -- Create_Id_Uniqueness_Policy --
   ---------------------------------

   function Create_Id_Uniqueness_Policy
     (Value : PortableServer.IdUniquenessPolicyValue)
     return PortableServer.IdUniquenessPolicy.Ref is
   begin
      return PortableServer.IdUniquenessPolicy.To_Ref
        (CORBA.ORB.Create_Policy
         (ID_UNIQUENESS_POLICY_ID, To_Any (Value)));
   end Create_Id_Uniqueness_Policy;

   ---------------------------------------
   -- Create_Implicit_Activation_Policy --
   ---------------------------------------

   function Create_Implicit_Activation_Policy
     (Value : PortableServer.ImplicitActivationPolicyValue)
     return PortableServer.ImplicitActivationPolicy.Ref is
   begin
      return PortableServer.ImplicitActivationPolicy.To_Ref
        (CORBA.ORB.Create_Policy
         (IMPLICIT_ACTIVATION_POLICY_ID, To_Any (Value)));
   end Create_Implicit_Activation_Policy;

   ----------------------------
   -- Create_Lifespan_Policy --
   ----------------------------

   function Create_Lifespan_Policy
     (Value : PortableServer.LifespanPolicyValue)
     return PortableServer.LifespanPolicy.Ref is
   begin
      return PortableServer.LifespanPolicy.To_Ref
        (CORBA.ORB.Create_Policy
         (LIFESPAN_POLICY_ID, To_Any (Value)));
   end Create_Lifespan_Policy;

   ----------------
   -- Create_POA --
   ----------------

   function Create_POA
     (Self         : Local_Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Local_Ref;
      Policies     : CORBA.Policy.PolicyList)
     return Local_Ref'Class
   is
      use type PolyORB.CORBA_P.Interceptors_Hooks.POA_Create_Handler;
      use type CORBA.Unsigned_Short;

      Res : PolyORB.POA.Obj_Adapter_Access;
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      POA_Policies : PolyORB.POA_Policies.PolicyList
        := PolyORB.CORBA_P.POA_Config.Convert_PolicyList (Policies);

      Note    : PolyORB.CORBA_P.Policy_Management.Policy_Manager_Note;
      Error   : PolyORB.Errors.Error_Container;
      Indices : CORBA.Unsigned_Short;

   begin
      pragma Debug (O ("Creating POA "
                       & CORBA.To_Standard_String (Adapter_Name)));

      --  Convert list of policies into policy override Note

      declare
         use PolyORB.CORBA_P.Policy_Management;

         The_Type : CORBA.PolicyType;
      begin
         for J in 1 .. CORBA.Policy.IDL_SEQUENCE_Policy.Length (Policies) loop
            The_Type :=
              CORBA.Policy.Get_Policy_Type
               (CORBA.Policy.IDL_SEQUENCE_Policy.Get_Element (Policies, J));

            if not Is_POA_Policy (The_Type) then
               PolyORB.Errors.Throw
                (Error,
                 PolyORB.Errors.InvalidPolicy_E,
                 PolyORB.Errors.InvalidPolicy_Members'
                  (Index => PolyORB.Types.Unsigned_Short (J)));
               exit;
            end if;

            Note.Overrides (The_Type) :=
              CORBA.Policy.IDL_SEQUENCE_Policy.Get_Element (Policies, J);
         end loop;
      end;

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      --  Check policy compatibility

      PolyORB.CORBA_P.Policy_Management.Check_Compatibility
        (Note.Overrides, Indices);

      if Indices /= 0 then
         Raise_InvalidPolicy ((Index => Indices));
      end if;

      --  Note: policy compability is tested by PolyORB.POA.Create_POA

      PolyORB.POA.Create_POA
        (POA,
         CORBA.To_String (Adapter_Name),
         PolyORB.POA_Manager.POAManager_Access
         (PortableServer.POAManager.Entity_Of (A_POAManager)),
         POA_Policies,
         Res,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      PolyORB.Annotations.Set_Note (PolyORB.POA.Notepad_Of (Res).all, Note);

      PolyORB.POA_Policies.Policy_Lists.Deallocate (POA_Policies);

      if not Found (Error) then
         if PolyORB.CORBA_P.Interceptors_Hooks.POA_Create /= null then
            PolyORB.CORBA_P.Interceptors_Hooks.POA_Create (Res, Error);

            if Found (Error) then
               PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
               --  XXX  if Error found, destroy POA
            end if;
         end if;
      end if;

      pragma Debug (O ("POA created"));

      declare
         New_Ref : Local_Ref'Class := Internals.To_CORBA_POA (Res);

      begin
         return New_Ref;
      end;
   end Create_POA;

   ----------------------
   -- Create_Reference --
   ----------------------

   function Create_Reference
     (Self : Local_Ref;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      Error : PolyORB.Errors.Error_Container;
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);
      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
   begin
      PolyORB.POA.Create_Object_Identification (POA, null, U_Oid, Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      declare
         Oid : aliased PolyORB.POA_Types.Object_Id :=
           PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);

         P_Result : PolyORB.References.Ref;
         C_Result : CORBA.Object.Ref;
      begin
         PolyORB.ORB.Create_Reference
           (PolyORB.Setup.The_ORB,
            Oid'Access,
            CORBA.To_Standard_String (Intf),
            P_Result);
         --  Obtain object reference.

         CORBA.Object.Internals.Convert_To_CORBA_Ref
           (P_Result, C_Result);
         return C_Result;
      end;
   end Create_Reference;

   ------------------------------
   -- Create_Reference_With_Id --
   ------------------------------

   function Create_Reference_With_Id
     (Self : Local_Ref;
      Oid  : ObjectId;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      Error : PolyORB.Errors.Error_Container;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;

      OOid : Object_Id_Access
        := new Object_Id'(PortableServer.Internals.To_PolyORB_Object_Id (Oid));

   begin
      PolyORB.POA.Create_Object_Identification (POA, OOid, U_Oid, Error);
      Free (OOid);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      declare
         A_Oid : aliased PolyORB.POA_Types.Object_Id :=
           PolyORB.POA_Types.U_Oid_To_Oid (U_Oid);

         P_Result : PolyORB.References.Ref;
         C_Result : CORBA.Object.Ref;
      begin
         PolyORB.ORB.Create_Reference
           (PolyORB.Setup.The_ORB,
            A_Oid'Access,
            CORBA.To_Standard_String (Intf),
            P_Result);
         --  Obtain object reference.

         CORBA.Object.Internals.Convert_To_CORBA_Ref
           (P_Result, C_Result);

         return C_Result;
      end;
   end Create_Reference_With_Id;

   --------------------------------------
   -- Create_Request_Processing_Policy --
   --------------------------------------

   function Create_Request_Processing_Policy
     (Value : PortableServer.RequestProcessingPolicyValue)
     return PortableServer.RequestProcessingPolicy.Ref is
   begin
      return PortableServer.RequestProcessingPolicy.To_Ref
        (CORBA.ORB.Create_Policy
         (REQUEST_PROCESSING_POLICY_ID, To_Any (Value)));
   end Create_Request_Processing_Policy;

   -------------------------------------
   -- Create_Servant_Retention_Policy --
   -------------------------------------

   function Create_Servant_Retention_Policy
     (Value : PortableServer.ServantRetentionPolicyValue)
     return PortableServer.ServantRetentionPolicy.Ref is
   begin
      return PortableServer.ServantRetentionPolicy.To_Ref
        (CORBA.ORB.Create_Policy
         (SERVANT_RETENTION_POLICY_ID, To_Any (Value)));
   end Create_Servant_Retention_Policy;

   --------------------------
   -- Create_Thread_Policy --
   --------------------------

   function Create_Thread_Policy
     (Value : PortableServer.ThreadPolicyValue)
     return PortableServer.ThreadPolicy.Ref is
   begin
      return PortableServer.ThreadPolicy.To_Ref
        (CORBA.ORB.Create_Policy
         (THREAD_POLICY_ID, To_Any (Value)));
   end Create_Thread_Policy;

   -----------------------
   -- Deactivate_Object --
   -----------------------

   procedure Deactivate_Object
     (Self : Local_Ref;
      Oid  : ObjectId)
   is
      Error : PolyORB.Errors.Error_Container;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      A_Oid : aliased constant PolyORB.POA_Types.Object_Id :=
        PolyORB.POA_Types.Object_Id
        (PortableServer.Internals.To_PolyORB_Object_Id (Oid));

   begin
      PolyORB.POA.Deactivate_Object (POA, A_Oid, Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Deactivate_Object;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self                : in out Local_Ref;
      Etherealize_Objects : CORBA.Boolean;
      Wait_For_Completion : CORBA.Boolean)
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

   begin

      PolyORB.POA.Destroy
        (POA,
         PolyORB.Types.Boolean (Etherealize_Objects),
         PolyORB.Types.Boolean (Wait_For_Completion));

      --  XXX CORBA Specifications says 'Self' should be an 'in'
      --  parameter; by doing so 'Self' is still a reference to an
      --  invalid POA --> file an issue against the spec to have Ref
      --  converted to an 'in out' arg...

   end Destroy;

   ----------------------------
   -- Extract_Reference_Info --
   ----------------------------

   procedure Extract_Reference_Info
     (Self          : Local_Ref;
      Reference     : CORBA.Object.Ref'Class;
      Ref_Servant   : out Servant;
      Ref_Object_Id : out Object_Id_Access)
   is
      use type PolyORB.Binding_Data.Profile_Access;

      The_Servant : PolyORB.Components.Component_Access;
      The_Profile : PolyORB.Binding_Data.Profile_Access;

      Error : Error_Container;

   begin
      pragma Debug (O ("Extract_Reference_Info: enter"));
      PolyORB.References.Binding.Bind
        (CORBA.Object.Internals.To_PolyORB_Ref
         (CORBA.Object.Ref (Reference)),
         PolyORB.Setup.The_ORB,
         (others => null),
         The_Servant,
         The_Profile,
         Local_Only => True,
         Error      => Error);

      if Found (Error) then
         pragma Debug (O ("Extract_Reference_Info: Bind failed"));
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      --  Check that the reference belongs to local ORB

      if The_Profile = null then
         Raise_WrongAdapter
           (WrongAdapter_Members'
            (CORBA.IDL_Exception_Members with null record),
            "reference does not belong to local ORB");
      end if;

      --  Ensure Reference was actually built by Self

      Ref_Object_Id := PolyORB.Binding_Data.Get_Object_Key (The_Profile.all);

      declare
         use type PolyORB.Types.String;
         U_Oid : PolyORB.POA_Types.Unmarshalled_Oid;
      begin
         PolyORB.POA_Types.Oid_To_U_Oid (Ref_Object_Id.all, U_Oid, Error);

         if Found (Error) then
            pragma Debug
              (O ("Extract_Reference_Info: Oid_To_U_Oid failed"));
            PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
         end if;

         if U_Oid.Creator /= To_POA (Self).Absolute_Address.all then
            pragma Debug
              (O ("Extract_Reference_Info: Wrong adapter"));
            pragma Debug
              (O (PolyORB.Types.To_Standard_String (U_Oid.Creator)));
            pragma Debug
              (O (To_POA (Self).Absolute_Address.all));

            Raise_WrongAdapter
              (WrongAdapter_Members'
               (CORBA.IDL_Exception_Members with null record));
         end if;
      end;

      Ref_Servant := Servant (CORBA.Impl.Internals.To_CORBA_Servant
                                (PolyORB.Servants.Servant_Access
                                  (The_Servant)));
      pragma Debug (O ("Extract_Reference_Info: leave"));
   end Extract_Reference_Info;

   --------------
   -- Find_POA --
   --------------

   function Find_POA
     (Self         : Local_Ref;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
      return Local_Ref'Class
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      POA_Ref : PolyORB.POA.Obj_Adapter_Access;

      Res : Local_Ref;

      Error : Error_Container;
   begin
      PolyORB.POA.Find_POA
        (POA,
         CORBA.To_Standard_String (Adapter_Name),
         Activate_It,
         POA_Ref,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      Res := Internals.To_CORBA_POA (POA_Ref);

      return Res;
   end Find_POA;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= AdapterAlreadyExists'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := AdapterAlreadyExists_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out AdapterNonExistent_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= AdapterNonExistent'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := AdapterNonExistent_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out InvalidPolicy_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= InvalidPolicy'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      PolyORB.Exceptions.User_Get_Members (From, To);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out NoServant_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= NoServant'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := NoServant_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectAlreadyActive_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= ObjectAlreadyActive'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := ObjectAlreadyActive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ObjectNotActive_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= ObjectNotActive'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := ObjectNotActive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ServantAlreadyActive_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= ServantAlreadyActive'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := ServantAlreadyActive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out ServantNotActive_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= ServantNotActive'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := ServantNotActive_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out WrongAdapter_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= WrongAdapter'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := WrongAdapter_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : Ada.Exceptions.Exception_Occurrence;
      To   : out WrongPolicy_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= WrongPolicy'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := WrongPolicy_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   -----------------
   -- Get_Servant --
   -----------------

   function Get_Servant
     (Self : Local_Ref)
     return Servant
   is
      POA     : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);
      Error   : PolyORB.Errors.Error_Container;
      Servant : PolyORB.Servants.Servant_Access;

   begin
      PolyORB.POA.Get_Servant
        (POA,
         Servant,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      return PortableServer.Servant
        (CORBA.Impl.Internals.To_CORBA_Servant (Servant));
   end Get_Servant;

   -------------------------
   -- Get_Servant_Manager --
   -------------------------

   function Get_Servant_Manager
     (Self : Local_Ref)
     return PortableServer.ServantManager.Local_Ref'Class
   is
      use PolyORB.CORBA_P.ServantActivator;
      use PolyORB.CORBA_P.ServantLocator;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Error : Error_Container;

      Manager : ServantManager_Access;

      Result : PortableServer.ServantManager.Local_Ref;

   begin
      PolyORB.POA.Get_Servant_Manager
        (POA,
         Manager,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      if Manager = null then
         return Result;

      else
         if Manager.all in CORBA_ServantActivator'Class then
            return Get_Servant_Manager
              (CORBA_ServantActivator (Manager.all));

         elsif Manager.all in CORBA_ServantLocator'Class then
            return Get_Servant_Manager
              (CORBA_ServantLocator (Manager.all));

         else
            raise Program_Error;
         end if;
      end if;

   end Get_Servant_Manager;

   -----------------------
   -- Get_The_Activator --
   -----------------------

   function Get_The_Activator
     (Self : Local_Ref)
     return PortableServer.AdapterActivator.Ref'Class
   is
      use PolyORB.CORBA_P.AdapterActivator;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Result : PortableServer.AdapterActivator.Ref;
   begin
      if POA.Adapter_Activator /= null then
         pragma Assert (POA.Adapter_Activator.all
                          in CORBA_AdapterActivator'Class);

         return Get_Adapter_Activator
           (CORBA_AdapterActivator (POA.Adapter_Activator.all));
      end if;

      return Result;
   end Get_The_Activator;

   ----------------------
   -- Get_The_Children --
   ----------------------

   function Get_The_Children
     (Self : Local_Ref)
     return POAList
   is
      use PolyORB.POA_Types.POA_Lists;
      use PolyORB.Smart_Pointers;
      use IDL_SEQUENCE_PortableServer_POA_Forward;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Result : POAList;

      POA_List : PolyORB.POA_Types.POA_Lists.List;

   begin
      pragma Debug (O ("Get_The_Children: enter"));

      PolyORB.POA.Get_The_Children (POA, POA_List);

      declare
         It : Iterator := First (POA_List);

      begin
         while not Last (It) loop
            pragma Debug (O ("++"));
            Append (Result,
                    Convert.To_Forward
                    (Internals.To_CORBA_POA
                     (PolyORB.POA.Obj_Adapter_Access
                      (PolyORB.POA_Types.Entity_Of (Value (It).all)))));
            Next (It);
         end loop;
      end;

      Deallocate (POA_List);

      pragma Debug (O ("Get_The_Children: end"));
      return Result;
   end Get_The_Children;

   ------------------
   -- Get_The_Name --
   ------------------

   function Get_The_Name
     (Self : Local_Ref)
     return CORBA.String is
   begin
      return CORBA.To_CORBA_String (To_POA (Self).Name.all);
   end Get_The_Name;

   --------------------
   -- Get_The_Parent --
   --------------------

   function Get_The_Parent (Self : Local_Ref) return Local_Ref'Class is
   begin
      return
        Internals.To_CORBA_POA
        (PolyORB.POA.Obj_Adapter_Access (To_POA (Self).Father));
   end Get_The_Parent;

   ------------------------
   -- Get_The_POAManager --
   ------------------------

   function Get_The_POAManager
     (Self : Local_Ref)
     return PortableServer.POAManager.Local_Ref
   is
      use PolyORB.Smart_Pointers;
      use PortableServer.POAManager;

      Res : PortableServer.POAManager.Local_Ref;

   begin
      pragma Debug (O ("Get_The_POAManager: enter"));

      Set (Res, Entity_Ptr (PolyORB.POA_Manager.Entity_Of
                            (To_POA (Self).POA_Manager)));

      pragma Debug (O ("Get_The_POAManager: leave"));
      return Res;
   end Get_The_POAManager;

   ---------------------
   -- Id_To_Reference --
   ---------------------

   function Id_To_Reference
     (Self : Local_Ref; Oid : ObjectId) return CORBA.Object.Ref
   is
   begin
      return Servant_To_Reference (Self, Id_To_Servant (Self, Oid));
   end Id_To_Reference;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant (Self : Local_Ref; Oid  : ObjectId) return Servant is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Error : PolyORB.Errors.Error_Container;
      S     : PolyORB.Servants.Servant_Access;
   begin
      PolyORB.POA.Id_To_Servant
        (POA,
         PortableServer.Internals.To_PolyORB_Object_Id (Oid),
         S,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      return Servant (CORBA.Impl.Internals.To_CORBA_Servant (S));
   end Id_To_Servant;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Root_POA : PortableServer.POA.Local_Ref;

      Error : PolyORB.Errors.Error_Container;

   begin
      PolyORB.CORBA_P.Exceptions.POA_Raise_From_Error
        := Raise_From_Error'Access;

      PortableServer.POA.Set
        (Root_POA,
         PolyORB.Smart_Pointers.Entity_Ptr
         (PolyORB.ORB.Object_Adapter (PolyORB.Setup.The_ORB)));

      --  By construction, Root POA must be in Hold state

      PolyORB.POA_Manager.Hold_Requests
        (PolyORB.POA_Manager.POAManager_Access
         (PolyORB.POA_Manager.Entity_Of
          (PolyORB.POA.Obj_Adapter
           (PolyORB.ORB.Object_Adapter
            (PolyORB.Setup.The_ORB).all).POA_Manager)),
         False,
         Error);

      CORBA.ORB.Register_Initial_Reference
        (CORBA.ORB.To_CORBA_String ("RootPOA"),
         CORBA.Object.Ref (Root_POA));
   end Initialize;

   ---------------
   -- Internals --
   ---------------

   package body Internals is

      ------------------
      -- To_CORBA_POA --
      ------------------

      function To_CORBA_POA
        (Referenced : PolyORB.POA.Obj_Adapter_Access)
         return Local_Ref
      is
         Res : Local_Ref;

      begin
         Set (Res, PolyORB.Smart_Pointers.Entity_Ptr (Referenced));

         return Res;
      end To_CORBA_POA;

   end Internals;

   --------------------------------
   -- Raise_AdapterAlreadyExists --
   --------------------------------

   procedure Raise_AdapterAlreadyExists
     (Excp_Memb : AdapterAlreadyExists_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);

   begin
      Ada.Exceptions.Raise_Exception (AdapterAlreadyExists'Identity, Message);
   end Raise_AdapterAlreadyExists;

   ------------------------------
   -- Raise_AdapterNonExistent --
   ------------------------------

   procedure Raise_AdapterNonExistent
     (Excp_Memb : AdapterNonExistent_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);

   begin
      Ada.Exceptions.Raise_Exception (AdapterNonExistent'Identity, Message);
   end Raise_AdapterNonExistent;

   ----------------------
   -- Raise_From_Error --
   ----------------------

   procedure Raise_From_Error
     (Error   : in out PolyORB.Errors.Error_Container;
      Message : Standard.String)
   is
   begin
      pragma Debug (O ("Raise_From_Error: enter"));

      pragma Assert (Is_Error (Error));

      --  One to one mapping of PolyORB Error_Id to CORBA POA exceptions.

      case Error.Kind is
         when AdapterAlreadyExists_E =>
            declare
               Member : constant AdapterAlreadyExists_Members :=
                 AdapterAlreadyExists_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_AdapterAlreadyExists (Member, Message);
            end;

         when AdapterNonExistent_E =>
            declare
               Member : constant AdapterNonExistent_Members :=
                 AdapterNonExistent_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_AdapterNonExistent (Member, Message);
            end;

         when InvalidPolicy_E =>
            declare
               Member : constant InvalidPolicy_Members :=
                 InvalidPolicy_Members'
                 (CORBA.IDL_Exception_Members with
                   Index => CORBA.Unsigned_Short
                              (PolyORB.Errors.InvalidPolicy_Members
                               (Error.Member.all).Index));
            begin
               Free (Error.Member);
               Raise_InvalidPolicy (Member, Message);
            end;

         when NoServant_E =>
            declare
               Member : constant NoServant_Members :=
                 NoServant_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_NoServant (Member, Message);
            end;

         when ObjectAlreadyActive_E =>
            declare
               Member : constant ObjectAlreadyActive_Members :=
                 ObjectAlreadyActive_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_ObjectAlreadyActive (Member, Message);
            end;

         when ObjectNotActive_E =>
            declare
               Member : constant ObjectNotActive_Members :=
                 ObjectNotActive_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_ObjectNotActive (Member, Message);
            end;

         when ServantAlreadyActive_E =>
            declare
               Member : constant ServantAlreadyActive_Members :=
                 ServantAlreadyActive_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_ServantAlreadyActive (Member, Message);
            end;

         when ServantNotActive_E =>
            declare
               Member : constant ServantNotActive_Members :=
                 ServantNotActive_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_ServantNotActive (Member, Message);
            end;

         when WrongAdapter_E =>
            declare
               Member : constant WrongAdapter_Members :=
                 WrongAdapter_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_WrongAdapter (Member, Message);
            end;

         when WrongPolicy_E =>
            declare
               Member : constant WrongPolicy_Members :=
                 WrongPolicy_Members'
                 (CORBA.IDL_Exception_Members with null record);
            begin
               Free (Error.Member);
               Raise_WrongPolicy (Member, Message);
            end;

         when others =>
            raise Program_Error;
      end case;
   end Raise_From_Error;

   -------------------------
   -- Raise_InvalidPolicy --
   -------------------------

   procedure Raise_InvalidPolicy
     (Excp_Memb : InvalidPolicy_Members;
      Message   : Standard.String := "")
   is
   begin
      PolyORB.Exceptions.User_Raise_Exception
        (InvalidPolicy'Identity, Excp_Memb, Message);
   end Raise_InvalidPolicy;

   ---------------------
   -- Raise_NoServant --
   ---------------------

   procedure Raise_NoServant
     (Excp_Memb : NoServant_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);

   begin
      Ada.Exceptions.Raise_Exception (NoServant'Identity, Message);
   end Raise_NoServant;

   -------------------------------
   -- Raise_ObjectAlreadyActive --
   -------------------------------

   procedure Raise_ObjectAlreadyActive
     (Excp_Memb : ObjectAlreadyActive_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);

   begin
      Ada.Exceptions.Raise_Exception (ObjectAlreadyActive'Identity, Message);
   end Raise_ObjectAlreadyActive;

   ---------------------------
   -- Raise_ObjectNotActive --
   ---------------------------

   procedure Raise_ObjectNotActive
     (Excp_Memb : ObjectNotActive_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);

   begin
      Ada.Exceptions.Raise_Exception (ObjectNotActive'Identity, Message);
   end Raise_ObjectNotActive;

   --------------------------------
   -- Raise_ServantAlreadyActive --
   --------------------------------

   procedure Raise_ServantAlreadyActive
     (Excp_Memb : ServantAlreadyActive_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);

   begin
      Ada.Exceptions.Raise_Exception (ServantAlreadyActive'Identity, Message);
   end Raise_ServantAlreadyActive;

   ----------------------------
   -- Raise_ServantNotActive --
   ----------------------------

   procedure Raise_ServantNotActive
     (Excp_Memb : ServantNotActive_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);

   begin
      Ada.Exceptions.Raise_Exception (ServantNotActive'Identity, Message);
   end Raise_ServantNotActive;

   ------------------------
   -- Raise_WrongAdapter --
   ------------------------

   procedure Raise_WrongAdapter
     (Excp_Memb : WrongAdapter_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);
   begin
      Ada.Exceptions.Raise_Exception (WrongAdapter'Identity, Message);
   end Raise_WrongAdapter;

   -----------------------
   -- Raise_WrongPolicy --
   -----------------------

   procedure Raise_WrongPolicy
     (Excp_Memb : WrongPolicy_Members;
      Message   : Standard.String := "")
   is
      pragma Unreferenced (Excp_Memb);

   begin
      Ada.Exceptions.Raise_Exception (WrongPolicy'Identity, Message);
   end Raise_WrongPolicy;

   ---------------------
   -- Reference_To_Id --
   ---------------------

   function Reference_To_Id
     (Self      : Local_Ref;
      Reference : CORBA.Object.Ref'Class) return ObjectId
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Ref_Servant   : Servant;
      Ref_Object_Id : Object_Id_Access;
      Result        : Object_Id_Access;
      Error         : Error_Container;
   begin
      Extract_Reference_Info (Self, Reference, Ref_Servant, Ref_Object_Id);

      PolyORB.POA_Policies.Id_Assignment_Policy.Object_Identifier
        (POA.Id_Assignment_Policy.all,
         Ref_Object_Id,
         Result,
         Error);

      if Found (Error) then
         Raise_From_Error (Error, "Reference_To_Id failed to extract oid");
      end if;

      declare
         function To_PSOid (X : Object_Id) return ObjectId
           renames PortableServer.Internals.To_PortableServer_ObjectId;

         Portable_Result : constant ObjectId := To_PSOid (Result.all);
      begin
         Free (Result);
         return Portable_Result;
      end;
   end Reference_To_Id;

   --------------------------
   -- Reference_To_Servant --
   --------------------------

   function Reference_To_Servant
     (Self      : Local_Ref;
      Reference : CORBA.Object.Ref'Class) return Servant
   is
      Ref_Servant   : Servant;
      Ref_Object_Id : Object_Id_Access;
   begin
      Extract_Reference_Info (Self, Reference, Ref_Servant, Ref_Object_Id);
      return Ref_Servant;
   end Reference_To_Servant;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id
     (Self      : Local_Ref;
      P_Servant : Servant)
     return ObjectId
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Oid   : PolyORB.POA_Types.Object_Id_Access;
      Error : PolyORB.Errors.Error_Container;

   begin
      PolyORB.POA.Servant_To_Id
        (POA,
         To_PolyORB_Servant (P_Servant),
         Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      --  XXX Associate an object with domain managers iff it has been
      --  implicitly activated by this call

      Associate_To_Domain_Managers (P_Servant);

      declare
         Result : constant ObjectId
           := PortableServer.Internals.To_PortableServer_ObjectId (Oid.all);

      begin
         PolyORB.POA_Types.Free (Oid);

         return Result;
      end;
   end Servant_To_Id;

   --------------------------
   -- Servant_To_Reference --
   --------------------------

   function Servant_To_Reference
     (Self      : Local_Ref;
      P_Servant : Servant)
     return CORBA.Object.Ref
   is
      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);
      Oid : PolyORB.Objects.Object_Id_Access;

      TID : constant Standard.String :=
        CORBA.To_Standard_String
        (PortableServer.Internals.Get_Type_Id (P_Servant));

      P_Result : PolyORB.References.Ref;
      C_Result : CORBA.Object.Ref;

      Error : PolyORB.Errors.Error_Container;
   begin
      PolyORB.POA.Export
        (POA,
         To_PolyORB_Servant (P_Servant),
         null,
         Oid,
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;

      PolyORB.ORB.Create_Reference
        (PolyORB.Setup.The_ORB, Oid, TID, P_Result);
      --  Obtain object reference.

      PolyORB.POA_Types.Free (Oid);

      --  XXX Associate an object with domain managers iff it has been
      --  implicitly activated by this call

      Associate_To_Domain_Managers (P_Servant);

      CORBA.Object.Internals.Convert_To_CORBA_Ref
        (P_Result, C_Result);

      return C_Result;
   end Servant_To_Reference;

   -----------------
   -- Set_Servant --
   -----------------

   procedure Set_Servant
     (Self      : Local_Ref;
      P_Servant : Servant)
   is
      POA   : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);
      Error : PolyORB.Errors.Error_Container;

   begin
      PolyORB.POA.Set_Servant
        (POA,
         PolyORB.Servants.Servant_Access (To_PolyORB_Servant (P_Servant)),
         Error);

      if Found (Error) then
         PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
      end if;
   end Set_Servant;

   -------------------------
   -- Set_Servant_Manager --
   -------------------------

   procedure Set_Servant_Manager
     (Self : Local_Ref;
      Imgr : PortableServer.ServantManager.Local_Ref'Class)
   is
      use PolyORB.CORBA_P.ServantActivator;
      use PolyORB.CORBA_P.ServantLocator;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);

      Error : Error_Container;
   begin
      if POA.Servant_Manager /= null then
         CORBA.Raise_Bad_Inv_Order
           (CORBA.System_Exception_Members'
            (Minor     => 6,
             Completed => CORBA.Completed_No));
      end if;

      if CORBA.Object.Is_A
         (CORBA.Object.Ref (Imgr),
          PortableServer.ServantActivator.Repository_Id)
      then
         declare
            CORBA_Servant_Manager : ServantActivator_Access;

         begin
            PolyORB.CORBA_P.ServantActivator.Create
              (CORBA_Servant_Manager,
               PortableServer.ServantActivator.Local_Ref (Imgr));

            PolyORB.POA.Set_Servant_Manager
              (POA,
               ServantManager_Access (CORBA_Servant_Manager),
               Error);

            if Found (Error) then
               PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
            end if;
         end;

      elsif CORBA.Object.Is_A
            (CORBA.Object.Ref (Imgr),
             PortableServer.ServantLocator.Repository_Id)
      then
         declare
            CORBA_Servant_Manager : ServantLocator_Access;

         begin
            Create
              (CORBA_Servant_Manager,
               PortableServer.ServantLocator.Local_Ref (Imgr));

            PolyORB.POA.Set_Servant_Manager
              (POA,
               ServantManager_Access (CORBA_Servant_Manager),
               Error);

            if Found (Error) then
               PolyORB.CORBA_P.Exceptions.Raise_From_Error (Error);
            end if;
         end;

      else
         CORBA.Raise_Obj_Adapter
           (CORBA.System_Exception_Members'
            (Minor     => 4,
             Completed => CORBA.Completed_No));
      end if;
   end Set_Servant_Manager;

   -----------------------
   -- Set_The_Activator --
   -----------------------

   procedure Set_The_Activator
     (Self : Local_Ref;
      To   : access PortableServer.AdapterActivator.Ref'Class)
   is
      use PolyORB.CORBA_P.AdapterActivator;

      POA : constant PolyORB.POA.Obj_Adapter_Access := To_POA (Self);
   begin
      if POA.Adapter_Activator /= null then
         Free (POA.Adapter_Activator);
      end if;

      Create (POA.Adapter_Activator, To);
   end Set_The_Activator;

   ------------
   -- To_POA --
   ------------

   function To_POA
     (Self : Local_Ref)
     return PolyORB.POA.Obj_Adapter_Access
   is
      use PolyORB.Smart_Pointers;

      Res : constant PolyORB.Smart_Pointers.Entity_Ptr :=
        Entity_Of (Self);

   begin
      if Res = null
        or else Res.all not in PolyORB.POA.Obj_Adapter'Class then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      declare
         use PolyORB.POA_Manager;

         The_POA : constant PolyORB.POA.Obj_Adapter_Access :=
           PolyORB.POA.Obj_Adapter_Access (Res);
      begin
         if Is_Nil (The_POA.POA_Manager) then
            CORBA.Raise_Object_Not_Exist (CORBA.Default_Sys_Member);
         end if;

         return The_POA;
      end;
   end To_POA;

   use PolyORB.Initialization;
   use PolyORB.Initialization.String_Lists;
   use PolyORB.Utils.Strings;

begin
   Register_Module
     (Module_Info'
      (Name      => +"portableserver.poa",
       Conflicts => Empty,
       Depends   => +"poa",
       Provides  => Empty,
       Implicit  => False,
       Init      => Initialize'Access,
       Shutdown  => null));
end PortableServer.POA;
