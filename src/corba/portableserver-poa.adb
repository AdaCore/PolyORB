------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   P O R T A B L E S E R V E R . P O A                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2001 ENST Paris University, France.          --
--                                                                          --
-- AdaBroker is free software; you  can  redistribute  it and/or modify it  --
-- under terms of the  GNU General Public License as published by the  Free --
-- Software Foundation;  either version 2,  or (at your option)  any  later --
-- version. AdaBroker  is distributed  in the hope that it will be  useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License  for more details.  You should have received  a copy of the GNU  --
-- General Public License distributed with AdaBroker; see file COPYING. If  --
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
--             AdaBroker is maintained by ENST Paris University.            --
--                     (email: broker@inf.enst.fr)                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Exceptions;

with Droopi.ORB;
with Droopi.POA;
with Droopi.POA_Manager;
with Droopi.References;
with Droopi.Setup;
with Droopi.Smart_Pointers;

with Droopi.CORBA_P.Exceptions;

--  with PortableServer.ServantManager.Impl;
--  with PortableServer.ServantActivator.Impl;
--  with PortableServer.ServantLocator.Impl;

package body PortableServer.POA is

   function Create_Ref
     (Referenced : Droopi.Smart_Pointers.Entity_Ptr) return Ref;

   function Create_Ref
     (Referenced : Droopi.Smart_Pointers.Entity_Ptr) return Ref
   is
      Res : Ref;
   begin
      Set (Res, Referenced);
      return Res;
   end Create_Ref;

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members)
   is
      use Ada.Exceptions;
   begin
      if Exception_Identity (From) /= AdapterAlreadyExists'Identity then
         Droopi.CORBA_P.Exceptions.Raise_Bad_Param;
      end if;
      To := AdapterAlreadyExists_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   procedure Get_Members (From : in CORBA.Exception_Occurrence;
                          To   : out AdapterNonExistent_Members)
   is
      use Ada.Exceptions;
   begin
      if Exception_Identity (From) /= AdapterNonExistent'Identity then
         Droopi.CORBA_P.Exceptions.Raise_Bad_Param;
      end if;
      To := AdapterNonExistent_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   function To_Ref (Self : CORBA.Object.Ref'Class) return Ref is
   begin
      if CORBA.Object.Entity_Of (Self).all
        not in Droopi.POA.Obj_Adapter'Class then
         Droopi.CORBA_P.Exceptions.Raise_Bad_Param;
      end if;
      return Create_Ref (CORBA.Object.Entity_Of (Self));
   end To_Ref;

   function To_POA
     (Self : Ref)
     return Droopi.POA.Obj_Adapter_Ptr;

   function To_POA
     (Self : Ref)
     return Droopi.POA.Obj_Adapter_Ptr
   is
      use Droopi.Smart_Pointers;

      Res : constant Droopi.Smart_Pointers.Entity_Ptr
        := Entity_Of (Self);

   begin
      if Res = null or else Res.all not in Droopi.POA.Obj_Adapter'Class then
         Droopi.CORBA_P.Exceptions.Raise_Bad_Param;
      end if;

      declare
         use Droopi.POA_Manager;

         The_POA : constant Droopi.POA.Obj_Adapter_Ptr
           := Droopi.POA.Obj_Adapter_Ptr (Res);
      begin
         if The_POA.POA_Manager = null then
            Droopi.CORBA_P.Exceptions.Raise_Object_Not_Exist;
         end if;

         return The_POA;
      end;
   end To_POA;

   function Get_The_Name (Self : Ref) return CORBA.String is
   begin
      return To_POA (Self).Name;
   end Get_The_Name;

   function Get_The_Parent (Self : Ref) return Ref'Class is
   begin
      return Ref'
        (Create_Ref
         (Droopi.Smart_Pointers.Entity_Ptr
          (To_POA (Self).Father)));
   end Get_The_Parent;

   function Get_The_POAManager
     (Self : Ref)
     return PortableServer.POAManager.Ref
   is
      use PortableServer.POAManager;

      Res : PortableServer.POAManager.Ref;

   begin
      Set (Res, Droopi.Smart_Pointers.Entity_Ptr
           (To_POA (Self).POA_Manager));
      return Res;
   end Get_The_POAManager;

   function Get_Servant_Manager
     (Self : Ref)
     return PortableServer.ServantManager.Ref
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);

   begin
--       if POA.Request_Processing_Policy /= USE_SERVANT_MANAGER then
--          raise WrongPolicy;
--       end if;
--       return POA.Servant_Manager;

      --  return USE_SERVANT_MANAGER
      --    (POA.Request_Processing_Policy).Servant_Manager
      raise Droopi.Not_Implemented;
      return Get_Servant_Manager (Self);
   end Get_Servant_Manager;

   procedure Set_Servant_Manager
     (Self : in Ref;
      Imgr : in PortableServer.ServantManager.Ref)
   is
--       package PSSM renames PortableServer.ServantManager;
--       package PSSA renames PortableServer.ServantActivator;
--       package PSSL renames PortableServer.ServantLocator;

      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);
--       Servant_Manager : constant PSSM.Impl.Object_Ptr
--         := PSSM.Impl.Object_Ptr (PSSM.Entity_Of (Imgr));

   begin
--       if POA.Request_Processing_Policy /= USE_SERVANT_MANAGER then
--          raise WrongPolicy;
--       end if;

--       if True
--         and then not PSSM.Is_Nil (Imgr)
--         and then
--         ((POA.Servant_Policy = RETAIN
--           and then Servant_Manager.all not in PSSA.Impl.Object'Class)
--         or else
--          (POA.Servant_Policy = NON_RETAIN
--           and then Servant_Manager.all not in PSSL.Impl.Object'Class))
--       then
--          Droopi.CORBA_P.Exceptions.Raise_Bad_Param;
--       end if;

--       POA.Servant_Manager := Imgr;
      raise Droopi.Not_Implemented;
   end Set_Servant_Manager;

   function Get_The_Activator
     (Self : Ref)
     return PortableServer.AdapterActivator.Ref is
   begin
      --  return To_POA (Self).Activator;
      raise Droopi.Not_Implemented;
      return Get_The_Activator (Self);
   end Get_The_Activator;

   procedure Set_The_Activator
     (Self : in Ref;
      To   : in PortableServer.AdapterActivator.Ref) is
   begin
      --  To_POA (Self).Activator := To;
      raise Droopi.Not_Implemented;
   end Set_The_Activator;

   ----------------
   -- Create_POA --
   ----------------

   function Create_POA
     (Self         : Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Ref;
      Tp           : ThreadPolicyValue;
      Lp           : LifespanPolicyValue;
      Up           : IdUniquenessPolicyValue;
      Ip           : IdAssignmentPolicyValue;
      Ap           : ImplicitActivationPolicyValue;
      Sp           : ServantRetentionPolicyValue;
      Rp           : RequestProcessingPolicyValue)
     return Ref'Class
   is
      pragma Warnings (Off);
      Res : Droopi.POA.Obj_Adapter_Ptr;
      --  XXX Never assigned a value.
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);
      --  XXX Never referenced.
      pragma Warnings (On);
   begin
      --  Note - The NON_RETAIN policy requires either the USE_DEFAULT_SERVANT
      --  or USE_SERVANT_MANAGER policies.

      if (Sp = NON_RETAIN
          and then Rp /= USE_DEFAULT_SERVANT
          and then Rp /= USE_SERVANT_MANAGER)
        or else
         (Rp = USE_ACTIVE_OBJECT_MAP_ONLY
          and then Sp /= RETAIN)
        or else
         (Rp = USE_DEFAULT_SERVANT
          and then Up /= MULTIPLE_ID)
        or else
         (Ap = IMPLICIT_ACTIVATION
          and then (Ip /= SYSTEM_ID or else Sp /= RETAIN))
      then
         Droopi.CORBA_P.Exceptions.Raise_Bad_Param;
      end if;

--       begin
--          Lock_W (All_POAs_Lock);
--          Res := Droopi.POA.Create_POA
--            (POA,
--             Adapter_Name,
--             POAManager_Object_Ptr
--             (PortableServer.POAManager.Entity_Of (A_POAManager)),
--             Tp, Lp, Up, Ip, Ap, Sp, Rp);
--          Unlock_W (All_POAs_Lock);
--          return Create_Ref (Droopi.Smart_Pointers.Entity_Ptr (Res));
--       exception
--          when others =>
--             Unlock_W (All_POAs_Lock);
--             raise;
--       end;
      raise Droopi.Not_Implemented;
      return Create_Ref (null);
   end Create_POA;

   --------------
   -- Find_POA --
   --------------

   function Find_POA
     (Self         : Ref;
      Adapter_Name : CORBA.String;
      Activate_It  : CORBA.Boolean)
      return Ref'Class
   is
--       The_POA : constant Droopi.POA.Obj_Adapter_Ptr
--         := To_POA (Self);
   begin
--       Lock_W (All_POAs_Lock);
--       declare
--          POA_Ref : constant Droopi.POA.Ref'Class
--            := Droopi.POA.Find_POA
--            (The_POA, Adapter_Name, Activate_It);

--          Res : Ref
--            := Create_Ref (Droopi.Smart_Pointers.Entity_Ptr
--                           (POA_Object_Of (POA_Ref)));
--       begin
--          Unlock_W (All_POAs_Lock);

--          if Is_Nil (Res) then
--             raise AdapterNonExistent;
--          end if;

--          return Res;
--       end;
      raise Droopi.Not_Implemented;
      return Create_Ref (null);
   end Find_POA;

   -------------
   -- Destroy --
   -------------

   procedure Destroy
     (Self                : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean)
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);

   begin
      --  Droopi.POA.Destroy_POA
      --    (POA, Etherealize_Objects, Wait_For_Completion);
      --  FIXME: Huh, SELF is still a reference to an invalid POA.
      --    --> file an issue against the spec to have Ref converted
      --        to an 'in out' arg...
      raise Droopi.Not_Implemented;
   end Destroy;

   -----------------
   -- Get_Servant --
   -----------------

   function Get_Servant
     (Self : Ref)
     return Servant
   is
--       POA : constant Droopi.POA.Obj_Adapter_Ptr
--         := To_POA (Self);

   begin
--       if POA.Request_Processing_Policy /= USE_DEFAULT_SERVANT then
--          raise WrongPolicy;
--       end if;

--       if POA.Default_Servant = null then
--          raise NoServant;
--       end if;

--       return POA.Default_Servant;
      raise Droopi.Not_Implemented;
      return Get_Servant (Self);
   end Get_Servant;

   -----------------
   -- Set_Servant --
   -----------------

   procedure Set_Servant
     (Self      : in Ref;
      P_Servant : in Servant)
   is
--       POA : constant Droopi.POA.Obj_Adapter_Ptr
--         := To_POA (Self);

   begin
--       if POA.Request_Processing_Policy /= USE_DEFAULT_SERVANT then
--          raise WrongPolicy;
--       end if;

--       POA.Default_Servant := P_Servant;
      raise Droopi.Not_Implemented;
   end Set_Servant;

   ---------------------
   -- Activate_Object --
   ---------------------

   function Activate_Object
     (Self      : Ref;
      P_Servant : Servant)
     return ObjectId
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);

   begin
--       --  Cf 9-34: this operation requires SYSTEM_ID and RETAIN policies.
--       if POA.Id_Assign_Policy /= SYSTEM_ID
--         or else POA.Servant_Policy /= RETAIN
--       then
--          raise WrongPolicy;
--       end if;

--       return Droopi.POA.Activate_Object (POA, P_Servant);
      raise Droopi.Not_Implemented;
      return Activate_Object (Self, P_Servant);
   end Activate_Object;

   -----------------------------
   -- Activate_Object_With_Id --
   -----------------------------

   procedure Activate_Object_With_Id
     (Self      : in Ref;
      Oid       : in ObjectId;
      P_Servant : in Servant)
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);

   begin
--       --  Cf 9-34: this operation requires RETAIN policy.
--       if POA.Servant_Policy /= RETAIN then
--          raise WrongPolicy;
--       end if;

--       Droopi.POA.Activate_Object_With_Id (POA, Oid, P_Servant);
      raise Droopi.Not_Implemented;
   end Activate_Object_With_Id;

   -----------------------
   -- Deactivate_Object --
   -----------------------

   procedure Deactivate_Object
     (Self : in Ref;
      Oid  : in ObjectId)
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);

   begin
--       --  Cf 9-34: this operation requires RETAIN policy.
--       if POA.Servant_Policy /= RETAIN then
--          raise WrongPolicy;
--       end if;

--       Droopi.POA.Deactivate_Object (POA, Oid);
      raise Droopi.Not_Implemented;
   end Deactivate_Object;

   ----------------------
   -- Create_Reference --
   ----------------------

   function Create_Reference
     (Self : Ref;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);

   begin
--       if POA.Id_Assign_Policy /= SYSTEM_ID then
--          raise WrongPolicy;
--       end if;

--       return Droopi.POA.Create_Reference (POA, Intf);
      raise Droopi.Not_Implemented;
      return Create_Reference (Self, Intf);
   end Create_Reference;

   ------------------------------
   -- Create_Reference_With_Id --
   ------------------------------

   function Create_Reference_With_Id
     (Self : Ref;
      Oid  : ObjectId;
      Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);

   begin
      raise Droopi.Not_Implemented;
      return Create_Reference_With_Id (Self, Oid, Intf);
   end Create_Reference_With_Id;

   -------------------
   -- Servant_To_Id --
   -------------------

   function Servant_To_Id
     (Self      : Ref;
      P_Servant : Servant)
     return ObjectId
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);

   begin
--       if POA.Request_Processing_Policy /= USE_DEFAULT_SERVANT
--         and then
--         (POA.Servant_Policy /= RETAIN
--          or else (POA.Uniqueness_Policy /= UNIQUE_ID
--                   and then POA.Activation_Policy /= IMPLICIT_ACTIVATION))
--       then
--          raise WrongPolicy;
--       end if;

--       return Droopi.POA.Servant_To_Skeleton (POA, P_Servant).Object_Id;
      raise Droopi.Not_Implemented;
      return Servant_To_Id (Self, P_Servant);
   end Servant_To_Id;

   --------------------------
   -- Servant_To_Reference --
   --------------------------

   function Servant_To_Reference
     (Self : Ref; P_Servant : Servant)
     return CORBA.Object.Ref
   is
      POA  : constant Droopi.POA.Obj_Adapter_Ptr := To_POA (Self);
      Oid : constant Droopi.Objects.Object_Id_Access
        := new Droopi.Objects.Object_Id'
        (Droopi.POA.Export (POA, To_Droopi_Servant (P_Servant)));
      --  XXX
      --  1. There is a possible memory leak here. Who free's
      --     the allocation for Oid?
      --  2. There is a pending possibility that Export is incorrect.
      --     See caveats in its body.

      The_Ref : Droopi.References.Ref;
      The_Ref_Info : constant Droopi.Smart_Pointers.Entity_Ptr
        := new CORBA.Object.Reference_Info;
      Result : CORBA.Object.Ref;
--      Skel : Droopi.POA.Skeleton_Ptr;
   begin
--       --  FIXME: If Servant_To_Reference is called in the context
--       --    of executing a request on the given servant, there are
--       --    no constraints on the POA's policies. (11.3.8.21).
--       if POA.Servant_Policy /= RETAIN
--         or else (POA.Uniqueness_Policy /= UNIQUE_ID
--                  and then POA.Activation_Policy /= IMPLICIT_ACTIVATION)
--       then
--          raise WrongPolicy;
--       end if;

--       Skel := Servant_To_Skeleton
--         (POA, P_Servant, Called_From_Servant_To_Reference => True);

--       return Droopi.POA.Skeleton_To_Ref (Skel.all);

      Droopi.ORB.Create_Reference (Droopi.Setup.The_ORB, Oid, The_Ref);
      --  Obtain object reference.

      CORBA.Object.Reference_Info (The_Ref_Info.all).IOR :=
        (Ref     => The_Ref,
         Type_Id => CORBA.To_CORBA_String (""));
      --  XXX Type_Id should be obtained by Servant.If_Desc.External_Name
      --  *if* Servant was a Droopi.POA_Types.Servant. Unfortunately, Servant
      --  is a PortableServer.Servant_Base'Class, which has nothing in common
      --  with Droopi.POA_Types.Servant.
      --  -> should use Servant.Neutral_View.If_Desc.External_Name.

      CORBA.Object.Set (Result, The_Ref_Info);
      return Result;
   end Servant_To_Reference;

   ---------------------
   -- Reference_To_Id --
   ---------------------

   function Reference_To_Id
     (Self : Ref;
      Reference : CORBA.Object.Ref'Class) return ObjectId
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr := To_POA (Self);
      --  Skel : Droopi.POA.Skeleton_Ptr;

   begin
--       Skel := Droopi.POA.Ref_To_Skeleton (Reference);
--       if POA_Object_Of (Skel.POA) /= POA_Object_Ptr (POA) then
--          raise WrongAdapter;
--       end if;

--       return Skel.Object_Id;
      raise Droopi.Not_Implemented;
      return Reference_To_Id (Self, Reference);
   end Reference_To_Id;

   --------------------------
   -- Reference_To_Servant --
   --------------------------

   function Reference_To_Servant
     (Self      : Ref;
      Reference : CORBA.Object.Ref'Class)
     return Servant
   is
      POA  : constant Droopi.POA.Obj_Adapter_Ptr := To_POA (Self);
      --  Skel : Droopi.POA.Skeleton_Ptr;

   begin
--       Skel := Droopi.POA.Ref_To_Skeleton (Reference);
--       if POA_Object_Of (Skel.POA) /= POA_Object_Ptr (POA) then
--          raise WrongAdapter;
--       end if;

--       if POA.Servant_Policy /= RETAIN
--         and then POA.Request_Processing_Policy /= USE_DEFAULT_SERVANT
--       then
--          raise WrongPolicy;
--       end if;

--       return Droopi.POA.Skeleton_To_Servant (POA, Skel);
      raise Droopi.Not_Implemented;
      return Reference_To_Servant (Self, Reference);
   end Reference_To_Servant;

   -------------------
   -- Id_To_Servant --
   -------------------

   function Id_To_Servant
     (Self : Ref;
      Oid  : ObjectId)
     return Servant
   is
      POA : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);
      --  Skel : Droopi.POA.Skeleton_Ptr;

   begin
--       if POA.Servant_Policy /= RETAIN then
--          raise WrongPolicy;
--       end if;

--       Skel := Droopi.POA.Id_To_Skeleton (POA, Oid);
--       if Skel.P_Servant = null then
--          raise PortableServer.POA.ObjectNotActive;
--       end if;

--       return Skel.P_Servant;
      raise Droopi.Not_Implemented;
      return Id_To_Servant (Self, Oid);
   end Id_To_Servant;

   ---------------------
   -- Id_To_Reference --
   ---------------------

   function Id_To_Reference
     (Self : Ref; Oid : ObjectId)
     return CORBA.Object.Ref
   is
      POA  : constant Droopi.POA.Obj_Adapter_Ptr
        := To_POA (Self);
      --  Skel : Droopi.POA.Skeleton_Ptr;

   begin
--       if POA.Servant_Policy /= RETAIN then
--          raise WrongPolicy;
--       end if;

--       Skel := Droopi.POA.Id_To_Skeleton (POA, Oid);
--       if Skel.P_Servant = null then
--          raise PortableServer.POA.ObjectNotActive;
--       end if;

--       return Droopi.POA.Skeleton_To_Ref (Skel.all);
      raise Droopi.Not_Implemented;
      return Id_To_Reference (Self, Oid);
   end Id_To_Reference;

end PortableServer.POA;
