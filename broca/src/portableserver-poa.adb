------------------------------------------------------------------------------
--                                                                          --
--                          ADABROKER COMPONENTS                            --
--                                                                          --
--                   P O R T A B L E S E R V E R . P O A                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.8 $
--                                                                          --
--            Copyright (C) 1999 ENST Paris University, France.             --
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

with Broca.Refs;
with Broca.Exceptions;
with Broca.POA;
with PortableServer.ServantActivator.Impl;
with PortableServer.ServantLocator.Impl;

package body Portableserver.POA is
   function Create_Ref (Referenced : Broca.Refs.Ref_Ptr) return Ref;

   function Create_Ref (Referenced : Broca.Refs.Ref_Ptr) return Ref is
      use Broca.Refs;
      Res : Ref;
   begin
      Set (Broca.Refs.Ref (Res), Referenced);
      return Res;
   end Create_Ref;

   procedure Get_Members
     (From : in CORBA.Exception_Occurrence;
      To   : out AdapterAlreadyExists_Members)
   is
      use Ada.Exceptions;
   begin
      if Exception_Identity (From) /= AdapterAlreadyExists'Identity then
         Broca.Exceptions.Raise_Bad_Param;
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
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      To := AdapterNonExistent_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   function To_Ref (Self : CORBA.Object.Ref) return Ref is
   begin
      if CORBA.Object.Get (Self).all not in Broca.POA.POA_Object'Class then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      return Create_Ref (CORBA.Object.Get (Self));
   end To_Ref;

   function To_Poa (Self : Ref) return Broca.POA.POA_Object_Ptr;

   function To_Poa (Self : Ref) return Broca.POA.POA_Object_Ptr is
      use Broca.POA;
      use Broca.Refs;
      Res_Ref : Broca.Refs.Ref_Ptr;
      Res : Broca.POA.POA_Object_Ptr;
   begin
      Res_Ref := Get (Self);
      if Res_Ref = null
        or else Res_Ref.all not in Broca.POA.POA_Object'Class
      then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      Res := Broca.POA.POA_Object_Ptr (Res_Ref);
      if Res.POA_Manager = null then
         Broca.Exceptions.Raise_Object_Not_Exist;
      else
         return Res;
      end if;
   end To_Poa;

   function Get_The_Name (Self : Ref) return CORBA.String is
   begin
      return To_Poa (Self).Name;
   end Get_The_Name;

   function Get_The_Parent (Self : Ref) return Ref'Class is
   begin
      return Ref'(Create_Ref (Broca.Refs.Ref_Ptr (To_Poa (Self).Parent)));
   end Get_The_Parent;

   function Get_The_POAManager (Self : Ref)
                                return PortableServer.POAManager.Ref is
      use Broca.POA;
      use PortableServer.POAManager;
      Res : PortableServer.POAManager.Ref;
   begin
      Set (Res,
           Broca.Refs.Ref_Ptr (Broca.POA.Get_The_POAManager (To_Poa (Self))));
      return Res;
   end Get_The_POAManager;

   function Get_Servant_Manager (Self : Ref)
                                 return PortableServer.ServantManager.Ref
   is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);
      if POA.Request_Policy /= USE_SERVANT_MANAGER then
         raise WrongPolicy;
      end if;
      return POA.Servant_Manager;
   end Get_Servant_Manager;

   procedure Set_Servant_Manager
     (Self : in Ref; Imgr : in PortableServer.ServantManager.Ref)
   is
      POA : Broca.POA.POA_Object_Ptr;
      Skel : Broca.POA.Internal_Skeleton_Ptr;
   begin
      POA := To_Poa (Self);
      if POA.Request_Policy /= USE_SERVANT_MANAGER then
         raise WrongPolicy;
      end if;
      Skel := Broca.POA.To_Internal_Skeleton (Imgr);
      if Skel.P_Servant /= null then
         case POA.Servant_Policy is
            when RETAIN =>
               if Skel.P_Servant.all not in
                 PortableServer.ServantActivator.Impl.Object'Class
               then
                  Broca.Exceptions.Raise_Bad_Param;
               end if;
            when NON_RETAIN =>
               if Skel.P_Servant.all not in
                 PortableServer.ServantLocator.Impl.Object'Class
               then
                  Broca.Exceptions.Raise_Bad_Param;
               end if;
         end case;
      end if;
      POA.Servant_Manager := Imgr;
   end Set_Servant_Manager;

   function Get_The_Activator (Self : Ref)
                               return PortableServer.AdapterActivator.Ref is
   begin
      return To_Poa (Self).Activator;
   end Get_The_Activator;

   procedure Set_The_Activator
     (Self : in Ref;
      To   : in PortableServer.AdapterActivator.Ref) is
   begin
      To_Poa (Self).Activator := To;
   end Set_The_Activator;

   function Create_POA
     (Self         : Ref;
      Adapter_Name : CORBA.String;
      A_POAManager : PortableServer.POAManager.Ref;
      Tp           : ThreadPolicyValue;
      Lp : LifespanPolicyValue;
      Up : IdUniquenessPolicyValue;
      Ip : IdAssignmentPolicyValue;
      Ap : ImplicitActivationPolicyValue;
      Sp : ServantRetentionPolicyValue;
      Rp : RequestProcessingPolicyValue)
     return Ref'Class
   is
      use Broca.POA;
      Res : Broca.POA.POA_Object_Ptr;
      POA : Broca.POA.POA_Object_Ptr;
   begin
      --  Note - The NON_RETAIN policy requires either the USE_DEFAULT_SERVANT
      --  or USE_SERVANT_MANAGER policies.
      if Sp = NON_RETAIN and (Rp /= USE_DEFAULT_SERVANT
                              and then Rp /= USE_SERVANT_MANAGER)
      then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      if Rp = USE_ACTIVE_OBJECT_MAP_ONLY and then Sp /= RETAIN then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      if Rp = USE_DEFAULT_SERVANT and then Up /= MULTIPLE_ID then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      if Ap = IMPLICIT_ACTIVATION and then (Ip /= SYSTEM_ID
                                            or else Sp /= RETAIN)
      then
         Broca.Exceptions.Raise_Bad_Param;
      end if;

      POA := To_Poa (Self);
      begin
         All_POAs_Lock.Lock_W;
         Res := Broca.POA.Create_POA
           (POA,
            Adapter_Name,
            POAManager_Object_Ptr
            (PortableServer.POAManager.Get (A_POAManager)),
            Tp, Lp, Up, Ip, Ap, Sp, Rp);
         All_POAs_Lock.Unlock_W;
         return Create_Ref (Broca.Refs.Ref_Ptr (Res));
      exception
         when others =>
            All_POAs_Lock.Unlock_W;
            raise;
      end;
   end Create_POA;

   function Find_POA
     (Self : Ref; Adapter_Name : CORBA.String; Activate_It : CORBA.Boolean)
      return Ref'Class
   is
      use Broca.POA;
      Res : Broca.POA.POA_Object_Ptr;
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);
      All_POAs_Lock.Lock_W;
      Res := Broca.POA.Find_POA (POA, Adapter_Name, Activate_It);
      All_POAs_Lock.Unlock_W;
      if Res = null then
         raise AdapterNonExistent;
      else
         return Create_Ref (Broca.Refs.Ref_Ptr (Res));
      end if;
   end Find_POA;

   procedure Destroy
     (Self                : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean)
   is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);
      Broca.POA.Destroy_POA (POA, Etherealize_Objects, Wait_For_Completion);
      --  FIXME: Huh, SELF is still a reference to an invalid POA.
   end Destroy;

   function Get_Servant (Self : Ref) return Servant is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);
      if POA.Request_Policy /= USE_DEFAULT_SERVANT then
         raise WrongPolicy;
      end if;
      if POA.Default_Servant = null then
         raise NoServant;
      else
         return POA.Default_Servant;
      end if;
   end Get_Servant;

   procedure Set_Servant (Self : in Ref; P_Servant : in Servant) is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);
      if POA.Request_Policy /= USE_DEFAULT_SERVANT then
         raise WrongPolicy;
      end if;
      POA.Default_Servant := P_Servant;
   end Set_Servant;

   function Activate_Object (Self : Ref; P_Servant : Servant)
                             return ObjectId is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);

      --  Cf 9-34: this operation requires SYSTEM_ID and RETAIN policies.
      if POA.Id_Assign_Policy /= SYSTEM_ID
        or else POA.Servant_Policy /= RETAIN
      then
         raise WrongPolicy;
      end if;

      return Broca.POA.Activate_Object (POA, P_Servant);
   end Activate_Object;

   procedure Activate_Object_With_Id
     (Self : in Ref; Oid : in ObjectId; P_Servant : in Servant) is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);

      --  Cf 9-34: this operation requires RETAIN policy.
      if POA.Servant_Policy /= RETAIN then
         raise WrongPolicy;
      end if;

      Broca.POA.Activate_Object_With_Id (POA, Oid, P_Servant);
   end Activate_Object_With_Id;

   procedure Deactivate_Object (Self : in Ref; Oid : in ObjectId) is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);

      --  Cf 9-34: this operation requires RETAIN policy.
      if POA.Servant_Policy /= RETAIN then
         raise WrongPolicy;
      end if;

      Broca.POA.Deactivate_Object (POA, Oid);
   end Deactivate_Object;

   function Create_Reference (Self : Ref; Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);
      if POA.Id_Assign_Policy /= SYSTEM_ID then
         raise WrongPolicy;
      end if;
      return Broca.POA.Create_Reference (POA, Intf);
   end Create_Reference;

   function Create_Reference_With_Id
     (Self : Ref; Oid : ObjectId; Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);
      return Broca.POA.Create_Reference_With_Id (POA, Oid, Intf);
   end Create_Reference_With_Id;

   function Servant_To_Id (Self : Ref; P_Servant : Servant) return ObjectId
   is
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);
      if POA.Servant_Policy /= RETAIN
        or else (POA.Uniqueness_Policy /= UNIQUE_ID
                 and then POA.Activation_Policy /= IMPLICIT_ACTIVATION)
      then
         raise WrongPolicy;
      end if;
      return Broca.POA.Servant_To_Id (POA, P_Servant);
   end Servant_To_Id;

   function Servant_To_Reference (Self : Ref; P_Servant : Servant)
                                  return CORBA.Object.Ref
   is
      Res : CORBA.Object.Ref;
      POA : Broca.POA.POA_Object_Ptr;
   begin
      POA := To_Poa (Self);
      if POA.Servant_Policy /= RETAIN
        or else (POA.Uniqueness_Policy /= UNIQUE_ID
                 and then POA.Activation_Policy /= IMPLICIT_ACTIVATION)
      then
         raise WrongPolicy;
      end if;
      CORBA.Object.Set
        (Res,
         Broca.Refs.Ref_Ptr (Broca.POA.Servant_To_Skeleton (POA, P_Servant)));
      return Res;
   end Servant_To_Reference;

   function Reference_To_Id
     (Self : Ref; Reference : CORBA.Object.Ref'CLASS) return ObjectId
   is
      use Broca.POA;
      POA : Broca.POA.POA_Object_Ptr;
      Skel : Broca.POA.Skeleton_Ptr;
   begin
      POA := To_Poa (Self);
      Skel := Broca.POA.To_Skeleton (Reference);
      if Skel.POA /= POA_Object_Ptr (POA) then
         raise WrongAdapter;
      end if;
      return Skel.Object_Id;
   end Reference_To_Id;

   function Reference_To_Servant
     (Self : Ref; Reference : CORBA.Object.Ref'CLASS) return Servant
   is
      use Broca.POA;
      POA : Broca.POA.POA_Object_Ptr;
      Skel : Broca.POA.Skeleton_Ptr;
   begin
      POA := To_Poa (Self);
      Skel := Broca.POA.To_Skeleton (Reference);
      if Skel.POA /= POA_Object_Ptr (POA) then
         raise WrongAdapter;
      end if;
      if POA.Servant_Policy /= RETAIN
        and then POA.Request_Policy /= USE_DEFAULT_SERVANT
      then
         raise WrongPolicy;
      end if;
      return Broca.POA.Skeleton_To_Servant (POA, Skel);
   end Reference_To_Servant;

   function Id_To_Servant (Self : Ref; Oid : ObjectId) return Servant
   is
      POA : Broca.POA.POA_Object_Ptr;
      Skel : Broca.POA.Skeleton_Ptr;
   begin
      POA := To_Poa (Self);
      if POA.Servant_Policy /= RETAIN then
         raise WrongPolicy;
      end if;
      Skel := Broca.POA.Id_To_Skeleton (POA, Oid);
      if Skel.P_Servant /= null then
         return Skel.P_Servant;
      else
         raise PortableServer.POA.ObjectNotActive;
      end if;
   end Id_To_Servant;

   function Id_To_Reference (Self : Ref; Oid : ObjectId)
                             return CORBA.Object.Ref
   is
      POA : Broca.POA.POA_Object_Ptr;
      Skel : Broca.POA.Skeleton_Ptr;
      Res : CORBA.Object.Ref;
   begin
      POA := To_Poa (Self);
      if POA.Servant_Policy /= RETAIN then
         raise WrongPolicy;
      end if;
      Skel := Broca.POA.Id_To_Skeleton (POA, Oid);
      if Skel.P_Servant /= null then
         CORBA.Object.Set (Res, Broca.Refs.Ref_Ptr (Skel));
         return Res;
      else
         raise PortableServer.POA.ObjectNotActive;
      end if;
   end Id_To_Reference;

end Portableserver.POA;
