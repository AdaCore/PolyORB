with Broca.Refs;
with Broca.Exceptions;
with Broca.Poa;
with PortableServer.ServantActivator.Impl;
with PortableServer.ServantLocator.Impl;

package body Portableserver.Poa is
   function Create_Ref (Referenced : Broca.Refs.Ref_Acc) return Ref;

   function Create_Ref (Referenced : Broca.Refs.Ref_Acc) return Ref is
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
      if CORBA.Object.Get (Self).all not in Broca.Poa.POA_Object'Class then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      return Create_Ref (CORBA.Object.Get (Self));
   end To_Ref;

   function To_Poa (Self : Ref) return Broca.Poa.POA_Object_Access;

   function To_Poa (Self : Ref) return Broca.Poa.POA_Object_Access is
      use Broca.Poa;
      use Broca.Refs;
      Res_Ref : Broca.Refs.Ref_Acc;
      Res : Broca.Poa.POA_Object_Access;
   begin
      Res_Ref := Get (Self);
      if Res_Ref = null
        or else Res_Ref.all not in Broca.Poa.POA_Object'Class
      then
         Broca.Exceptions.Raise_Bad_Param;
      end if;
      Res := Broca.Poa.POA_Object_Access (Res_Ref);
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
      return Ref'(Create_Ref (Broca.Refs.Ref_Acc (To_Poa (Self).Parent)));
   end Get_The_Parent;

   function Get_The_POAManager (Self : Ref)
                                return PortableServer.POAManager.Ref is
      use Broca.Poa;
      use PortableServer.POAManager;
      Res : PortableServer.POAManager.Ref;
   begin
      Set (Res,
           Broca.Refs.Ref_Acc (Broca.Poa.Get_The_POAManager (To_Poa (Self))));
      return Res;
   end Get_The_POAManager;

   function Get_Servant_Manager (Self : Ref)
                                 return PortableServer.ServantManager.Ref
   is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);
      if Poa.Request_Policy /= USE_SERVANT_MANAGER then
         raise WrongPolicy;
      end if;
      return Poa.Servant_Manager;
   end Get_Servant_Manager;

   procedure Set_Servant_Manager
     (Self : in Ref; Imgr : in PortableServer.ServantManager.Ref)
   is
      Poa : Broca.Poa.POA_Object_Access;
      Skel : Broca.Poa.Internal_Skeleton_Access;
   begin
      Poa := To_Poa (Self);
      if Poa.Request_Policy /= USE_SERVANT_MANAGER then
         raise WrongPolicy;
      end if;
      Skel := Broca.Poa.To_Internal_Skeleton (Imgr);
      if Skel.P_Servant /= null then
         case Poa.Servant_Policy is
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
      Poa.Servant_Manager := Imgr;
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
      use Broca.Poa;
      Res : Broca.Poa.POA_Object_Access;
      Poa : Broca.Poa.POA_Object_Access;
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

      Poa := To_Poa (Self);
      begin
         All_POAs_Lock.Lock_W;
         Res := Broca.Poa.Create_POA
           (Poa,
            Adapter_Name,
            POAManager_Object_Access
            (PortableServer.POAManager.Get (A_POAManager)),
            Tp, Lp, Up, Ip, Ap, Sp, Rp);
         All_POAs_Lock.Unlock_W;
         return Create_Ref (Broca.Refs.Ref_Acc (Res));
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
      use Broca.Poa;
      Res : Broca.Poa.POA_Object_Access;
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);
      All_POAs_Lock.Lock_W;
      Res := Broca.Poa.Find_POA (Poa, Adapter_Name, Activate_It);
      All_POAs_Lock.Unlock_W;
      if Res = null then
         raise AdapterNonExistent;
      else
         return Create_Ref (Broca.Refs.Ref_Acc (Res));
      end if;
   end Find_POA;

   procedure Destroy
     (Self                : in Ref;
      Etherealize_Objects : in CORBA.Boolean;
      Wait_For_Completion : in CORBA.Boolean)
   is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);
      Broca.Poa.Destroy_POA (Poa, Etherealize_Objects, Wait_For_Completion);
      --  FIXME: Huh, SELF is still a reference to an invalid POA.
   end Destroy;

   function Get_Servant (Self : Ref) return Servant is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);
      if Poa.Request_Policy /= USE_DEFAULT_SERVANT then
         raise WrongPolicy;
      end if;
      if Poa.Default_Servant = null then
         raise NoServant;
      else
         return Poa.Default_Servant;
      end if;
   end Get_Servant;

   procedure Set_Servant (Self : in Ref; P_Servant : in Servant) is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);
      if Poa.Request_Policy /= USE_DEFAULT_SERVANT then
         raise WrongPolicy;
      end if;
      Poa.Default_Servant := P_Servant;
   end Set_Servant;

   function Activate_Object (Self : Ref; P_Servant : Servant)
                             return ObjectId is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);

      --  Cf 9-34: this operation requires SYSTEM_ID and RETAIN policies.
      if Poa.Id_Assign_Policy /= SYSTEM_ID
        or else Poa.Servant_Policy /= RETAIN
      then
         raise WrongPolicy;
      end if;

      return Broca.Poa.Activate_Object (Poa, P_Servant);
   end Activate_Object;

   procedure Activate_Object_With_Id
     (Self : in Ref; Oid : in ObjectId; P_Servant : in Servant) is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);

      --  Cf 9-34: this operation requires RETAIN policy.
      if Poa.Servant_Policy /= RETAIN then
         raise WrongPolicy;
      end if;

      Broca.Poa.Activate_Object_With_Id (Poa, Oid, P_Servant);
   end Activate_Object_With_Id;

   procedure Deactivate_Object (Self : in Ref; Oid : in ObjectId) is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);

      --  Cf 9-34: this operation requires RETAIN policy.
      if Poa.Servant_Policy /= RETAIN then
         raise WrongPolicy;
      end if;

      Broca.Poa.Deactivate_Object (Poa, Oid);
   end Deactivate_Object;

   function Create_Reference (Self : Ref; Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);
      if Poa.Id_Assign_Policy /= SYSTEM_ID then
         raise WrongPolicy;
      end if;
      return Broca.Poa.Create_Reference (Poa, Intf);
   end Create_Reference;

   function Create_Reference_With_Id
     (Self : Ref; Oid : ObjectId; Intf : CORBA.RepositoryId)
      return CORBA.Object.Ref
   is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);
      return Broca.Poa.Create_Reference_With_Id (Poa, Oid, Intf);
   end Create_Reference_With_Id;

   function Servant_To_Id (Self : Ref; P_Servant : Servant) return ObjectId
   is
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);
      if Poa.Servant_Policy /= RETAIN
        or else (Poa.Uniqueness_Policy /= UNIQUE_ID
                 and then Poa.Activation_Policy /= IMPLICIT_ACTIVATION)
      then
         raise WrongPolicy;
      end if;
      return Broca.Poa.Servant_To_Id (Poa, P_Servant);
   end Servant_To_Id;

   function Servant_To_Reference (Self : Ref; P_Servant : Servant)
                                  return CORBA.Object.Ref
   is
      Res : CORBA.Object.Ref;
      Poa : Broca.Poa.POA_Object_Access;
   begin
      Poa := To_Poa (Self);
      if Poa.Servant_Policy /= RETAIN
        or else (Poa.Uniqueness_Policy /= UNIQUE_ID
                 and then Poa.Activation_Policy /= IMPLICIT_ACTIVATION)
      then
         raise WrongPolicy;
      end if;
      CORBA.Object.Set
        (Res,
         Broca.Refs.Ref_Acc (Broca.Poa.Servant_To_Skeleton (Poa, P_Servant)));
      return Res;
   end Servant_To_Reference;

   function Reference_To_Id
     (Self : Ref; Reference : CORBA.Object.Ref'CLASS) return ObjectId
   is
      use Broca.Poa;
      Poa : Broca.Poa.POA_Object_Access;
      Skel : Broca.Poa.Skeleton_Access;
   begin
      Poa := To_Poa (Self);
      Skel := Broca.Poa.To_Skeleton (Reference);
      if Skel.Poa /= POA_Object_Access (Poa) then
         raise WrongAdapter;
      end if;
      return Skel.Object_Id;
   end Reference_To_Id;

   function Reference_To_Servant
     (Self : Ref; Reference : CORBA.Object.Ref'CLASS) return Servant
   is
      use Broca.Poa;
      Poa : Broca.Poa.POA_Object_Access;
      Skel : Broca.Poa.Skeleton_Access;
   begin
      Poa := To_Poa (Self);
      Skel := Broca.Poa.To_Skeleton (Reference);
      if Skel.Poa /= POA_Object_Access (Poa) then
         raise WrongAdapter;
      end if;
      if Poa.Servant_Policy /= RETAIN
        and then Poa.Request_Policy /= USE_DEFAULT_SERVANT
      then
         raise WrongPolicy;
      end if;
      return Broca.Poa.Skeleton_To_Servant (Poa, Skel);
   end Reference_To_Servant;

   function Id_To_Servant (Self : Ref; Oid : ObjectId) return Servant
   is
      Poa : Broca.Poa.POA_Object_Access;
      Skel : Broca.Poa.Skeleton_Access;
   begin
      Poa := To_Poa (Self);
      if Poa.Servant_Policy /= RETAIN then
         raise WrongPolicy;
      end if;
      Skel := Broca.Poa.Id_To_Skeleton (Poa, Oid);
      if Skel.P_Servant /= null then
         return Skel.P_Servant;
      else
         raise PortableServer.POA.ObjectNotActive;
      end if;
   end Id_To_Servant;

   function Id_To_Reference (Self : Ref; Oid : ObjectId)
                             return CORBA.Object.Ref
   is
      Poa : Broca.Poa.POA_Object_Access;
      Skel : Broca.Poa.Skeleton_Access;
      Res : CORBA.Object.Ref;
   begin
      Poa := To_Poa (Self);
      if Poa.Servant_Policy /= RETAIN then
         raise WrongPolicy;
      end if;
      Skel := Broca.Poa.Id_To_Skeleton (Poa, Oid);
      if Skel.P_Servant /= null then
         CORBA.Object.Set (Res, Broca.Refs.Ref_Acc (Skel));
         return Res;
      else
         raise PortableServer.POA.ObjectNotActive;
      end if;
   end Id_To_Reference;

end Portableserver.Poa;
