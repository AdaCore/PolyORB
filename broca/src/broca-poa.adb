with Broca.Exceptions;

package body Broca.Poa is
   function Get_The_POAManager (Self : access POA_Object)
                                return POAManager_Object_Access is
   begin
      return Self.POA_Manager;
   end Get_The_POAManager;

   function Object_To_IOR (Obj : Skeleton)
                           return Broca.Types.Buffer_Descriptor is
   begin
      return Obj.Ior;
   end Object_To_IOR;

   function To_Skeleton (Ref : CORBA.Object.Ref'Class)
                         return Skeleton_Access
   is
      use Broca.Refs;
      Res : Broca.Refs.Ref_Acc;
   begin
      Res := CORBA.Object.Get (Ref);
      if Res = null or else Res.all not in Skeleton'Class then
         Broca.Exceptions.Raise_Bad_Param;
      else
         return Skeleton_Access (Res);
      end if;
   end To_Skeleton;

   --  Can raise bad_param.
   function To_Internal_Skeleton (Ref : CORBA.Object.Ref'Class)
                                  return Internal_Skeleton_Access
   is
      use Broca.Refs;
      Res : Broca.Refs.Ref_Acc;
   begin
      Res := CORBA.Object.Get (Ref);
      if Res = null or else Res.all not in Internal_Skeleton'Class then
         Broca.Exceptions.Raise_Bad_Param;
      else
         return Internal_Skeleton_Access (Res);
      end if;
   end To_Internal_Skeleton;

   function Create_Internal_Skeleton (P_Servant : PortableServer.Servant)
                                      return Internal_Skeleton_Access
   is
      Res : Internal_Skeleton_Access;
   begin
      Res := new Internal_Skeleton;
      Res.P_Servant := P_Servant;
      Broca.Refs.Inc_Usage (Broca.Refs.Ref_Acc (Res));
      return Res;
   end Create_Internal_Skeleton;
end Broca.Poa;
