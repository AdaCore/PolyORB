with Broca.Exceptions;
with Broca.Buffers; use Broca.Buffers;

with Broca.Debug;
pragma Elaborate_All (Broca.Debug);

package body Broca.Poa is
   Flag : constant Natural := Broca.Debug.Is_Active ("broca.poa");
   procedure O is new Broca.Debug.Output (Flag);

   function Get_The_POAManager (Self : access POA_Object)
                                return POAManager_Object_Access is
   begin
      return Self.POA_Manager;
   end Get_The_POAManager;


   procedure Compute_New_Size
     (Buffer : in out Broca.Buffers.Buffer_Descriptor;
      Value  : in Skeleton) is
   begin
      Compute_New_Size (Buffer, Value.IOR);
   end Compute_New_Size;

   procedure Marshall
     (Buffer : in out Broca.Buffers.Buffer_Descriptor;
      Value  : in Skeleton) is
   begin
      Append_Buffer (Buffer, Value.IOR);
   end Marshall;

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
