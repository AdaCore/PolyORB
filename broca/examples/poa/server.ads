with CORBA.Object;
with PortableServer.POA;
with PortableServer.ServantManager;
with Echo.My_Impl;
pragma Elaborate_All (PortableServer.ServantManager);

package Server is
   type String_Acc is access String;

   --  Flags
   Flag_Verbose : Boolean := False;
   Flag_Discard : Boolean := False;
   Flag_Delay : Boolean := False;
   Flag_Tilt : Boolean := False;
   Flag_Servant_Policy : PortableServer.RequestProcessingPolicyValue :=
     PortableServer.USE_ACTIVE_OBJECT_MAP_ONLY;
   Flag_Ior_File : String_Acc := null;

   --  If set, there is a wall to echo your string.
   Flag_Wall : Boolean := True;


   --  If True, the POA managing the echo servant has single_thread policy.
   Flag_Serial : Boolean := False;

   Bad_Option : exception;

   My_Echo : Echo.My_Impl.Object_Acc;

   My_Servant_Manager : PortableServer.ServantManager.Ref;

   Ref_To_Export : CORBA.Object.Ref;

   procedure Decode_Options;

   function Build_Poa_Tree (Poa : PortableServer.POA.Ref)
     return PortableServer.POA.Ref;
end Server;
