----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.ORB;

with PolyORB.CORBA_P.Server_Tools;
with PortableServer;

with CORBA.Repository_Root.NativeDef.Skel;
pragma Warnings (Off, CORBA.Repository_Root.NativeDef.Skel);

package body CORBA.Repository_Root.NativeDef.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : NativeDef_Forward.Ref)
                       return Object_Ptr is
      Result : PortableServer.Servant;
   begin
      PolyORB.CORBA_P.Server_Tools.Reference_To_Servant
        (NativeDef.Convert_Forward.To_Ref (Fw_Ref),
         Result);
      return NativeDef.Impl.Object_Ptr (Result);
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return NativeDef_Forward.Ref is
      Ref : NativeDef.Ref;
   begin
      PolyORB.CORBA_P.Server_Tools.Initiate_Servant
        (PortableServer.Servant (Obj), Ref);
      return NativeDef.Convert_Forward.To_Forward (Ref);
   end To_Forward;

   ----------------
   --  get_type  --
   ----------------
   function get_type
     (Self : access Object)
      return CORBA.TypeCode.Object
   is
   begin
      return CORBA.ORB.Create_Native_Tc
        (get_id (Self), get_name (Self));
   end get_type;

end CORBA.Repository_Root.NativeDef.Impl;
