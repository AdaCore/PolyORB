----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA.Impl;
with CORBA.ORB.Typecode;

package body CORBA.Repository_Root.NativeDef.Impl is

   -----------------
   --  To_Object  --
   -----------------
   function To_Object (Fw_Ref : NativeDef_Forward.Ref)
                       return Object_Ptr is
   begin
      return NativeDef.Impl.Object_Ptr
        (NativeDef.Object_Of
         (NativeDef.Convert_Forward.To_Ref
          (Fw_Ref)));
   end To_Object;

   ------------------
   --  To_Forward  --
   ------------------
   function To_Forward (Obj : Object_Ptr)
                        return NativeDef_Forward.Ref is
      Ref : NativeDef.Ref;
   begin
      Set (Ref, CORBA.Impl.Object_Ptr (Obj));
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
      return  CORBA.ORB.Typecode.Create_Native_Tc (Get_Id (Self),
                                                   Get_Name (Self));
   end get_type;

end CORBA.Repository_Root.NativeDef.Impl;
