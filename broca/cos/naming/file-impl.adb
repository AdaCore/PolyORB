----------------------------------------------
--  This file has been generated automatically
--  by AdaBroker (http://adabroker.eu.org/)
----------------------------------------------

with CORBA;
with CORBA.ORB;

with PortableServer.POA;

with File.Skel;
with File.Helper;

package body File.Impl is

   type File_Ptr is access all Object'Class;

   Root_POA_String : constant CORBA.String
     := CORBA.To_CORBA_String ("RootPOA");

   Root_POA : constant PortableServer.POA.Ref
     := PortableServer.POA.To_Ref
          (CORBA.ORB.Resolve_Initial_References
           (CORBA.ORB.ObjectId (Root_POA_String)));

   function New_File
     return File.Ref
   is
      Obj : File_Ptr;
      Oid : PortableServer.ObjectId;

   begin
      Obj      := new Object;

      Oid := PortableServer.POA.Activate_Object
        (Root_POA, PortableServer.Servant (Obj));

      return File.Helper.To_Ref
        (PortableServer.POA.Servant_To_Reference
         (Root_POA, PortableServer.Servant (Obj)));
   end New_File;

   function get_Image
     (Self : access Object)
     return CORBA.String is
   begin
      return Self.Image;
   end get_Image;

   procedure set_Image
     (Self : access Object;
      To : in CORBA.String) is
   begin
      Self.Image := To;
   end set_Image;

end File.Impl;
