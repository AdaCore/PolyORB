with CORBA.Impl;

with PolyORB.Smart_Pointers;

package body PolyORB.CORBA_P.ServantLocator is

   ------------
   -- Create --
   ------------

   procedure Create
     (Self :    out PPT.ServantLocator_Access;
      SL   : access PortableServer.ServantLocator.Ref'Class) is
   begin
      Self := new CORBA_ServantLocator;

      CORBA_ServantLocator (Self.all).SL
        := PortableServer.ServantLocator.SL_Ptr (SL);
   end Create;

   -------------------------
   -- Get_Servant_Manager --
   -------------------------

   function Get_Servant_Manager
     (Self : CORBA_ServantLocator)
     return PortableServer.ServantLocator.Ref'Class is
   begin
      return Self.SL.all;
   end Get_Servant_Manager;

   ---------------
   -- Preinvoke --
   ---------------

   procedure Preinvoke
     (Self       : access CORBA_ServantLocator;
      Oid        : in     PPT.Object_Id;
      Adapter    : access PPT.Obj_Adapter'Class;
      Operation  : in     PolyORB.Types.Identifier;
      The_Cookie : out    PPT.Cookie;
      Returns    : out    PolyORB.Servants.Servant_Access)
   is
      CORBA_POA     : PortableServer.POA_Forward.Ref;
      CORBA_Servant : PortableServer.Servant;

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      PortableServer.ServantLocator.Preinvoke
        (PortableServer.ServantLocator.Ref'Class (Self.SL.all),
         PortableServer.ObjectId (Oid),
         CORBA_POA,
         CORBA.Identifier (Operation),
         PortableServer.ServantLocator.Cookie (The_Cookie),
         CORBA_Servant);

      Returns := PolyORB.Servants.Servant_Access
        (PortableServer.To_PolyORB_Servant (CORBA_Servant));

   end Preinvoke;

   ----------------
   -- Postinvoke --
   ----------------

   procedure Postinvoke
     (Self        : access CORBA_ServantLocator;
      Oid         : in     PPT.Object_Id;
      Adapter     : access PPT.Obj_Adapter'Class;
      Operation   : in     PolyORB.Types.Identifier;
      The_Cookie  : in     PPT.Cookie;
      The_Servant : in     PolyORB.Servants.Servant_Access)
   is
      CORBA_POA     : PortableServer.POA_Forward.Ref;

      CORBA_Servant : constant PortableServer.Servant :=
        PortableServer.Servant (CORBA.Impl.To_CORBA_Servant (The_Servant));

   begin
      PortableServer.POA_Forward.Set
        (CORBA_POA,
         PolyORB.Smart_Pointers.Entity_Ptr (Adapter));

      PortableServer.ServantLocator.Postinvoke
        (PortableServer.ServantLocator.Ref'Class (Self.SL.all),
         PortableServer.ObjectId (Oid),
         CORBA_POA,
         CORBA.Identifier (Operation),
         PortableServer.ServantLocator.Cookie (The_Cookie),
         CORBA_Servant);
   end Postinvoke;

end PolyORB.CORBA_P.ServantLocator;
