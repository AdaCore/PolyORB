--  This package provides glue codee between PolyORB's
--  ServantLocator and CORBA specific ServantLocator.

with PortableServer.ServantLocator;

with PolyORB.POA_Types;
with PolyORB.Servants;
with PolyORB.Types;

package PolyORB.CORBA_P.ServantLocator is

   package PPT renames PolyORB.POA_Types;

   type CORBA_ServantLocator is new PPT.ServantLocator with private;

   procedure Create
     (Self :    out PPT.ServantLocator_Access;
      SL   : access PortableServer.ServantLocator.Ref'Class);

   function Get_Servant_Manager
     (Self : CORBA_ServantLocator)
     return PortableServer.ServantLocator.Ref'Class;

   procedure Preinvoke
     (Self       : access CORBA_ServantLocator;
      Oid        : in     PPT.Object_Id;
      Adapter    : access PPT.Obj_Adapter'Class;
      Operation  : in     PolyORB.Types.Identifier;
      The_Cookie : out    PPT.Cookie;
      Returns    : out    PolyORB.Servants.Servant_Access);

   procedure Postinvoke
     (Self        : access CORBA_ServantLocator;
      Oid         : in     PPT.Object_Id;
      Adapter     : access PPT.Obj_Adapter'Class;
      Operation   : in     PolyORB.Types.Identifier;
      The_Cookie  : in     PPT.Cookie;
      The_Servant : in     PolyORB.Servants.Servant_Access);

private

   type CORBA_ServantLocator is new PPT.ServantLocator with record
      SL : PortableServer.ServantLocator.SL_Ptr;
   end record;

end PolyORB.CORBA_P.ServantLocator;
