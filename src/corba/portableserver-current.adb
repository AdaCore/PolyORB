package body PortableServer.Current is

   -------------
   -- Get_POA --
   -------------

   function Get_POA
     (Self : Ref)
     return PortableServer.POA_Forward.Ref is
   begin
      pragma Warnings (Off);
      return Get_POA (Self);
      pragma Warnings (On);
   end Get_POA;

   -------------------
   -- Get_Object_Id --
   -------------------

   function Get_Object_Id
     (Self : Ref)
     return ObjectId is
   begin
      pragma Warnings (Off);
      return Get_Object_Id (Self);
      pragma Warnings (On);
   end Get_Object_Id;

   -----------------
   -- Get_Members --
   -----------------

   procedure Get_Members
     (From : in  Ada.Exceptions.Exception_Occurrence;
      To   : out NoContext_Members)
   is
      use Ada.Exceptions;

   begin
      if Exception_Identity (From) /= NoContext'Identity then
         CORBA.Raise_Bad_Param (CORBA.Default_Sys_Member);
      end if;

      To := NoContext_Members'
        (CORBA.IDL_Exception_Members with null record);
   end Get_Members;

   ---------------------
   -- Raise_NoContext --
   ---------------------

   procedure Raise_NoContext
     (Excp_Memb : in NoContext_Members)
   is
      pragma Warnings (Off); --  WAG:3.15
      pragma Unreferenced (Excp_Memb);
      pragma Warnings (On); --  WAG:3.15
   begin
      raise NoContext;
   end Raise_NoContext;

end PortableServer.Current;
