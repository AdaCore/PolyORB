--  $Id$

package body CORBA.AbstractBase is

   ---------------
   -- Object_Of --
   ---------------

   function Object_Of
     (The_Ref : Ref)
     return CORBA.Impl.Object_Ptr is
   begin
      return CORBA.Impl.Object_Ptr (Entity_Of (The_Ref));
   end Object_Of;

end CORBA.AbstractBase;
