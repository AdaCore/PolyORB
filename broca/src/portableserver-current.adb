with Broca.Task_Attributes;

package body PortableServer.Current is

   procedure Get_Members
     (From : in  CORBA.Exception_Occurrence;
      To   : out NoContext_Members) is
   begin
      raise Program_Error;
   end Get_Members;

   function get_POA (Self : Ref)
     return PortableServer.POA_Forward.Ref is
   begin
      -- XXX should check validity and possibly raise NoContext;
      return Broca.Task_Attributes.Current_POA;
   end get_POA;

   function get_object_id (Self : Ref)
     return ObjectId is
   begin
      -- XXX should check validity and possibly raise NoContext;
      return Broca.Task_Attributes.Current_Object;
   end get_object_id;

end PortableServer.Current;
