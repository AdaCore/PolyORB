
--  This is a root class. For each subprogram of an IDL interface which is
--  declared "one way", a descendant of this class has to be provided.  It
--  contains all the information to make the remote call : arguments,
--  results, exceptions, and how to send them on/ reveive them from a giop.
--  ( see proxyCall.h)

with CORBA;
with AdaBroker.GIOP_C;

package AdaBroker.OmniOWProxyCallDesc is

   type Object is abstract tagged limited private;
   --  Type of an omniowProxyCallDesc object

   type Object_Ptr is access all Object;
   --  Type pointer on type Object

   function Aligned_Size
     (Self    : in Object;
      Size_In : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long is abstract;
   --  This function computes the size needed to marshall the arguments of
   --  the subprogram

   procedure Marshal_Arguments
     (Self        : in Object;
      Giop_Client : in out AdaBroker.GIOP_C.Object) is abstract;
   --  Marshalls the arguments of the subprogram into a Giop_C object

   function Operation
     (Self : in Object)
      return CORBA.String is abstract;
   --  Returns the name of the subprogram

private

   type Object is abstract tagged limited null record;
   --  Implementation of the private type Object

end AdaBroker.OmniOWProxyCallDesc;

