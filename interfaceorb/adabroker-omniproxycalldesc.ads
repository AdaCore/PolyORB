
--  This is a root class. For each subprogram of an IDL interface, which is
--  not declared "one way", a descendant of this class has to be provided.
--  It contains al the information to make the remote call : arguments,
--  results, exceptions, and how to send them on/ reveive them from a giop.
--  ( see proxyCall.h)

with Ada.Finalization;

with CORBA;

with AdaBroker; use AdaBroker;
with AdaBroker.GIOP_C;

package AdaBroker.OmniProxyCallDesc is

   type Object is
     abstract new Ada.Finalization.Limited_Controlled with private;
   --  Type of an omniProxyCallDesc object

   procedure Set_User_Exceptions
     (Self           : in out Object;
      Has_Exceptions : CORBA.Boolean);
   --  Set the boolean Pd_Has_User_Exception


   function Operation
     (Self : in Object)
      return CORBA.String is abstract;
   --  Returns the name of the subprogram

   function Align_Size
     (Self    : in Object;
      Size_In : in CORBA.Unsigned_Long)
      return CORBA.Unsigned_Long;
   --  This function computes the size needed to marshall the arguments of
   --  the subprogram

   procedure Marshal_Arguments
     (Self        : in Object;
      GIOP_Client : in out GIOP_C.Object);
   --  Marshals the arguments of the subprogram into a Giop_C object

   procedure Unmarshal_Returned_Values
     (Self        : in out Object;
      GIOP_Client : in out GIOP_C.Object);
   --  Unmarshalls the returned values of the subprogram from a Giop_C
   --  object

   procedure User_Exception
     (Self        : in Object;
      GIOP_Client : in out GIOP_C.Object;
      Repoid      : in CORBA.String);
   --  Must be overloaded by call descs which have exceptions

   function Has_User_Exceptions
     (Self : in Object)
      return CORBA.Boolean;
   --  Returns Pd_Has_User_Exception

private

   type Object is
     abstract new Ada.Finalization.Limited_Controlled with record
        Pd_Has_User_Exception : CORBA.Boolean;
     end record;
   --  Implementation of the private type Object

end AdaBroker.OmniProxyCallDesc;

