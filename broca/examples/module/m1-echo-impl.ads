with M1.Echo.Skel;
with CORBA;

package M1.Echo.Impl is
   --  My own implementation of echo object.
   --  This is simply used to define the operations.

   type Object is new Echo.Skel.Object with null record;

   type Object_Acc is access Object;

private
   function EchoMy_String (Self : access Object; Mesg : in M1.My_String)
                        return CORBA.String;

end M1.Echo.Impl;
