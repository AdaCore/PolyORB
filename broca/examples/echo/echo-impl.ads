with Echo.Skel;
with CORBA;

package Echo.Impl is
   --  My own implementation of echo object.
   --  This is simply used to define the operations.

   type Object is new Echo.Skel.Object with null record;

   type Object_Acc is access Object;

private
   function EchoString (Self : access Object; Mesg : in CORBA.String)
                        return CORBA.String;

end Echo.Impl;
