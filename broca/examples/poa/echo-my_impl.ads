with Echo.Impl;
with CORBA;

package Echo.My_Impl is
   --  My own implementation of echo object.
   --  This is simply used to define the operations.

   type Object is new Echo.Impl.Object with record
      Delay_Time : Duration := 0.0;
   end record;

   type Object_Acc is access Object;

private
   function EchoString (Self : access Object; Mesg : in CORBA.String)
                        return CORBA.String;

end Echo.My_Impl;
