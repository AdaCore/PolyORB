package body Echo.My_Impl is

   function EchoString (Self : access Object; Mesg : in CORBA.String)
                        return CORBA.String is
   begin
      return Mesg;
   end EchoString;

end Echo.My_Impl;

