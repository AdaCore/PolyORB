package CORBA.Request is

   type Object is private;

   procedure Add_Arg
     (Self : in out Object;
      Arg  : in     NamedValue);

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags);

   procedure Delete
     (Self : in out Object);

   procedure Send
     (Self         : in out Object;
      Invoke_Flags : in     Flags);

   procedure Get_Response
     (Self           : in out Object;
      Response_Flags : in     Flags);

private
   -- implementation defined

end CORBA.Request;
