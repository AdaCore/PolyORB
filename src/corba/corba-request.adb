--  The CORBA Dynamic Invocation Interface.

--  $Id$

with PolyORB.Requests;

with CORBA.Context;
with CORBA.NVList;
with CORBA.Object;

package body CORBA.Request is

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Request   :    out Object;
      Req_Flags : in     Flags) is
   begin
      PolyORB.Requests.Create_Request
        (Target    => CORBA.Object.To_PolyORB_Ref
         (CORBA.Object.Ref (CORBA.AbstractBase.Ref'Class (Self))),
         Operation => To_Standard_String (Operation),
         Arg_List  => CORBA.NVList.To_PolyORB_Ref (Arg_List),
         Result    => Result,
         Req       => Request.The_Request);
      --  XXX Some arguments are not taken into account!
   end Create_Request;

   procedure Create_Request
     (Self      : in     CORBA.AbstractBase.Ref;
      Ctx       : in     CORBA.Context.Ref;
      Operation : in     Identifier;
      Arg_List  : in     CORBA.NVList.Ref;
      Result    : in out NamedValue;
      Exc_List  : in     ExceptionList.Ref;
      Ctxt_List : in     ContextList.Ref;
      Request   :    out CORBA.Request.Object;
      Req_Flags : in     Flags) is
   begin
      raise PolyORB.Not_Implemented;
   end Create_Request;

   procedure Invoke
     (Self         : in out Object;
      Invoke_Flags : in     Flags  := 0) is
   begin
      PolyORB.Requests.Invoke (Self.The_Request);
      --  XXX Some arguments are not taken into account!
   end Invoke;

   procedure Delete (Self : in out Object) is
   begin
      PolyORB.Requests.Destroy_Request (Self.The_Request);
   end Delete;

   function To_PolyORB_Request
     (Request : Object)
     return PolyORB.Requests.Request_Access is
   begin
      return Request.The_Request;
   end To_PolyORB_Request;

   function To_CORBA_Request
     (Request : PolyORB.Requests.Request_Access)
     return Object is
   begin
      return Object'(The_Request => Request);
   end To_CORBA_Request;

end CORBA.Request;

